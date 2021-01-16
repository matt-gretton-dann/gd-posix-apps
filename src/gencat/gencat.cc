/** \file   gencat.cc
 *  \brief  Main program for gencat
 *  \author Copyright 2020, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/bit.hh"
#include "gd/fcntl.h"
#include "gd/filesystem.hh"
#include "gd/format.hh"
#include "gd/libgen.h"
#include "gd/limits.h"
#include "gd/nl_types.h"
#include "gd/stdlib.h"
#include "gd/sys/stat.h"
#include "gd/unistd.h"

#include <cassert>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

namespace {
std::string_view program_name;  ///< Program name - somewhere global for all.

/** \brief         Wrapper around write
 *  \param  fd     File descriptor to write to
 *  \param  data   Pointer to data to write
 *  \param  amount number of bytes to write.
 *  \return        Amount of data written.
 *  \throws        std::system_error() on an error.
 *
 * This wrapper on ::write handles redoing work when receiving `EINTR`, and also
 * writing files that are larger SSIZE_MAX.
 *
 * On an error it throws a std::system_error exception.  Note that in this case
 * the output file may already have some data in it.
 */
int xwrite(int fd, char const* data, std::uint64_t amount)
{
  char const* end = data + amount;
  while (data < end) {
    // Jump through some hoops to ensure we write at most SSIZE_MAX bytes of
    // data, and certainly no more than requested.
    std::uint64_t to_do = end - data;
    std::size_t to_write =
      static_cast<std::size_t>(std::min(static_cast<std::uint64_t>(SSIZE_MAX), to_do));
    ssize_t written = ::write(fd, data, to_write);
    if (written == -1) {
      // EINTR means 'try again'.
      if (errno != EINTR) {
        throw std::system_error(std::make_error_code(static_cast<std::errc>(errno)));
      }
    }
    else {
      data += written;
    }
  }
  return amount;
}

/** \brief A basic location recorder.
 *
 * Records a filename and location (which may be line number, offset, or some
 * other mechanism of recoridng the location in a file).
 *
 * Can be used to throw error messages, or get warning messages.
 */
class Location
{
public:
  /** \brief          Constructor
   *  \param filename File name
   *  \param loc      Initial location (defaults to 0).
   */
  explicit Location(std::string filename, std::uint64_t loc = 0) noexcept
      : filename_(std::move(filename)), loc_(loc)
  {
  }

  /** \brief          Constructor
   *  \param filename File name
   *  \param loc      Initial location (defaults to 0).
   */
  explicit Location(std::string_view filename, std::uint64_t loc = 0)
      : filename_(filename), loc_(loc)
  {
  }

  /** \brief  Get the filename. */
  std::string const& filename() const noexcept { return filename_; }

  /** \brief  Get the current location. */
  std::uint64_t loc() const noexcept { return loc_; }

  /** \brief  Set the location. */
  void loc(std::uint64_t l) noexcept { loc_ = l; }

  /** \brief  Increase the location value. */
  void inc_loc(std::uint64_t inc) noexcept
  {
    assert(loc_ < SIZE_MAX - inc);
    loc_ += inc;
  }

  /** \brief      Throw a runtime_error
   *  \param f    Format for error message
   *  \param args Args to apply to error message.
   *
   * Throws a std::runtime_error with the what() being a prefix giving the
   * location followed by the error message specified, and formatted through
   * fmt::format.
   */
  template<typename... Ts>
  [[noreturn]] void error(std::string const& f, Ts... args) const
  {
    std::string result = ::fmt::format("{}:{}:ERROR: ", filename_, loc_);
    result += ::fmt::format(f, args...);
    throw std::runtime_error(result);
  }

  /** \brief       Get a warning message
   *  \param  f    Format for warning message
   *  \param  args Args to apply to warning message.
   *  \return      String with warning message.
   *
   * Returns a string consisting of a prefix giving the
   * location followed by the error message specified, and formatted through
   * fmt::format.
   */
  template<typename... Ts>
  std::string warning(std::string const& f, Ts... args) const
  {
    std::string result = ::fmt::format("{}:{}:WARNING: ", filename_, loc_);
    result += ::fmt::format(f, args...);
    return result;
  }

private:
  std::string filename_;  ///< File name
  std::uint64_t loc_;     ///< Current location
};

/** \brief       Read a little-endian integer from a data block.
 *  \tparam T    Integer type to read.
 *  \param  data Offset of data to read.
 *  \return      Value
 *
 * Caller is responsible for ensuring \a data points to valid memory.
 *
 * If the system being compiled for is big-endian, or the data is not
 * naturally aligned a slow byte-by-byte read is done.  Otherwise we just do a
 * type conversion.
 */
template<typename T, std::enable_if_t<std::is_integral<T>::value, bool> = true>
T read(std::uint8_t const* data)
{
  // Fast path - no endian conversion, and data is appropriately aligned.
  if constexpr (::bit::endian::native == ::bit::endian::little) {
    if (((uintptr_t)data) % sizeof(T) == 0) {
      return *(reinterpret_cast<T const*>(data));
    }
  }

  // Slow path - read data byte by byte.
  T result = 0;
  for (unsigned i = 0; i < sizeof(T); ++i) {
    result |= static_cast<T>(data[i]) << (i * 8);
  }
  return result;
}

template<typename T, std::enable_if_t<std::is_integral<T>::value, bool> = true>
void write(std::uint8_t* data, T value)
{
  // Fast path - no endian conversion, and data is appropriately aligned.
  if constexpr (bit::endian::native == bit::endian::little) {
    if (((uintptr_t)data) % sizeof(T) == 0) {
      *(reinterpret_cast<T*>(data)) = value;
    }
  }

  // Slow path - write data byte by byte.
  for (unsigned i = 0; i < sizeof(T); ++i) {
    data[i] = std::uint8_t(value & 0xff);
    value >>= 8;
  }
}

/** \brief  A message catalogue.
 *
 * This class represents the majority of the working logic of the code.  It
 * handles loading binary catalogue files, and textual message files merging
 * them together, and then writing out the combined result at the end.
 */
class MessageCatalogue
{
public:
  /** \brief      Load a catalogue file.
   *  \param file File to load from.
   */
  void load_catfile(std::string_view const& file)
  {
    Location loc(file, 0);
    Data data;

    std::ifstream is(file.data(), std::ios::binary | std::ios::in);
    if (!is) {
      loc.error("Unable to open {} for reading.", file);
    }

    // Read the header
    load_header(is, loc, data);
    std::uint32_t set_count = read<std::uint32_t>(data.data() + set_count_off);
    std::uint64_t file_size = read<std::uint64_t>(data.data() + file_size_off);
    data.reserve(file_size);

    // Read the rest of the file.
    load_data(is, loc, data, file_size);
    std::uint64_t set_offset = hdr_size;
    auto raw_data = data.data();

    // Process each set.
    for (std::uint32_t set = 0; set < set_count; ++set) {
      TableEntry set_te = read_table_entry(loc, data, set_offset);
      set_offset += table_entry_size;

      auto [set_it, success] = sets_.insert({set_te.id, MessageSet()});
      if (!success) {
        loc.error("Message catalogue contains multiple definitions of set {}", set_te.id);
      }

      if (set_te.offset >= file_size) {
        loc.loc(set_te.offset);
        loc.error("Message array offset outside bounds of message catalogue.");
      }

      // Process each message within each set.
      for (std::uint32_t msg = 0; msg < set_te.len; ++msg) {
        TableEntry msg_te = read_table_entry(loc, data, set_te.offset);
        set_te.offset += table_entry_size;

        auto msg_str = read_string(loc, data, set_te.id, msg_te);

        auto [msg_it, success] = set_it->second.insert({msg_te.id, msg_str});
        if (!success) {
          loc.error("Multiple definitions of message with ID: {}.{}.", set_te.id, msg_te.id);
        }
      }
    }
  }

  /** \brief      Handle loading a message file.
   *  \param file File name to load
   */
  void load_msgfile(std::string_view const& file)
  {
    Location loc(file, 0);
    std::ifstream ifs;
    if (file != "-") {
      ifs.open(file.data());
      if (!ifs) {
        loc.error("Unable to open {} for reading.", file);
      }
    }
    std::istream& is = (file == "-") ? std::cin : ifs;

    std::string line;

    auto set_it = sets_.insert({NL_SETD, MessageSet()}).first;
    int quote = -1;
    while (std::getline(ifs, line)) {
      loc.inc_loc(1);

      // Handle line continuations
      while (line.size() != 0 && line[line.size() - 1] == '\\') {
        std::string line2;
        loc.inc_loc(1);
        if (!std::getline(ifs, line2)) {
          loc.error("Unexpected end of file indicator.");
        }
        line = line.substr(0, line.size() - 1) + line2;
      }

      // Interpret the line.
      if (line.substr(0, 5) == "$set ") {
        auto r = std::stoul(line.substr(5));
        validate_id(loc, r, "NL_SETMAX", NL_SETMAX);
        set_it = sets_.insert({r, MessageSet()}).first;
      }
      else if (line.substr(0, 8) == "$delset ") {
        auto r = std::stoul(line.substr(8));
        validate_id(loc, r, "NL_TEXTMAX", NL_TEXTMAX);
        auto it = sets_.find(r);
        if (it != sets_.end()) {
          // We only clear (and not erase) just in case $set is set to the same
          // value that we've just erased.
          it->second.clear();
        }
      }
      else if (line.substr(0, 2) == "$ " || line.empty()) {
        /* Do nothing: Comment.  */
      }
      else if (line.substr(0, 7) == "$quote ") {
        if (line.size() > 8) {
          loc.error("Quote should be a single character, or empty to clear.");
        }
        else if (line.size() == 7) {
          quote = -1;
        }
        else {
          quote = line[7];
        }
      }
      else if (line == "$quote") {
        quote = -1;
      }
      else if (std::isdigit(line[0])) {
        std::size_t pos = 0;
        auto r = std::stoul(line, &pos);
        validate_id(loc, r, "NL_MSGMAX", NL_MSGMAX);

        if (line.size() == pos) {
          set_it->second.erase(r);
        }
        else if (line[pos] != ' ') {
          loc.error("Message number should be followed by a space.");
        }
        else {
          std::string value(line.substr(pos + 1));
          if (!value.empty() && quote != -1 && value[0] == (char)(quote & 0xff)) {
            if (value[value.size() - 1] != (char)(quote & 0xff)) {
              std::cerr << loc.warning("String starts with a quote but does not end with one.");
            }
            else {
              value = value.substr(1, value.size() - 2);
            }
          }
          auto [it, success] = set_it->second.insert({r, std::string(value)});
          if (!success) {
            std::cerr << loc.warning("Replacing existing message ID {}.{}", set_it->first, r)
                      << "\n";
            it->second = std::string(value);
          }
        }
      }
      else {
        loc.error("Unrecognised line contents.");
      }
    }

    // Delete any empty sets.  When we handle deleting sets above we merely
    // clear the message map We don't delete any actual messages.
    auto it = sets_.begin();
    while (it != sets_.end()) {
      if (it->second.empty()) {
        it = sets_.erase(it);
      }
      else {
        ++it;
      }
    }
  }

  /** \brief    Save out a cat file to the indicated file descriptor.
   *  \param fd File descriptor.
   */
  void save_catfile(std::string_view const& filename, int fd) const
  {
    Data data{'M', 'S', 'G', '\0', 1, 0, 0, 0, 0, 0, 0, 0};
    Location loc(filename);
    data.resize(hdr_size);
    write(data.data() + set_count_off, std::uint32_t(sets_.size()));
    // Can't write file size yet - as we don't know it.

    data.resize(hdr_size + sets_.size() * table_entry_size);
    std::uint64_t set_offset = hdr_size;
    for (auto const& kv : sets_) {
      // Msg Array needs to be aligned to 8 byte boundary - and goes at current end of file.
      std::uint64_t msg_array_offset = (data.size() + 7) & ~Data::size_type(7);
      data.resize(msg_array_offset + kv.second.size() * 16);

      // Write set entry pointing to message array.
      write_table_entry(
        loc, data, set_offset,
        TableEntry{kv.first, static_cast<std::uint32_t>(kv.second.size()), msg_array_offset});
      set_offset += table_entry_size;

      // Write message array entries.
      for (auto const& kv2 : kv.second) {
        std::uint64_t msg_offset = data.size();
        write_table_entry(
          loc, data, msg_array_offset,
          TableEntry{kv2.first, static_cast<std::uint32_t>(kv2.second.size()) + 1, msg_offset});
        msg_array_offset += table_entry_size;
        for (auto c : kv2.second) {
          data.push_back(std::uint8_t(c));
        }
        // Null terminate the string.
        data.push_back(0);
      }
    }

    // Now we can write the file size.
    write(data.data() + file_size_off, std::uint64_t(data.size()));

    // Now write data to the file.
    xwrite(fd, reinterpret_cast<char const*>(data.data()), data.size());
  }

private:
  using Id = std::uint32_t;                      ///< Id integer type
  using Length = std::uint32_t;                  ///< Length integer type
  using Offset = std::uint64_t;                  ///< Offset type
  using MessageSet = std::map<Id, std::string>;  ///< Map of Message IDs to strings
  using SetMap = std::map<Id, MessageSet>;       ///< Map of Set Ids to Messages
  using Data = std::vector<std::uint8_t>;        ///< Type for storing arrays of bytes

  /** \brief  Simple structure to represent a table entry.
   *
   * Because of the design of the catfile format the tables all have similar
   * layouts: Entry ID, a length (which may be bytes or number of entries in
   * subtable) and offset to the entry's data.
   */
  struct TableEntry
  {
    Id id;
    Length len;
    Offset offset;
  };

  /** \brief      Load the initial header into a data structure.
   *  \param is   Input stream to ready from
   *  \param loc  Location object to use.
   *  \param data Data location to store header in.
   *
   * Will validate the header.
   */
  static void load_header(std::istream& is, Location& loc, Data& data)
  {
    char buf[hdr_size];
    if (!is.read(buf, hdr_size)) {
      throw std::runtime_error("Message catalogue is too short to be valid.");
    }
    data.insert(data.end(), buf, buf + is.gcount());

    if (data[0] != 'M' || data[1] != 'S' || data[2] != 'G' || data[3] != '\0') {
      loc.error("File does not start with message catalogue magic.");
    }
    if (data[4] != 1) {
      loc.error("File is not a version 1 message catalogue.");
    }
    for (auto i = 5; i < 12; ++i) {
      if (data[i] != 0) {
        loc.loc(i);
        loc.error("Reserved field is not 0.");
      }
    }
  }

  /** \brief      Load the rest of a catfile into a data block
   *  \param is   Input stream to ready from
   *  \param loc  Location object to use.
   *  \param data Data location to store header in.
   *  \param file_size Expected file size (including already loaded header).
   *
   * Will validate the size of the file.
   */
  static void load_data(std::istream& is, Location& loc, Data& data, Offset file_size)
  {
    constexpr std::size_t bufsize = 4096;
    char buf[bufsize];
    while (is.read(buf, bufsize)) {
      data.insert(data.end(), buf, buf + is.gcount());
      if (data.size() > file_size) {
        loc.loc(file_size);
        loc.error("Message catalogue is larger than it claims.");
      }
    }
    data.insert(data.end(), buf, buf + is.gcount());
    if (data.size() > file_size) {
      loc.loc(file_size);
      loc.error("Message Catalogue is larger than it claims.");
    }
    if (data.size() < file_size) {
      loc.loc(data.size());
      loc.error("Message catalogue is not as large as it claims.");
    }
  }

  /** \brief         Read a table entry
   *  \param  loc    Location object
   *  \param  data   Data to be read.
   *  \param  offset Offset of table entry within data.
   *  \return        A table entry.
   */
  TableEntry read_table_entry(Location& loc, Data const& data, Offset offset) const
  {
    if (offset > data.size() - table_entry_size) {
      loc.loc(offset);
      loc.error("Table is outside bounds of message catalogue.");
    }
    if (offset % 8 != 0) {
      loc.loc(offset);
      loc.error("Table entry is not aligned to 8-byte boundary.");
    }
    auto raw_data = data.data() + offset;
    Id id = read<std::uint32_t>(raw_data + table_entry_id_off);
    Length len = read<std::uint32_t>(raw_data + table_entry_len_off);
    Offset off = read<std::uint64_t>(raw_data + table_entry_off_off);
    return TableEntry{id, len, off};
  }

  /** \brief        Write a table entry
   *  \param data   Data to write into
   *  \param offset Offset to write at
   *  \param te     Table entry to write
   */
  void write_table_entry(Location& loc, Data& data, Offset offset, TableEntry const& te) const
  {
    if (offset % 8 != 0) {
      loc.loc(offset);
      loc.error("Table entry is not aligned ot 8-byte boundary.");
    }
    write(data.data() + offset + table_entry_id_off, te.id);
    write(data.data() + offset + table_entry_len_off, te.len);
    write(data.data() + offset + table_entry_off_off, te.offset);
  }

  /** \brief         Read a string
   *  \param  loc    Location object
   *  \param  data   Data to read from.
   *  \param  set_id ID of set string belongs to.
   *  \param  te     Message table entry giving location of string to read.
   *  \return        Read string.
   */
  std::string read_string(Location& loc, Data const& data, Id set_id, TableEntry const& te) const
  {
    loc.loc(te.offset);
    if (te.offset >= data.size()) {
      loc.error("Message {}.{} starts beyond the end of the message catalogue.", set_id, te.id);
    }
    if (te.len > data.size()) {
      loc.error("Message {}.{} is longer than the length of the message "
                "catalogue.",
                set_id, te.id);
    }
    if (te.offset > data.size() - te.len) {
      loc.error("Message {}.{} overflows the end of the message catalogue.", set_id, te.id);
    }
    if (data.data()[te.offset + te.len - 1] != '\0') {
      loc.error("Message {}.{} is not terminated by a NUL character.", set_id, te.id);
    }

    return std::string(reinterpret_cast<char const*>(data.data() + te.offset), te.len - 1);
  }

  static void validate_id(Location& loc, Id id, const char* soft_limit_name, Id soft_limit)
  {
    if (id > std::numeric_limits<Id>::max()) {
      loc.error("Id {} is too large.", id);
    }
    else if (id > soft_limit) {
      std::cerr << loc.warning("Id {} is larger than {} ({}).", id, soft_limit_name, soft_limit)
                << '\n';
    }
    else if (id < 1) {
      loc.error("Id {} is too small, must be at least 1.", id);
    }
  }

  static constexpr Offset hdr_size = 24;            ///< Size of the header.
  static constexpr Offset set_count_off = 12;       ///< Offset in header of count of sets
  static constexpr Offset file_size_off = 16;       ///< Offset in header of file size
  static constexpr Offset table_entry_size = 16;    ///< Size of a table entry
  static constexpr Offset table_entry_id_off = 0;   ///< Table entry ID offset
  static constexpr Offset table_entry_len_off = 4;  ///< Table entry length offset
  static constexpr Offset table_entry_off_off = 8;  ///< Table entry offset offset

  SetMap sets_;  ///< Set ID -> Messages map.
};
}  // namespace

int main(int argc, char** argv)
try {
  program_name = ::basename(argv[0]);

  // Skip '--' if present as the first argument.
  if (argc > 1 && argv[1][0] == '-' && argv[1][1] == '-' && argv[1][2] == '\0') {
    ++argv;
    --argc;
  }

  if (argc < 3) {
    std::cerr << fmt::format("{}: Need to specify a message catalogue and at "
                             "least one message file.",
                             program_name);
    std::exit(EXIT_FAILURE);
  }

  std::string_view catfile(argv[1]);
  MessageCatalogue cat;
  bool updating = catfile != "-" && fs::exists(catfile);
  if (updating) {
    cat.load_catfile(catfile);
  }

  for (auto msgfile = argv + 2; *msgfile != nullptr; ++msgfile) {
    cat.load_msgfile(*msgfile);
  }

  bool failed = false;
  bool remove_outfile = false;
  std::string outfile(catfile);
  int fd = -1;

  try {
    if (updating) {
      outfile += ".XXXXXX";
      remove_outfile = true;
      fd = ::mkstemp(outfile.data());
    }
    else if (outfile == "-") {
      fd = STDOUT_FILENO;
    }
    else {
      fd = ::open(outfile.c_str(), O_CREAT | O_WRONLY | O_TRUNC,
                  S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    }

    if (fd == -1) {
      throw std::system_error(std::make_error_code(static_cast<std::errc>(errno)));
    }

    cat.save_catfile(outfile, fd);
  }
  catch (std::exception& e) {
    std::cerr << e.what() << "\n";
    failed = true;
    remove_outfile = (fd != -1 && fd != STDOUT_FILENO);
  }

  if (fd != -1 && fd != STDOUT_FILENO) {
    close(fd);
  }

  if (updating) {
    if (unlink(catfile.data()) == -1) {
      std::cerr << fmt::format("{}: Unable to remove file: {}\n", program_name, catfile);
      failed = true;
    }
    else if (rename(outfile.data(), catfile.data()) == -1) {
      std::cerr << fmt::format("{}: Unable to remame file {} to {}\n", program_name, outfile,
                               catfile);
      std::cerr << fmt::format("{}: Leaving temporary file in place.\n", program_name);
      remove_outfile = false;
      failed = true;
    }
  }

  if (remove_outfile) {
    unlink(outfile.c_str());
    // Silently ignore any errors in the above.
  }

  return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}
catch (std::exception const& e) {
  std::cerr << fmt::format("{}: {}\n", program_name, e.what());
  std::exit(EXIT_FAILURE);
}
catch (...) {
  std::cerr << fmt::format("Unrecognised exception");
}
