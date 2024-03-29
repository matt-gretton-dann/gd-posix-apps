/** \file   gencat.cc
 *  \brief  Main program for gencat
 *  \author Copyright 2020, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"
#include "gd/nl_types.h"

#include "gd/bit.hh"
#include "gd/fcntl.h"
#include "gd/filesystem.hh"
#include "gd/format.hh"
#include "gd/limits.h"
#include "gd/span.hh"
#include "gd/stdlib.h"
#include "gd/sys/stat.h"
#include "gd/unistd.h"

#include "gencat-messages.hh"

#include <cassert>
#include <clocale>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

namespace Gencat = GD::Gencat;

namespace {
// NOLINTNEXTLINE
std::string program_name;  ///< Program name - somewhere global for all.

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
template<typename T, std::size_t E>
void xwrite(int fd, GD::Span::span<T, E> data)
{
  auto d = GD::Span::as_bytes(data);
  std::size_t amount_done = 0;
  while (amount_done < d.size()) {
    // Jump through some hoops to ensure we write at most SSIZE_MAX bytes of
    // data, and certainly no more than requested.
    std::uint64_t const to_do = d.size() - amount_done;
    std::size_t const to_write =
      static_cast<std::size_t>(std::min(static_cast<std::uint64_t>(SSIZE_MAX), to_do));
    ssize_t const written = ::write(fd, data.data() + amount_done, to_write);
    if (written == -1) {
      // EINTR means 'try again'.
      if (errno != EINTR) {
        throw std::system_error(std::make_error_code(static_cast<std::errc>(errno)));
      }
    }
    else {
      amount_done += written;
    }
  }
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
  explicit Location(std::string filename,  // NOLINT(bugprone-exception-escape)
                    std::uint64_t loc = 0) noexcept
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
  [[nodiscard]] auto filename() const noexcept -> std::string const& { return filename_; }

  /** \brief  Get the current location. */
  [[nodiscard]] auto loc() const noexcept -> std::uint64_t { return loc_; }

  /** \brief  Set the location. */
  void loc(std::uint64_t l) noexcept { loc_ = l; }

  /** \brief  Increase the location value. */
  void inc_loc(std::uint64_t inc) noexcept
  {
    assert(loc_ < SIZE_MAX - inc);  // NOLINT
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
  [[noreturn]] void error(Gencat::Msg msg, Ts... args) const
  {
    auto error = Gencat::Messages::get().get(Gencat::Set::generic, Gencat::Msg::error);
    std::string result = ::fmt::format("{}:{}:{}: ", filename_, loc_, error);
    result += Gencat::Messages::get().format(Gencat::Set::gencat, msg, args...);
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
  [[nodiscard]] [[nodiscard]] auto warning(Gencat::Msg msg, Ts... args) const -> std::string
  {
    auto warning = Gencat::Messages::get().get(Gencat::Set::generic, Gencat::Msg::warning);
    std::string result = ::fmt::format("{}:{}:{}: ", filename_, loc_, warning);
    result += Gencat::Messages::get().format(Gencat::Set::gencat, msg, args...);
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
template<typename T, typename It, std::enable_if_t<std::is_integral<T>::value, bool> = true>
auto read(It data) -> T
{
  // Slow path - read data byte by byte.
  T result = 0;
  for (unsigned i = 0; i < sizeof(T); ++i) {
    result |= static_cast<T>(*data++) << (i * CHAR_BIT);
  }
  return result;
}

template<typename T, typename It, std::enable_if_t<std::is_integral<T>::value, bool> = true>
void write(It data, T value)
{
  // Slow path - write data byte by byte.
  for (unsigned i = 0; i < sizeof(T); ++i) {
    *data++ = static_cast<std::uint8_t>(value);
    value >>= CHAR_BIT;
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
      loc.error(Gencat::Msg::unable_to_open, file);
    }

    // Read the header
    load_header(is, loc, data);
    auto set_count = read<std::uint32_t>(data.begin() + set_count_off);
    auto file_size = read<std::uint64_t>(data.begin() + file_size_off);
    data.reserve(file_size);

    // Read the rest of the file.
    load_data(is, loc, data, file_size);
    std::uint64_t set_offset = hdr_size;

    // Process each set.
    for (std::uint32_t set = 0; set < set_count; ++set) {
      TableEntry set_te = read_table_entry(loc, data, set_offset);
      set_offset += table_entry_size;

      auto [set_it, success] = sets_.insert({set_te.id, MessageSet()});
      if (!success) {
        loc.error(Gencat::Msg::multiple_set_definitions, set_te.id);
      }

      if (set_te.offset >= file_size) {
        loc.loc(set_te.offset);
        loc.error(Gencat::Msg::message_array_offset_out_of_bounds);
      }

      // Process each message within each set.
      for (std::uint32_t msg = 0; msg < set_te.len; ++msg) {
        TableEntry const msg_te = read_table_entry(loc, data, set_te.offset);
        set_te.offset += table_entry_size;

        auto msg_str = read_string(loc, data, set_te.id, msg_te);

        auto result = set_it->second.insert({msg_te.id, msg_str});
        if (!result.second) {
          loc.error(Gencat::Msg::message_multiple_definition, set_te.id, msg_te.id);
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
        loc.error(Gencat::Msg::unable_to_open, file);
      }
    }
    std::istream& is = (file == "-") ? std::cin : ifs;

    std::string line;

    auto set_it = sets_.insert({NL_SETD, MessageSet()}).first;
    int quote = -1;
    while (std::getline(is, line)) {
      loc.inc_loc(1);

      // Handle line continuations
      while (!line.empty() && line[line.size() - 1] == '\\') {
        std::string line2;
        loc.inc_loc(1);
        if (!std::getline(is, line2)) {
          loc.error(Gencat::Msg::unexpected_eof);
        }
        line = line.substr(0, line.size() - 1);
        line += line2;
      }

      if (line.empty()) {
        continue;
      }

      // Interpret the line.
      constexpr std::string_view set_cmd = "set";
      constexpr std::string_view delset_cmd = "delset";
      constexpr std::string_view quote_cmd = "quote";

      if (line[0] == '$') {
        auto split = line.find(' ');
        auto cmd = line.substr(1, split - 1);
        auto value = split == std::string::npos ? std::string() : line.substr(split + 1);
        if (cmd == set_cmd && !value.empty()) {
          auto r = std::stoul(value);
          validate_id(loc, r, "NL_SETMAX", NL_SETMAX);
          set_it = sets_.insert({r, MessageSet()}).first;
        }
        else if (cmd == delset_cmd && !value.empty()) {
          auto r = std::stoul(value);
          validate_id(loc, r, "NL_TEXTMAX", NL_TEXTMAX);
          auto it = sets_.find(r);
          if (it != sets_.end()) {
            // We only clear (and not erase) just in case $set is set to the same
            // value that we've just erased.
            it->second.clear();
          }
        }
        else if (cmd == quote_cmd) {
          if (value.empty()) {
            quote = -1;
          }
          else if (value.size() > 1) {
            loc.error(Gencat::Msg::bad_quote_character);
          }
          else {
            quote = static_cast<unsigned char>(value[0]);
          }
        }
        else if (cmd.empty()) {
          /* Comment.  */
          continue;
        }
        else {
          loc.error(Gencat::Msg::unrecognised_line);
        }
      }
      else if (std::isdigit(line[0]) != 0) {
        std::size_t pos = 0;
        auto r = std::stoul(line, &pos);
        validate_id(loc, r, "NL_MSGMAX", NL_MSGMAX);

        if (line.size() == pos) {
          set_it->second.erase(r);
        }
        else if (line[pos] != ' ') {
          loc.error(Gencat::Msg::message_should_be_followed_by_space);
        }
        else {
          std::string value(line.substr(pos + 1));
          if (!value.empty() && quote != -1 && value[0] == static_cast<char>(quote)) {
            if (value[value.size() - 1] != static_cast<char>(quote)) {
              std::cerr << loc.warning(Gencat::Msg::quoted_string_not_terminated);
            }
            else {
              value = value.substr(1, value.size() - 2);
            }
          }
          value = parse_escaped_string(value, loc);
          auto [it, success] = set_it->second.insert({r, std::string(value)});
          if (!success) {
            std::cerr << loc.warning(Gencat::Msg::replacing_message, set_it->first, r) << "\n";
            it->second = std::string(value);
          }
        }
      }
      else {
        loc.error(Gencat::Msg::unrecognised_line);
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
    write(data.begin() + set_count_off, static_cast<std::uint32_t>(sets_.size()));
    // Can't write file size yet - as we don't know it.

    data.resize(hdr_size + sets_.size() * table_entry_size);
    Offset set_offset = hdr_size;
    for (auto const& kv : sets_) {
      // Msg Array needs to be aligned to 8 byte boundary - and goes at current end of file.
      Offset msg_array_offset =
        (data.size() + off_align - 1) & ~static_cast<Data::size_type>(off_align - 1);
      data.resize(msg_array_offset + kv.second.size() * table_entry_size);

      // Write set entry pointing to message array.
      write_table_entry(
        loc, data, set_offset,
        TableEntry{kv.first, static_cast<std::uint32_t>(kv.second.size()), msg_array_offset});
      set_offset += table_entry_size;

      // Write message array entries.
      for (auto const& kv2 : kv.second) {
        std::uint64_t const msg_offset = data.size();
        write_table_entry(
          loc, data, msg_array_offset,
          TableEntry{kv2.first, static_cast<std::uint32_t>(kv2.second.size()) + 1, msg_offset});
        msg_array_offset += table_entry_size;
        for (auto c : kv2.second) {
          data.push_back(static_cast<std::uint8_t>(c));
        }
        // Null terminate the string.
        data.push_back(0);
      }
    }

    // Now we can write the file size.
    write(data.begin() + file_size_off, static_cast<std::uint64_t>(data.size()));

    // Now write data to the file.
    xwrite(fd, GD::Span::span<uint8_t>(data.data(), data.size()));
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

  /** \brief      Parse an escaped string and interpret the escapes.
   *  \param  s   String to escape
   *  \param  loc Location of string
   *  \return     Escaped string
   *  \throws     runtime_errors if the string is not properly escaped.
   */
  static auto parse_escaped_string(std::string const& s, Location& loc) -> std::string
  {
    enum class State { normal, backslash, octal1, octal2 };
    std::string result;
    State state = State::normal;
    unsigned int o = 0;
    for (auto c = s.begin(); c != s.end(); ++c) {
      if (state == State::normal) {
        if (*c == '\\') {
          state = State::backslash;
        }
        else {
          result += *c;
        }
      }
      else if (state == State::octal1) {
        if (*c >= '0' && *c <= '7') {
          o = (o << 3) + (*c - '0');
          state = State::octal2;
        }
        else {
          result += static_cast<char>(o);
          state = State::normal;
          /* Need to rescan C.  */
          --c;
        }
      }
      else if (state == State::octal2) {
        if (*c >= '0' && *c <= '7') {
          o = (o << 3) + (*c - '0');
          if (o <= std::numeric_limits<unsigned char>::max()) {
            result += static_cast<char>(o);
          }
        }
        else {
          result += static_cast<char>(o);
          --c;
        }
        state = State::normal;
      }
      else if (state == State::backslash) {
        state = State::normal;
        switch (*c) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
          o = *c - '0';
          state = State::octal1;
          break;
        case '\\':
          result += '\\';
          break;
        case 'n':
          result += '\n';
          break;
        case 't':
          result += '\t';
          break;
        case 'v':
          result += '\v';
          break;
        case 'b':
          result += '\b';
          break;
        case 'r':
          result += '\r';
          break;
        case 'f':
          result += '\f';
          break;
        default:
          loc.error(Gencat::Msg::bad_escape_character, *c);
          break;
        }
      }
      else {
        assert(false && "Unhandled escape state.");  // NOLINT
      }
    }

    switch (state) {
    case State::backslash:
      loc.error(Gencat::Msg::unterminated_escape, s);
      break;
    case State::octal1:
    case State::octal2:
      result += static_cast<char>(o);
    case State::normal:
      break;
    default:
      assert(false && "Unhandled switch case.");  // NOLINT
    }

    return result;
  }

  /** \brief      Load the initial header into a data structure.
   *  \param is   Input stream to ready from
   *  \param loc  Location object to use.
   *  \param data Data location to store header in.
   *
   * Will validate the header.
   */
  static void load_header(std::istream& is, Location& loc, Data& data)
  {
    std::vector<char> buf(hdr_size);
    if (!is.read(buf.data(), static_cast<std::streamsize>(buf.size()))) {
      loc.error(Gencat::Msg::catalogue_too_short);
    }
    std::copy(buf.begin(), buf.begin() + is.gcount(), std::back_inserter(data));

    if (data[0] != 'M' || data[1] != 'S' || data[2] != 'G' || data[3] != '\0') {
      loc.error(Gencat::Msg::catalogue_magic_missing);
    }
    if (data[4] != 1) {
      loc.error(Gencat::Msg::catalogue_not_version1);
    }
    for (auto i = version_off + 1; i < magic_size; ++i) {
      if (data[i] != 0) {
        loc.loc(i);
        loc.error(Gencat::Msg::header_non0_reserved_fields);
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
    std::array<char, bufsize> buf{};
    while (is.read(buf.data(), buf.size())) {
      std::copy(buf.begin(), buf.begin() + is.gcount(), std::back_inserter(data));
      if (data.size() > file_size) {
        loc.loc(file_size);
        loc.error(Gencat::Msg::message_catalogue_too_large);
      }
    }
    std::copy(buf.begin(), buf.begin() + is.gcount(), std::back_inserter(data));
    if (data.size() > file_size) {
      loc.loc(file_size);
      loc.error(Gencat::Msg::message_catalogue_too_large);
    }
    if (data.size() < file_size) {
      loc.loc(data.size());
      loc.error(Gencat::Msg::catalogue_size_not_consistent_small);
    }
  }

  /** \brief         Read a table entry
   *  \param  loc    Location object
   *  \param  data   Data to be read.
   *  \param  offset Offset of table entry within data.
   *  \return        A table entry.
   */
  static auto read_table_entry(Location& loc, Data const& data, Offset offset) -> TableEntry
  {
    if (offset > data.size() - table_entry_size) {
      loc.loc(offset);
      loc.error(Gencat::Msg::table_entry_outside_bounds);
    }
    if (offset % off_align != 0) {
      loc.loc(offset);
      loc.error(Gencat::Msg::table_entry_not_aligned);
    }
    auto raw_data = data.begin() + static_cast<Data::difference_type>(offset);
    Id const id = read<std::uint32_t>(raw_data + table_entry_id_off);
    auto len = read<std::uint32_t>(raw_data + table_entry_len_off);
    auto off = read<std::uint64_t>(raw_data + table_entry_off_off);
    return TableEntry{id, len, off};
  }

  /** \brief        Write a table entry
   *  \param data   Data to write into
   *  \param offset Offset to write at
   *  \param te     Table entry to write
   */
  static void write_table_entry(Location& loc, Data& data, Offset offset, TableEntry const& te)
  {
    if (offset % off_align != 0) {
      loc.loc(offset);
      loc.error(Gencat::Msg::table_entry_not_aligned);
    }
    write(data.begin() + static_cast<Data::difference_type>(offset + table_entry_id_off), te.id);
    write(data.begin() + static_cast<Data::difference_type>(offset + table_entry_len_off), te.len);
    write(data.begin() + static_cast<Data::difference_type>(offset + table_entry_off_off),
          te.offset);
  }

  /** \brief         Read a string
   *  \param  loc    Location object
   *  \param  data   Data to read from.
   *  \param  set_id ID of set string belongs to.
   *  \param  te     Message table entry giving location of string to read.
   *  \return        Read string.
   */
  static auto read_string(Location& loc, Data const& data, Id set_id, TableEntry const& te)
    -> std::string
  {
    loc.loc(te.offset);
    if (te.offset >= data.size()) {
      loc.error(Gencat::Msg::message_starts_beyond_end, set_id, te.id);
    }
    if (te.len > data.size()) {
      loc.error(Gencat::Msg::message_bigger_than_file, set_id, te.id);
    }
    if (te.offset > data.size() - te.len) {
      loc.error(Gencat::Msg::message_ends_beyond_end, set_id, te.id);
    }
    if (data[te.offset + te.len - 1] != '\0') {
      loc.error(Gencat::Msg::message_no_nul, set_id, te.id);
    }

    std::string r;
    std::transform(data.begin() + static_cast<Data::difference_type>(te.offset),
                   data.begin() + static_cast<Data::difference_type>(te.offset + te.len - 1),
                   std::back_inserter(r), [](auto c) { return static_cast<char>(c); });
    return r;
  }

  // NOLINTNEXTLINE
  static void validate_id(Location& loc, unsigned long id, const char* soft_limit_name,
                          Id soft_limit)
  {
    if (id > std::numeric_limits<Id>::max()) {
      loc.error(Gencat::Msg::id_too_big_hard, id);
    }
    else if (id > soft_limit) {
      std::cerr << loc.warning(Gencat::Msg::id_too_big_soft, id, soft_limit_name, soft_limit)
                << '\n';
    }
    else if (id < 1) {
      loc.error(Gencat::Msg::id_too_small, id);
    }
  }

  static constexpr Offset off_align = 8;            ///< Offset alignments
  static constexpr Offset version_off = 4;          ///< Offset of version number
  static constexpr Offset magic_size = 12;          ///< Size of header magic
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

auto main(int argc, char** argv) -> int
try {
  (void)std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)
  GD::Span::span<char*> args(argv, argc);
  GD::Span::span<char*>::iterator it = args.begin();

  program_name = ::basename(*it++);  // NOLINT(concurrency-mt-unsafe)

  // Skip '--' if present as the first argument.
  if (it != args.end() && std::strcmp(*it, "--") == 0) {
    ++it;
  }

  if (it == args.end()) {
    std::cerr << Gencat::Messages::get().format(Gencat::Set::gencat, Gencat::Msg::missing_arguments,
                                                program_name);
    std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
  }

  std::string_view catfile(*it++);
  MessageCatalogue cat;

  if (it == args.end()) {
    std::cerr << Gencat::Messages::get().format(Gencat::Set::gencat, Gencat::Msg::missing_arguments,
                                                program_name);
    std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
  }

  bool updating = catfile != "-" && fs::exists(catfile);
  if (updating) {
    cat.load_catfile(catfile);
  }

  std::for_each(it, args.end(), [&cat](auto msgfile) { cat.load_msgfile(msgfile); });

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
      // We know on Windows that the following line will have equivalent values
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
      std::cerr << Gencat::Messages::get().format(
        Gencat::Set::gencat, Gencat::Msg::unable_to_remove, program_name, catfile);
      failed = true;
    }
    else if (rename(outfile.data(), catfile.data()) == -1) {
      std::cerr << Gencat::Messages::get().format(
        Gencat::Set::gencat, Gencat::Msg::unable_to_rename, program_name, outfile, catfile);
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
  std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
}
catch (...) {
  std::cerr << Gencat::Messages::get().format(Gencat::Set::generic,
                                              Gencat::Msg::unrecognised_exception);
}
