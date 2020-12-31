/** \file   gencat.cc
 *  \brief  Main program for gencat
 *  \author Copyright 2020, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/filesystem.hh"
#include "gd/format.hh"

#include <bit>
#include <cassert>
#include <climits>
#include <cstdlib>
#include <fcntl.h>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <nl_types.h>
#include <string>
#include <string_view>
#include <system_error>
#include <unistd.h>
#include <vector>

namespace {
std::string_view program_name; ///< Program name - somewhere global for all.

/** \brief A basic location recorder.
 *
 * Records a filename and location (which may be line number, offset, or some
 * other mechanism of recoridng the location in a file).
 *
 * Can be used to throw error messages, or get warning messages.
 */
class Location {
public:
  /** \brief          Constructor
   *  \param filename File name
   *  \param loc      Initial location (defaults to 0).
   */
  explicit Location(std::string filename, std::uint64_t loc = 0) noexcept
      : filename_(std::move(filename)), loc_(loc) {}

  /** \brief          Constructor
   *  \param filename File name
   *  \param loc      Initial location (defaults to 0).
   */
  explicit Location(std::string_view filename, std::uint64_t loc = 0)
      : filename_(filename), loc_(loc) {}

  /** \brief  Get the filename. */
  std::string const &filename() const noexcept { return filename_; }

  /** \brief  Get the current location. */
  std::uint64_t loc() const noexcept { return loc_; }

  /** \brief  Set the location. */
  void loc(std::uint64_t l) noexcept { loc_ = l; }

  /** \brief  Increase the location value. */
  void inc_loc(std::uint64_t inc) noexcept {
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
  template <typename... Ts>
  [[noreturn]] void error(std::string const &f, Ts... args) {
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
  template <typename... Ts>
  std::string warning(std::string const &f, Ts... args) {
    std::string result = ::fmt::format("{}:{}:WARNING: ", filename_, loc_);
    result += ::fmt::format(f, args...);
    return result;
  }

private:
  std::string filename_; ///< File name
  std::uint64_t loc_;    ///< Current location
};

/** \brief       Read a little-endian integer from a data block.
 *  \tparam T    Integer type to read.
 *  \param  data Offset of data to read.
 *  \return      Value
 *
 * Caller is responsible for ensuring \a data points to valid memory.
 *
 * If the system being compiled for is big-endian, or the data is not naturally
 * aligned a slow byte-by-byte read is done.  Otherwise we just do a type
 * conversion.
 */
template <typename T, std::enable_if_t<std::is_integral<T>::value, bool> = true>
T read(std::uint8_t const *data) {
  // Fast path - no endian conversion, and data is appropriately aligned.
  if constexpr (std::endian::native == std::endian::little) {
    if (((uintptr_t)data) % sizeof(T) == 0) {
      return *(reinterpret_cast<T const *>(data));
    }
  }

  // Slow path - read data byte by byte.
  T result;
  for (unsigned i = 0; i < sizeof(T); ++i) {
    result = result | (data[i] << (i * 8));
  }
  return result;
}

template <typename T, std::enable_if_t<std::is_integral<T>::value, bool> = true>
void write(std::uint8_t *data, T value) {
  // Fast path - no endian conversion, and data is appropriately aligned.
  if constexpr (std::endian::native == std::endian::little) {
    if (((uintptr_t)data) % sizeof(T) == 0) {
      *(reinterpret_cast<T *>(data)) = value;
    }
  }

  // Slow path - write data byte by byte.
  for (unsigned i = 0; i < sizeof(T); ++i) {
    data[i] = std::uint8_t(value & 0xff);
    value >>= 8;
  }
}

class MessageCatalogue {
public:
  using MessageSet = std::map<std::uint32_t, std::string>;
  using SetMap = std::map<std::uint32_t, MessageSet>;

  void load_catfile(std::string_view const &file) {

    if (!sets_.empty()) {
      throw std::runtime_error(
          "Calling load_catfile on non-empty MessageCatalogue");
    }

    std::ifstream is(file.data(), std::ios::binary | std::ios::in);
    std::vector<std::uint8_t> data;
    char tmp[4096];
    Location loc(file, 0);

    // Read the header
    if (!is.read(tmp, 24)) {
      throw std::runtime_error("File is too short for a message catalogue.");
    }
    data.insert(data.end(), tmp, tmp + is.gcount());

    // Check header
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
    std::uint32_t set_count = read<std::uint32_t>(data.data() + 12);
    std::uint64_t file_size = read<std::uint64_t>(data.data() + 16);
    data.reserve(file_size);

    // Read the rest of the file.
    while (is.read(tmp, 4096)) {
      data.insert(data.end(), tmp, tmp + 4096);
      if (data.size() > file_size) {
        loc.loc(file_size);
        loc.error("File is larger than it claims.");
      }
    }
    data.insert(data.end(), tmp, tmp + is.gcount());
    if (data.size() > file_size) {
      loc.loc(file_size);
      loc.error("File is larger than it claims.");
    }

    std::uint64_t set_offset = 24;
    auto raw_data = data.data();
    for (std::uint32_t set = 0; set < set_count; ++set) {
      loc.loc(set_offset);
      if (set_offset > file_size - 16) {
        loc.error("File is not big enough for set table.");
      }
      std::uint32_t set_id = read<std::uint32_t>(raw_data + set_offset + 0);
      std::uint32_t set_msg_count =
          read<std::uint32_t>(raw_data + set_offset + 4);
      std::uint64_t set_msg_array_offset =
          read<std::uint64_t>(raw_data + set_offset + 8);
      set_offset += 16;

      auto [set_it, success] = sets_.insert({set_id, MessageSet()});
      if (!success) {
        loc.error("Message catalogue contains multiple definitions of set {}",
                  set_id);
      }

      if (set_msg_array_offset >= file_size) {
        loc.loc(set_msg_array_offset);
        loc.error("Message array offset outside bounds of message catalogue.");
      }
      for (std::uint32_t msg = 0; msg < set_msg_count; ++msg) {
        loc.loc(set_msg_array_offset);
        if (set_msg_array_offset > file_size - 16) {
          loc.error("Message Catalogue for set {} is not big enough for "
                    "message table.",
                    set_id);
        }
        std::uint32_t msg_id =
            read<std::uint32_t>(raw_data + set_msg_array_offset + 0);
        std::uint32_t msg_length =
            read<std::uint32_t>(raw_data + set_msg_array_offset + 4);
        std::uint64_t msg_offset =
            read<std::uint64_t>(raw_data + set_msg_array_offset + 8);
        set_msg_array_offset += 16;

        loc.loc(msg_offset);
        if (msg_offset >= file_size) {
          loc.error(
              "Message {}.{} starts beyond the end of the message catalogue.",
              set_id, msg_id);
        }
        if (msg_length > file_size) {
          loc.error("Message {}.{} is longer than the length of the message "
                    "catalogue.",
                    set_id, msg_id);
        }
        if (msg_offset > file_size - msg_length) {
          loc.error("Message {}.{} overflows the end of the file.", set_id,
                    msg_id);
        }

        auto [msg_it, success] = set_it->second.insert(
            {msg_id,
             std::string(reinterpret_cast<char const *>(raw_data + msg_offset),
                         msg_length)});
        if (!success) {
          loc.error("Multiple definitions of message with ID: {}.{}.", set_id,
                    msg_id);
        }
      }
    }
  }

  void load_msgfile(std::string_view const &file) {
    std::ifstream ifs;
    if (file != "-") {
      ifs.open(file.data());
    }
    std::istream &is = (file == "-") ? std::cin : ifs;

    std::string line;
    Location loc(file, 0);
    auto set_it = sets_.insert({NL_SETD, MessageSet()}).first;
    int quote = -1;
    while (std::getline(ifs, line)) {
      loc.inc_loc(1);

      // Handle line continuations
      while (line[line.size() - 1] == '\\') {
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
        if (r > std::numeric_limits<std::uint32_t>::max()) {
          loc.error("Set number {} is too large.", r);
        } else if (r > NL_SETMAX) {
          std::cerr << loc.warning("Set number {} is larger than NL_SETD ({}).",
                                   r, NL_SETD)
                    << '\n';
        } else if (r < 1) {
          loc.error("Set number {} is too small, must be at least 1.", r);
        }

        set_it = sets_.insert({r, MessageSet()}).first;
      } else if (line.substr(0, 8) == "$delset ") {
        auto r = std::stoul(line.substr(8));
        auto it = sets_.find(r);
        if (it != sets_.end()) {
          it->second.clear();
        }
      } else if (line.substr(0, 2) == "$ ") {
        /* Do nothing: Comment.  */
      } else if (line.substr(0, 7) == "$quote ") {
        if (line.size() > 8) {
          loc.error("Quote should be a single character, or empty to clear.");
        } else if (line.size() == 7) {
          quote = -1;
        } else {
          quote = line[7];
        }
      } else if (line == "$quote") {
        quote = -1;
      } else if (std::isdigit(line[0])) {
        std::size_t pos = 0;
        auto r = std::stoul(line, &pos);
        if (r > std::numeric_limits<std::uint32_t>::max()) {
          loc.error("Message number {} is too large.", r);
        } else if (r > NL_MSGMAX) {
          std::cerr << loc.warning(
                           "Message number {} is larger than NL_MSGMAX ({}).",
                           r, NL_MSGMAX)
                    << '\n';
        } else if (r < 1) {
          loc.error("Message number {} is too small, must be at least 1.", r);
        }

        if (line.size() == pos) {
          set_it->second.erase(r);
        } else if (line[pos] != ' ') {
          loc.error("Message number should be followed by a space.");
        } else {
          std::string value(line.substr(pos + 1));
          if (!value.empty() && quote != -1 &&
              value[0] == (char)(quote & 0xff)) {
            if (value[value.size() - 1] != (char)(quote & 0xff)) {
              std::cerr << loc.warning(
                  "String starts with a quote but does not end with one.");
            } else {
              value = value.substr(1, value.size() - 2);
            }
          }
          auto [it, success] = set_it->second.insert({r, std::string(value)});
          if (!success) {
            std::cerr << loc.warning("Replacing existing message ID {}.{}",
                                     set_it->first, r)
                      << "\n";
            it->second = std::string(value);
          }
        }
      } else {
        loc.error("Unrecognised line contents.");
      }
    }

    // Delete any empty sets.
    auto it = sets_.begin();
    while (it != sets_.end()) {
      if (it->second.empty()) {
        it = sets_.erase(it);
      } else {
        ++it;
      }
    }
  }

  void emit(int fd) {
    std::vector<std::uint8_t> data{'M', 'S', 'G', '\0', 1, 0, 0, 0, 0, 0, 0, 0};
    data.resize(24);
    write(data.data() + 12, std::uint32_t(sets_.size()));
    // Can't write file size yet.

    data.resize(24 + sets_.size() * 16);
    std::uint64_t set_offset = 24;
    for (auto const &kv : sets_) {
      write(data.data() + set_offset, kv.first);
      write(data.data() + set_offset + 4, std::uint32_t(kv.second.size()));
      std::uint64_t msg_array_offset = data.size();
      write(data.data() + set_offset + 8, msg_array_offset);
      set_offset += 16;

      data.resize(data.size() + kv.second.size() * 16);
      for (auto const &kv2 : kv.second) {
        write(data.data() + msg_array_offset, kv2.first);
        write(data.data() + msg_array_offset + 4, kv2.second.size());
        std::uint64_t msg_offset = data.size();
        write(data.data() + msg_array_offset + 8, msg_offset);
        msg_array_offset += 16;
        for (auto c : kv2.second) {
          data.push_back(std::uint8_t(c));
        }
      }
    }

    // Now we can write the file size.
    write(data.data() + 16, std::uint64_t(data.size()));

    // Now write data to the file.
    std::uint64_t offset = 0;
    while (offset < data.size()) {
      std::uint64_t left = data.size() - offset;
      ssize_t written = ::write(fd, data.data() + offset,
                                std::min(std::uint64_t(SSIZE_MAX), left));
      if (written == -1) {
        if (errno != EINTR) {
          throw std::system_error(
              std::make_error_code(static_cast<std::errc>(errno)));
        }
      } else {
        offset += written;
      }
    }
  }

private:
  SetMap sets_;
};
} // namespace

int main(int argc, char **argv) try {
  program_name = ::basename(argv[0]);

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
    } else if (outfile == "-") {
      fd = STDOUT_FILENO;
    } else {
      fd = ::open(outfile.c_str(), O_CREAT | O_WRONLY | O_TRUNC,
                  S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    }

    if (fd == -1) {
      throw std::system_error(
          std::make_error_code(static_cast<std::errc>(errno)));
    }

    cat.emit(fd);
  } catch (std::exception &e) {
    std::cerr << e.what() << "\n";
    failed = true;
    remove_outfile = (fd != -1 && fd != STDOUT_FILENO);
  }

  if (fd != -1 && fd != STDOUT_FILENO) {
    close(fd);
  }

  if (updating) {
    if (unlink(catfile.data()) == -1) {
      std::cerr << fmt::format("{}: Unable to remove file: {}\n", program_name,
                               catfile);
      failed = true;
    } else if (rename(outfile.data(), catfile.data()) == -1) {
      std::cerr << fmt::format("{}: Unable to remame file {} to {}\n",
                               program_name, outfile, catfile);
      std::cerr << fmt::format("{}: Leaving temporary file in place.\n",
                               program_name);
      remove_outfile = false;
      failed = true;
    }
  }

  if (remove_outfile) {
    unlink(outfile.c_str());
    // Silently ignore any errors in the above.
  }

  return failed ? EXIT_FAILURE : EXIT_SUCCESS;
} catch (std::exception const &e) {
  std::cerr << fmt::format("{}: {}\n", program_name, e.what());
  std::exit(EXIT_FAILURE);
} catch (...) {
  std::cerr << fmt::format("Unrecognised exception");
}
