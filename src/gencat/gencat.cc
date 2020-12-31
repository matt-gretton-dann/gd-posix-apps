/** \file   gencat.cc
 *  \brief  Main program for gencat
 *  \author Copyright 2020, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/filesystem.hh"
#include <bit>
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
std::string_view program_name;

class ErrorReporter {
public:
  ErrorReporter(std::string_view const &file, std::size_t lineno,
                bool error = true)
      : exit_on_destruction_(error) {
    std::cerr << file << ":" << lineno << ":" << (error ? "ERROR" : "WARNING")
              << ": ";
  }

  ~ErrorReporter() {
    if (exit_on_destruction_) {
      std::exit(1);
    }
  }

  std::ostream &stream() const noexcept { return std::cerr; }

private:
  bool exit_on_destruction_;
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

    // Read the header
    if (!is.read(tmp, 24)) {
      throw std::runtime_error("File is too short for a message catalogue.");
    }
    data.insert(data.end(), tmp, tmp + is.gcount());

    // Check header
    if (data[0] != 'M' || data[1] != 'S' || data[2] != 'G' || data[3] != '\0') {
      throw std::runtime_error("File not a message catalogue.");
    }
    if (data[4] != '1') {
      throw std::runtime_error("File not version 1 message catalogue.");
    }
    for (auto i = 5; i < 12; ++i) {
      if (data[i] != 0) {
        throw std::runtime_error("Reserved field not 0");
      }
    }
    std::uint32_t set_count = read<std::uint32_t>(data.data() + 12);
    std::uint64_t file_size = read<std::uint64_t>(data.data() + 16);
    data.reserve(file_size);

    // Read the rets of the file.
    while (is.read(tmp, 4096)) {
      data.insert(data.end(), tmp, tmp + 4096);
      if (data.size() > file_size) {
        throw std::runtime_error("File is larger than documented ");
      }
    }
    data.insert(data.end(), tmp, tmp + is.gcount());
    if (data.size() > file_size) {
      throw std::runtime_error("File is larger than documented.");
    }

    std::uint64_t set_offset = 24;
    auto raw_data = data.data();
    for (std::uint32_t set = 0; set < set_count; ++set) {
      if (set_offset > file_size - 16) {
        throw std::runtime_error("File is not big enough for set table");
      }
      std::uint32_t set_id = read<std::uint32_t>(raw_data + set_offset + 0);
      std::uint32_t set_msg_count =
          read<std::uint32_t>(raw_data + set_offset + 4);
      std::uint64_t set_msg_array_offset =
          read<std::uint64_t>(raw_data + set_offset + 8);
      set_offset += 16;

      auto [set_it, success] = sets_.insert({set_id, MessageSet()});
      if (!success) {
        throw std::runtime_error("File contains duplicate sets.");
      }

      if (set_msg_array_offset < file_size) {
        throw std::runtime_error("Message array offset not in file");
      }
      for (std::uint32_t msg = 0; msg < set_msg_count; ++msg) {
        if (set_msg_array_offset > file_size - 16) {
          throw std::runtime_error("Message array overflows file size");
        }
        std::uint32_t msg_id =
            read<std::uint32_t>(raw_data + set_msg_array_offset + 0);
        std::uint32_t msg_length =
            read<std::uint32_t>(raw_data + set_msg_array_offset + 4);
        std::uint64_t msg_offset =
            read<std::uint64_t>(raw_data + set_msg_array_offset + 8);
        set_msg_array_offset += 16;

        if (msg_offset >= file_size) {
          throw std::runtime_error("Message starts beyond end of file.");
        }
        if (msg_length > file_size) {
          throw std::runtime_error("Message length is way too long.");
        }
        if (msg_offset > file_size - msg_length) {
          throw std::runtime_error("Message overflows end of file.");
        }

        auto [msg_it, success] = set_it->second.insert(
            {msg_id,
             std::string(reinterpret_cast<char const *>(raw_data + msg_offset),
                         msg_length)});
        if (!success) {
          throw std::runtime_error("Duplicate messages");
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
    std::size_t lineno = 0;
    auto set_it = sets_.insert({NL_SETD, MessageSet()}).first;
    int quote = -1;
    while (std::getline(ifs, line)) {
      ++lineno;

      // Handle line continuations
      while (line[line.size() - 1] == '\\') {
        std::string line2;
        ++lineno;
        if (!std::getline(ifs, line2)) {
          ErrorReporter err(file, lineno);
          err.stream() << "Unexpected EOF.\n";
        }
        line = line.substr(0, line.size() - 1) + line2;
      }

      // Interpret the line.
      if (line.substr(0, 5) == "$set ") {
        auto r = std::stoul(line.substr(5));
        if (r > std::numeric_limits<std::uint32_t>::max()) {
          ErrorReporter err(file, lineno);
          err.stream() << "Set number " << r << " is too large.\n";
        } else if (r > NL_SETMAX) {
          ErrorReporter err(file, lineno, false);
          err.stream() << "Set number " << r << " is larger than NL_SETD ("
                       << NL_SETD << ").\n";
        } else if (r < 1) {
          ErrorReporter err(file, lineno);
          err.stream() << "Set number " << r
                       << " is too small (must be at least 1).\n";
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
          std::cerr << file << ":" << lineno
                    << ":ERROR: Quote should only be one character.\n";
          std::exit(1);
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
          std::cerr << file << ":" << lineno << ":ERROR: Message number " << r
                    << " is too large.\n";
          std::exit(1);
        } else if (r > NL_MSGMAX) {
          std::cerr << file << ":" << lineno << ":WARNING: Message number " << r
                    << " is larger than NL_MSGMAX (" << NL_MSGMAX << ").\n";
        } else if (r < 1) {
          std::cerr << file << ":" << lineno << ":ERROR: Message number " << r
                    << " is too small (must be at least 1).\n";
          std::exit(1);
        }

        if (line.size() == pos) {
          set_it->second.erase(r);
        } else if (line[pos] != ' ') {
          std::cerr
              << file << ":" << lineno
              << ":ERROR: Message number should be followed by a space.\n";
          std::exit(1);
        } else {
          std::string_view value(line.substr(pos + 1));
          if (!value.empty() && quote != -1 &&
              value[0] == (char)(quote & 0xff)) {
            if (value[value.size() - 1] != (char)(quote & 0xff)) {
              std::cerr << file << ":" << lineno
                        << ":WARNING: Non-terminated quoted string.\n";
            } else {
              value = value.substr(1, value.size() - 2);
            }
          }
          auto [it, success] = set_it->second.insert({r, std::string(value)});
          if (!success) {
            std::cerr << file << ":" << lineno
                      << ":WARNING: Replacing existing ID " << r << "\n";
          }
        }
      } else {
        std::cerr << file << ":" << lineno << ":ERROR: Unrecognised line.\n";
        std::exit(1);
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
      ++set_offset;

      data.resize(data.size() + kv.second.size() * 16);
      for (auto const &kv2 : kv.second) {
        write(data.data() + msg_array_offset, kv2.first);
        write(data.data() + msg_array_offset + 4, kv2.second.size());
        std::uint64_t msg_offset = data.size();
        write(data.data() + msg_array_offset + 8, msg_offset);
        data.resize(msg_offset + kv2.second.size());
        for (auto c : kv2.second) {
          data[msg_offset++] = std::uint8_t(c);
        }
      }
    }

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

int main(int argc, char **argv) {
  program_name = argv[0];

  if (argc < 3) {
    std::cerr << program_name
              << ": Need to specify catalog and at least one message file.\n";
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

  std::string outfile(catfile);
  int fd = -1;
  if (updating) {
    outfile += ".XXXXXX";
    fd = ::mkstemp(outfile.data());
  } else if (outfile == "-") {
    fd = STDOUT_FILENO;
  } else {
    fd = ::open(outfile.c_str(), O_CREAT | O_WRONLY | O_TRUNC,
                S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  }

  if (fd == -1) {
    std::cerr << program_name << ": Unable to open file: " << outfile << "\n";
  }

  bool failed = false;
  bool remove_outfile = false;
  try {
    cat.emit(fd);
  } catch (std::exception &e) {
    std::cerr << e.what() << "\n";
    failed = true;
    remove_outfile = true;
    std::exit(EXIT_FAILURE);
  }

  if (fd != STDOUT_FILENO) {
    close(fd);
  }

  if (updating) {
    if (unlink(catfile.data()) == -1) {
      std::cerr << program_name << ": Unable to remove file: " << catfile
                << "\n";
      failed = true;
      remove_outfile = true;
    } else if (rename(outfile.data(), catfile.data()) == -1) {
      std::cerr << program_name << ": Unable to rename file: " << outfile
                << " to " << catfile << "\n";
      std::cerr << "Leaving temporary file in place.\n";
      failed = true;
    }
  }

  if (remove_outfile) {
    unlink(outfile.c_str());
    // Silently ignore any errors in the above.
  }

  return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}
