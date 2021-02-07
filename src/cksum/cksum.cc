/** \file   cksum.cpp
 *  \brief  Implement cksum utility
 *  \author Copyright 2020-21, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/string.h"
#include "message-data.hh"
#include "util/file.hh"
#include "util/utils.hh"

#include <array>
#include <iostream>
#include <istream>
#include <limits>
#include <locale.h>
#include <stdint.h>

namespace {
constexpr unsigned uint8_max = std::numeric_limits<::uint8_t>::max();
constexpr unsigned uint8_bits = 8;
constexpr unsigned uint24_bits = 24;
}  // namespace

namespace GD::Cksum {
extern std::array<::uint32_t, uint8_max + 1> cksum_table;
}

namespace {
template<typename... Ts>
void report_error(GD::Cksum::Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Cksum::Messages::get().format(GD::Cksum::Set::cksum, msg, args...) << '\n';
}

auto update_crc(::uint32_t crc, ::uint8_t c) -> ::uint32_t
{
  return (crc << uint8_bits) ^ GD::Cksum::cksum_table[(crc >> uint24_bits) ^ c];
}

auto do_cksum(std::string_view fname) -> bool
{
  ::size_t len = 0;
  ::uint32_t crc = 0;
  int c;
  GD::InputFile fh(fname, "rb");
  if (fh.error()) {
    return false;
  }

  /* Walk through the stream.  */
  while ((c = fh.getc()) != EOF) {
    ++len;
    crc = update_crc(crc, static_cast<::uint8_t>(c));
  }
  if (fh.error()) {
    return false;
  }

  // Add the length of the stream
  ::size_t clen = len;
  while (clen != 0) {
    crc = update_crc(crc, clen & uint8_max);
    clen >>= uint8_bits;
  }

  // Invert result
  crc = ~crc;

  std::cout << crc << " " << len;
  if (fname != "-") {
    std::cout << " " << fname;
  }
  std::cout << "\n";
  return true;
}

}  // namespace

auto main(int argc, char** argv) -> int
try {
  ::setlocale(LC_ALL, "");
  GD::program_name(argv[0]);

  // Skip a '--' option
  if (argc > 1 && std::strcmp(argv[1], "--") == 0) {
    ++argv;
    --argc;
  }

  return GD::for_each_file(argc - 1, argv + 1, do_cksum) ? EXIT_SUCCESS : EXIT_FAILURE;
}
catch (std::exception const& e) {
  report_error(GD::Cksum::Msg::unhandled_std_exception, e.what());
  ::exit(EXIT_FAILURE);
}
catch (...) {
  report_error(GD::Cksum::Msg::unhandled_exception);
  ::exit(EXIT_FAILURE);
}
