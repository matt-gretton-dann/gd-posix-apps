/** \file   gen-cksum-table.cpp
 *  \brief  Generate the checksum lookup-table
 *  \author Copyright 2020-2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"

#include "gd/limits.h"
#include "gd/span.hh"
#include "gd/stdlib.h"

#include <cinttypes>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>

namespace {
constexpr unsigned uint8_max = std::numeric_limits<std::uint8_t>::max();
constexpr unsigned uint8_bits = 8;
constexpr unsigned uint24_bits = 24;
}  // namespace

auto main(int argc, char** argv) -> int
{
  // The polynomial we're generating a table for.
  constexpr std::uint32_t poly = 0b0000'0100'1100'0001'0001'1101'1011'0111;
  constexpr std::uint32_t table_size = uint8_max + 1;
  constexpr std::uint32_t bit31 = 1U << 31;

  GD::Span::span<char*> args(argv, argc);
  if (args.size() != 2) {
    // NOLINTNEXTLINE(concurrency-mt-unsafe)
    std::cerr << ::basename(args[0]) << ": Need to specify output file.\n";
    return EXIT_FAILURE;
  }

  std::ofstream of(args[1]);

  of << "/** \\file   cksum-table.cc\n"
        " *  \\brief  Checksum table\n"
        " *  \\author Generated from gen-cksum-table.cc\n"
        " */\n"
        "\n"
        "/* clang-format off */\n"
        "\n"
        "#include <array>\n"
        "#include <cinttypes>\n"
        "#include <limits>\n"
        "\n"
        "namespace GD::Cksum {\n"
        "\n"
        "extern const std::array<std::uint32_t, std::numeric_limits<std::uint8_t>::max() + 1> "
        "  cksum_table = {\n"
        "\n"
        "  ";

  for (std::uint32_t i = 0; i < table_size; ++i) {
    std::uint32_t crc = i << uint24_bits;
    for (int j = 0; j < uint8_bits; ++j) {
      if ((crc & bit31) != 0) {
        crc = (crc << 1) ^ poly;
      }
      else {
        crc <<= 1;
      }
    }
    of << "UINT32_C(0x" << std::hex << std::setw(uint8_bits) << std::setfill('0') << crc << "),";
    if (i == table_size - 1) {
      of << "\n";
    }
    else if ((i & 3) == 3) {
      of << "\n  ";
    }
    else {
      of << " ";
    }
  }

  of << "};\n"
        "\n"
        "/* clang-format on */\n"
        "\n"
        "} // namespace GD::Cksum\n";

  return EXIT_SUCCESS;
}
