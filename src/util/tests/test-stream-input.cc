/** \file test-stream-input.cc
 * Character input compatibility for privately owned stdio streams.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "util/file.hh"

#include <new>

#include <catch2/catch.hpp>

#include <chrono>
#include <fstream>
#include <stdexcept>
#include <string>
#include <system_error>

namespace {
struct TemporaryInput
{
  fs::path directory;
  TemporaryInput()
  {
    auto const stamp = std::chrono::steady_clock::now().time_since_epoch().count();
    for (unsigned attempt = 0; attempt < 32; ++attempt) {
      auto path = fs::temp_directory_path() /
                  ("gd-stream-" + std::to_string(stamp) + "-" + std::to_string(attempt));
      if (fs::create_directory(path)) {
        directory = std::move(path);
        return;
      }
    }
    throw std::runtime_error("cannot create exclusive stream test directory");
  }
  ~TemporaryInput()
  {
    std::error_code ignored;
    fs::remove_all(directory, ignored);
  }
  auto file() const -> fs::path { return directory / "input.bin"; }
};
}  // namespace

TEST_CASE("Exclusive and ordinary reads share binary stream state", "[util][file][input]")
{
  TemporaryInput temporary;
  {
    std::ofstream output(temporary.file(), std::ios::binary);
    for (unsigned i = 0; i < 8192; ++i)
      output.put(static_cast<char>(i % 256));
    REQUIRE(output.good());
  }
  GD::StreamInputFile input(temporary.file().string(), "rb");
  REQUIRE_FALSE(input.error());
  for (unsigned i = 0; i < 8192; ++i) {
    auto const actual = (i % 2 == 0) ? input.getc_exclusive() : input.getc();
    REQUIRE(actual == static_cast<int>(i % 256));
  }
  CHECK(input.getc_exclusive() == EOF);
  CHECK(input.getc() == EOF);
  CHECK(input.eof());
  CHECK_FALSE(input.error());
}

TEST_CASE("Exclusive reads interleave with line input", "[util][file][input]")
{
  TemporaryInput temporary;
  {
    std::ofstream output(temporary.file(), std::ios::binary);
    output << "ab\ncd\nef\n";
    REQUIRE(output.good());
  }
  GD::StreamInputFile input(temporary.file().string());
  REQUIRE_FALSE(input.error());
  CHECK(input.getc_exclusive() == 'a');
  CHECK(input.getc() == 'b');
  CHECK(input.getline().empty());
  CHECK(input.getline() == "cd");
  CHECK(input.getc_exclusive() == 'e');
  CHECK(input.getc() == 'f');
  CHECK(input.getc_exclusive() == '\n');
  CHECK(input.getc_exclusive() == EOF);
  CHECK(input.eof());
  CHECK_FALSE(input.error());
}
