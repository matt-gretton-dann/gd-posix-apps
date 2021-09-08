/** \file   src/basename/basename.cc
 *  \brief  Implement basename utility
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"

#include "gd/limits.h"
#include "gd/stdlib.h"
#include "gd/string.h"

#include "util/utils.hh"

#include "basename-messages.hh"

#include <clocale>
#include <cstdio>
#include <iostream>
#include <span>

namespace {
template<typename... Ts>
[[noreturn]] void report_error(GD::Basename::Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Basename::Messages::get().format(GD::Basename::Set::basename, msg, args...)
            << '\n'
            << GD::Basename::Messages::get().format(GD::Basename::Set::basename,
                                                    GD::Basename::Msg::usage, GD::program_name())
            << '\n';
  std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
}

}  // namespace

auto main(int argc, char** argv) -> int
{
  std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)
  std::span<char*> args(argv, argc);
  GD::program_name(args[0]);

  auto begin = args.begin() + 1;

  // Skip optional `--`.
  if (begin != args.end() && std::strcmp(*begin, "--") == 0) {
    ++begin;
  }

  if (begin == args.end()) {
    report_error(GD::Basename::Msg::missing_arguments);
  }
  if (std::distance(begin, args.end()) > 2) {
    report_error(GD::Basename::Msg::too_many_arguments);
  }

  std::string bname(*begin);
  bname = ::basename(bname.data());  // NOLINT(concurrency-mt-unsafe)

  ++begin;
  if (begin != args.end()) {
    std::string_view suffix(*begin);
    if (bname.ends_with(suffix)) {
      bname.resize(bname.size() - suffix.size());
    }
  }

  std::cout << bname << '\n';
  return EXIT_SUCCESS;
}
