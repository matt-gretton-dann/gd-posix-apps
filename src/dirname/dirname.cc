/** \file   src/dirname/dirname.cc
 *  \brief  Implement dirname utility
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"

#include "gd/limits.h"
#include "gd/span.hh"
#include "gd/stdlib.h"
#include "gd/string.h"

#include "util/utils.hh"

#include "dirname-messages.hh"

#include <clocale>
#include <cstdio>
#include <iostream>

namespace {
template<typename... Ts>
[[noreturn]] void report_error(GD::Dirname::Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Dirname::Messages::get().format(GD::Dirname::Set::dirname, msg, args...) << '\n'
            << GD::Dirname::Messages::get().format(GD::Dirname::Set::dirname,
                                                   GD::Dirname::Msg::usage, GD::program_name())
            << '\n';
  std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
}

}  // namespace
auto main(int argc, char** argv) -> int
try {
  (void)std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)
  GD::Span::span<char*> const args(argv, argc);
  GD::program_name(args[0]);

  // Skip argv[0].
  GD::Span::span<char*>::iterator it = args.begin() + 1;

  // Skip optional `--`.
  if (it != args.end() && std::strcmp(*it, "--") == 0) {
    ++it;
  }

  if (it == args.end()) {
    report_error(GD::Dirname::Msg::missing_arguments);
  }
  if (it + 1 != args.end()) {
    report_error(GD::Dirname::Msg::too_many_arguments);
  }

  std::string bname(*it);
  bname = ::dirname(bname.data());  // NOLINT(concurrency-mt-unsafe)
  std::cout << bname << '\n';
  return EXIT_SUCCESS;
}
catch (std::exception const& e) {
  report_error(GD::Dirname::Msg::unhandled_std_exception, e.what());
  return EXIT_FAILURE;
}
catch (...) {
  report_error(GD::Dirname::Msg::unhandled_exception);
  return EXIT_FAILURE;
}
