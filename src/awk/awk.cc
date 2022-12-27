/** \file   awk.cc
 *  \brief  Main program for awk
 *  \author Copyright 2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "awk.hh"

#include "gd/stdlib.h"
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "awk-messages.hh"

#include <cassert>
#include <clocale>
#include <iostream>
#include <memory>
#include <variant>

using Msg = GD::Awk::Msg;

namespace {
/** \brief       Report an error and exit with exit code 1.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
  constexpr auto usage = Msg::usage;
  std::cerr << GD::program_name() << ": "
            << GD::Awk::Messages::get().format(GD::Awk::Set::awk, msg, args...) << '\n'
            << GD::Awk::Messages::get().format(GD::Awk::Set::awk, usage, GD::program_name())
            << '\n';
  std::exit(1);  // NOLINT(concurrency-mt-unsafe)
}

[[maybe_unused]] void parse(std::unique_ptr<GD::Awk::Reader>&& r)
{
  auto lexer{std::make_unique<GD::Awk::Lexer>(std::move(r))};
  // GD::Bc::Parser parser(std::make_unique<GD::Bc::Lexer>(std::move(r)), true);
}
}  // namespace

auto main(int argc, char** argv) -> int
try {
  (void)std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)
  GD::Span::span<char*> const args(argv, argc);
  GD::program_name(args[0]);

  int c = 0;
  std::vector<std::string> variable_assignments;
  std::vector<std::string> files;
  while ((c = ::getopt(argc, argv, ":F:f:v:")) != -1) {  // NOLINT(concurrency-mt-unsafe)
    switch (c) {
    case 'F': {
      std::string fs_var_assign{"FS="};
      fs_var_assign += optarg;
      variable_assignments.push_back(fs_var_assign);
      break;
    }
    case 'f':
      files.emplace_back(optarg);
      break;
    case 'v':
      variable_assignments.emplace_back(optarg);
      break;
    case ':':
    case '?':
    default:
      error(Msg::unrecognised_option, static_cast<char>(optopt));
      break;
    }
  }

  return EXIT_SUCCESS;
}  // namespace
catch (std::exception& e) {
  error(Msg::uncaught_std_exception, e.what());
}
catch (...) {
  error(Msg::uncaught_exception);
}
