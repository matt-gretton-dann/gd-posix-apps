/** \file   src/asa/asa.cc
 *  \brief  Implement asa utility
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"

#include "gd/span.hh"
#include "gd/string.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "asa-messages.hh"

#include <cassert>
#include <clocale>
#include <fstream>
#include <iostream>
#include <string>
#include <string_view>
#include <system_error>

using Msg = GD::Asa::Msg;

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
            << GD::Asa::Messages::get().format(GD::Asa::Set::asa, msg, args...) << '\n'
            << GD::Asa::Messages::get().format(GD::Asa::Set::asa, usage, GD::program_name())
            << '\n';
  std::exit(1);  // NOLINT(concurrency-mt-unsafe)
}

class State
{
public:
  auto output(std::string_view input_file) -> bool
  {
    GD::StreamInputFile is(input_file);
    while (!is.eof() && !is.error()) {
      std::string line = is.getline();
      if (!line.empty()) {
        switch (line[0]) {
        case ' ':
          break;
        case '0':
          std::cout << '\n';
          break;
        case '1':
          std::cout << '\f';
          break;
        case '+':
          if (!first_input_) {
            std::cout << '\r';
          }
          break;
        default:
          break;
        }
        std::cout << line.substr(1);
        first_input_ = false;
      }
    }

    return !is.error();
  }

  void output_term() const
  {
    if (!first_input_) {
      std::cout << '\n';
    }
  }

private:
  bool first_input_{true};
};
}  // namespace

auto main(int argc, char** argv) -> int
try {
  // NOLINTNEXTLINE(concurrency-mt-unsafe)
  (void)std::setlocale(LC_ALL, "");
  GD::Span::span<char*> const args(argv, argc);
  GD::program_name(args[0]);

  State state;
  std::string fname;

  auto begin = args.begin() + 1;
  if (begin != args.end() && *begin != nullptr && ::strcmp(*begin, "--") == 0) {
    ++begin;
  }

  try {
    bool const success =
      GD::for_each_file(begin, args.end(), [&state, &fname](std::string_view file) {
        fname = file;
        return state.output(file);
      });
    state.output_term();
    return success ? EXIT_SUCCESS : EXIT_FAILURE;
  }
  catch (std::exception& e) {
    state.output_term();
    std::cerr << GD::program_name();
    if (!args.empty()) {
      std::cerr << ":" << fname;
    }
    std::cerr << ": " << e.what() << "\n";
    return EXIT_FAILURE;
  }
}
catch (std::exception& e) {
  error(Msg::uncaught_std_exception, e.what());
}
catch (...) {
  error(Msg::uncaught_exception);
}
