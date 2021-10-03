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

#include <cassert>
#include <clocale>
#include <fstream>
#include <iostream>
#include <string>

#include <string_view>
#include <system_error>

class State
{
public:
  auto output(std::string_view input_file) -> bool
  {
    GD::InputFile is(input_file);
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

auto main(int argc, char** argv) -> int
{
  // NOLINTNEXTLINE(concurrency-mt-unsafe)
  std::setlocale(LC_ALL, "");
  GD::Std::span<char*> args(argv, argc);
  GD::program_name(args[0]);

  State state;
  std::string fname;

  auto begin = args.begin() + 1;
  if (begin != args.end() && *begin != nullptr && ::strcmp(*begin, "--") == 0) {
    ++begin;
  }

  try {
    bool success = GD::for_each_file(begin, args.end(), [&state, &fname](std::string_view file) {
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
