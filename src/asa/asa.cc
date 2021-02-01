/** \file   src/asa/asa.cc
 *  \brief  Implement asa utility
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"
#include "gd/string.h"

#include <cassert>
#include <fstream>
#include <iostream>
#include <locale.h>
#include <string>
#include <string_view>
#include <system_error>

class InputStream
{
public:
  InputStream(std::string_view fname) : use_stdin_(fname == "-")
  {
    if (!use_stdin_) {
      is_.open(fname.data());
      if (!is_.is_open()) {
        throw std::system_error(std::make_error_code(static_cast<std::errc>(errno)));
      }
    }
  }

  InputStream(InputStream const&) = delete;
  InputStream& operator=(InputStream const&) = delete;
  InputStream(InputStream&&) = delete;
  InputStream& operator=(InputStream&&) = delete;

  std::istream& get() noexcept
  {
    if (use_stdin_) {
      return std::cin;
    }
    else {
      return is_;
    }
  }

private:
  bool use_stdin_;
  std::ifstream is_;
};

class State
{
public:
  State(std::string_view program_name) : program_name_(program_name), first_input_(true)
  {
    char* s = ::strdup(program_name.data());
    if (s != nullptr) {
      char* pn = ::strdup(::basename(s));
      if (pn != nullptr) {
        program_name_ = pn;
        ::free(pn);
      }
      ::free(s);
    }
  }

  std::string_view program_name() const noexcept { return program_name_; }

  void output(std::string_view input_file)
  {
    InputStream is(input_file);
    std::string line;
    while (std::getline(is.get(), line)) {
      if (line.size() >= 1) {
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
      }
      first_input_ = false;
    }
  }

  void output_term() const
  {
    if (!first_input_) {
      std::cout << '\n';
    }
  }

private:
  std::string program_name_;
  bool first_input_;
};

template<typename F>
void for_each_file(int argc, char** argv, F apply_fn)
{
  if (argc == 0) {
    apply_fn("-");
  }
  else {
    while (argc > 0) {
      assert(*argv != nullptr);
      apply_fn(*argv);
      ++argv;
      --argc;
    }
  }

  assert(argc == 0);
  assert(*argv == nullptr);
}

int main(int argc, char** argv)
{
  ::setlocale(LC_ALL, "");
  State state(argv[0]);
  std::string fname;

  if (argc > 1 && argv[1][0] == '-' && argv[1][1] == '-' && argv[1][2] == '\0') {
    ++argv;
    --argc;
  }

  try {
    for_each_file(argc - 1, argv + 1, [&state, &fname](std::string_view file) {
      fname = file;
      state.output(file);
    });
    state.output_term();
    return EXIT_SUCCESS;
  }
  catch (std::exception& e) {
    state.output_term();
    std::cerr << state.program_name();
    if (*argv != NULL) {
      std::cerr << ":" << fname;
    }
    std::cerr << ": " << e.what() << "\n";
    return EXIT_FAILURE;
  }
}
