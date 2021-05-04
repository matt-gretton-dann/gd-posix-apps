/** \file   bc.cc
 *  \brief  Main program for bc
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "bc.hh"

#include "gd/stdlib.h"
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "bc-messages.hh"

#include <assert.h>
#include <iostream>
#include <memory>
#include <variant>

using Msg = GD::Bc::Msg;

namespace {
/** \brief  Library script loaded with -l. */
char const* library_script = "";

/** \brief       Report an error and exit with exit code 1.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Bc::Messages::get().format(GD::Bc::Set::bc, msg, args...) << '\n'
            << GD::Bc::Messages::get().format(GD::Bc::Set::bc, Msg::usage, GD::program_name())
            << '\n';
  ::exit(1);
}

void execute(std::unique_ptr<GD::Bc::Reader>&& r)
{
  GD::Bc::VM vm(std::cout, std::clog);
  GD::Bc::Parser parser(std::make_unique<GD::Bc::Lexer>(std::move(r)), true);

  bool cont = true;
  while (cont) {
    auto instructions = parser.parse();
    if (instructions) {
      cont = vm.execute(*instructions);
    }
  }
}
}  // namespace

int main(int argc, char** argv)
{
  ::setlocale(LC_ALL, "");
  GD::program_name(argv[0]);

  int c;
  bool load_library = false;
  while ((c = ::getopt(argc, argv, ":l")) != -1) {
    switch (c) {
    case 'l':
      load_library = true;
      break;
    case ':':
    case '?':
    default:
      error(Msg::unrecognised_option, (char)optopt);
      break;
    }
  }

  if (load_library) {
    auto r = std::make_unique<GD::Bc::StringReader>(library_script);
    execute(std::move(r));
  }

  auto process = [](std::string_view fname) -> bool {
    auto r = std::make_unique<GD::Bc::FileReader>(fname);
    execute(std::move(r));
    return true;
  };

  bool success = GD::for_each_file(argc - optind, argv + optind, process, GD::FEFFlags::none);
  if (success) {
    auto r = std::make_unique<GD::Bc::FileReader>("-");
    execute(std::move(r));
  }

  return success ? 0 : 1;
}  // namespace
