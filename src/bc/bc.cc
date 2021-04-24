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

class State
{
public:
  void execute(std::unique_ptr<GD::Bc::Reader>&& r)
  {
    GD::Bc::Lexer l(std::move(r));
    bool cont = true;
    do {
      GD::Bc::Token const& t = l.peek();
      t.debug(std::cout);
      std::cout << "\n";
      cont = (t.type() != GD::Bc::Token::Type::eof);
      l.chew();
    } while (cont);
  }
};
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

  State state;
  if (load_library) {
    auto r = std::make_unique<GD::Bc::StringReader>(library_script);
    state.execute(std::move(r));
  }

  auto process = [&state](std::string_view fname) -> bool {
    auto r = std::make_unique<GD::Bc::FileReader>(fname);
    state.execute(std::move(r));
    return true;
  };

  bool success = GD::for_each_file(argc - optind, argv + optind, process, GD::FEFFlags::none);
  if (success) {
    auto r = std::make_unique<GD::Bc::FileReader>("-");
    state.execute(std::move(r));
  }

  return success ? 0 : 1;
}  // namespace
