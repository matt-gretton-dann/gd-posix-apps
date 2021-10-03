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

#include <cassert>
#include <clocale>
#include <iostream>
#include <memory>
#include <variant>

#include "find-multiply-split-point.hh"

using Msg = GD::Bc::Msg;

namespace {
/** \brief  Library script loaded with -l. */
char const* const library_script = R"EOF(
scale=20
)EOF";

/** \brief       Report an error and exit with exit code 1.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
#if ENABLE_EXTENSIONS
  constexpr auto usage = Msg::usage_extensions;
#else
  constexpr auto usage = Msg::usage;
#endif
  std::cerr << GD::program_name() << ": "
            << GD::Bc::Messages::get().format(GD::Bc::Set::bc, msg, args...) << '\n'
            << GD::Bc::Messages::get().format(GD::Bc::Set::bc, usage, GD::program_name()) << '\n';
  std::exit(1);  // NOLINT(concurrency-mt-unsafe)
}

void execute(GD::Bc::VM& vm, std::unique_ptr<GD::Bc::Reader>&& r)
{
  GD::Bc::Parser parser(std::make_unique<GD::Bc::Lexer>(std::move(r)), true);

  bool cont = true;
  while (cont) {
    auto instructions = parser.parse();
    if (instructions) {
      cont = parser.seen_quit();
      cont |= vm.execute(*instructions);
    }
    else {
      cont = false;
    }
  }
}
}  // namespace

auto main(int argc, char** argv) -> int
{
  std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)
  GD::Std::span<char*> args(argv, argc);
  GD::program_name(args[0]);

  int c = 0;
  bool load_library = false;
  bool save_specials = false;
#if ENABLE_EXTENSIONS
  char const* opts = ":glq";
#else
  char const* opts = ":l";
#endif
  while ((c = ::getopt(argc, argv, opts)) != -1) {  // NOLINT(concurrency-mt-unsafe)
    switch (c) {
    case 'g':
      save_specials = true;
      break;
    case 'l':
      load_library = true;
      break;
    case 'q':
      break;
    case ':':
    case '?':
    default:
      error(Msg::unrecognised_option, static_cast<char>(optopt));
      break;
    }
  }

  GD::Bc::VM vm(std::cout, std::clog, save_specials);

  /* Configure where we do multiplication splits.  */
  GD::Bc::Number::multiply_split_point(BC_MULTIPLY_SPLIT_POINT);

  if (load_library) {
    auto r = std::make_unique<GD::Bc::StringReader>(library_script);
    execute(vm, std::move(r));
  }

  auto process = [&vm](std::string_view fname) -> bool {
    auto r = std::make_unique<GD::Bc::FileReader>(fname);
    execute(vm, std::move(r));
    return true;
  };

  bool success = GD::for_each_file(args.begin() + optind, args.end(), process, GD::FEFFlags::none);
  if (success) {
    auto r = std::make_unique<GD::Bc::FileReader>("-");
    execute(vm, std::move(r));
  }

  return success ? 0 : 1;
}  // namespace
