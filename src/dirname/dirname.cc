/** \file   src/dirname/dirname.cc
 *  \brief  Implement dirname utility
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "dirname-messages.hh"
#include "gd/libgen.h"
#include "gd/limits.h"
#include "gd/stdlib.h"
#include "gd/string.h"
#include "util/utils.hh"

#include <iostream>
#include <locale.h>
#include <stdio.h>

namespace {
template<typename... Ts>
[[noreturn]] void report_error(GD::Dirname::Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Dirname::Messages::get().format(GD::Dirname::Set::dirname, msg, args...) << '\n'
            << GD::Dirname::Messages::get().format(GD::Dirname::Set::dirname,
                                                   GD::Dirname::Msg::usage, GD::program_name())
            << '\n';
  ::exit(EXIT_FAILURE);
}

}  // namespace
int main(int argc, char** argv)
{
  ::setlocale(LC_ALL, "");
  GD::program_name(argv[0]);

  // Skip argv[0].
  ++argv;
  --argc;

  // Skip optional `--`.
  if (argc > 0 && argv[0][0] == '-' && argv[0][1] == '-' && argv[0][2] == '\0') {
    ++argv;
    --argc;
  }

  if (argc == 0) {
    report_error(GD::Dirname::Msg::missing_arguments);
  }
  if (argc > 1) {
    report_error(GD::Dirname::Msg::too_many_arguments);
  }

  char* bname = ::strdup(argv[0]);
  if (bname == 0) {
    report_error(GD::Dirname::Msg::out_of_memory);
  }
  bname = ::dirname(bname);
  ::size_t base_len = ::strlen(bname);

  while (base_len > INT_MAX) {
    ::printf("%.*s", INT_MAX, bname);
    bname += INT_MAX;
    base_len -= INT_MAX;
  }
  ::printf("%.*s\n", static_cast<int>(base_len), bname);
  return EXIT_SUCCESS;
}
