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

#include <iostream>
#include <locale.h>
#include <stdio.h>

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
    report_error(GD::Basename::Msg::missing_arguments);
  }
  if (argc > 2) {
    report_error(GD::Basename::Msg::too_many_arguments);
  }

  char* bname = ::strdup(argv[0]);
  if (bname == 0) {
    report_error(GD::Basename::Msg::out_of_memory);
  }
  bname = ::basename(bname);
  ::size_t base_len = ::strlen(bname);

  if (argc == 2) {
    ::size_t suffix_len = ::strlen(argv[1]);
    if (base_len > suffix_len && ::strcmp(bname + base_len - suffix_len, argv[1]) == 0) {
      base_len -= suffix_len;
    }
  }

  while (base_len > INT_MAX) {
    ::printf("%.*s", INT_MAX, bname);
    bname += INT_MAX;
    base_len -= INT_MAX;
  }
  ::printf("%.*s\n", static_cast<int>(base_len), bname);
  return EXIT_SUCCESS;
}
