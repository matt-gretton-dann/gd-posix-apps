/**
 * \file      src/util/make-stdin-binary.cc
 * \brief     Make standard input a binary file.
 * \author    Matthew Gretton-Dann
 * \copyright 2021 Matthew Gretton-Dann
 *            SPDX-License-Identifier: Apache-2.0
 */
#include "util/file.hh"

#ifdef _WIN32
#  include <fcntl.h>
#  include <io.h>
#  include <stdio.h>
#endif

void GD::make_stdin_binary()
{
#ifdef _WIN32
  _setmode(_fileno(stdin), _O_BINARY);
#endif  // _WIN32
}
