/**
 * \file      src/util/make-stdout-binary.cc
 * \brief     Make standard output a binary file.
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

void GD::make_stdout_binary()
{
#ifdef _WIN32
  _setmode(_fileno(stdout), _O_BINARY);
#endif  // _WIN32
}
