/** \file   src/util/input-file/input-file-getc.cc
 *  \brief  Handle getting a character.
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/file.hh"

#include "util-messages.hh"

#include <cassert>
#include <cstdio>

auto GD::StreamInputFile::getc() -> int
{
  assert(file_ != nullptr);  // NOLINT
  int c = 0;
  while ((c = std::fgetc(file_)) == EOF) {
    if (std::feof(file_) != 0) {
      return EOF;
    }

    assert(std::ferror(file_));  // NOLINT
    if (errno == EINTR || errno == EAGAIN) {
      std::clearerr(file_);
    }
    else {
      report_error(Msg::file_read_error);
      return EOF;
    }
  }

  return c;
}
