/** \file   src/util/input-file/input-file-getc.cc
 *  \brief  Handle getting a character.
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/file.hh"

#include "util-messages.hh"

#include <assert.h>
#include <stdio.h>

int GD::StreamInputFile::getc()
{
  assert(file_ != nullptr);
  int c;
  while ((c = fgetc(file_)) == EOF) {
    if (feof(file_)) {
      return EOF;
    }
    else {
      assert(ferror(file_));
      if (errno == EINTR || errno == EAGAIN) {
        ::clearerr(file_);
      }
      else {
        report_error(Msg::file_read_error);
        return EOF;
      }
    }
  }

  return c;
}
