/** \file   src/util/input-file/input-file-getline.cc
 *  \brief  Handle getting a line
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/file.hh"

#include "util-messages.hh"

#include <assert.h>
#include <stdio.h>
#include <string>

std::string GD::InputFile::getline()
{
  assert(file_ != nullptr);
  char buf[1024];
  std::string result;
  while (true) {
    if (::fgets(buf, 1024, file_) == nullptr) {
      if (ferror(file_)) {
        if (errno == EINTR || errno == EAGAIN) {
          ::clearerr(file_);
          continue;
        }
        else {
          report_error(Msg::file_read_error);
        }
      }

      return result;
    }

    result += buf;
    if (result[result.size() - 1] == '\n') {
      return result.substr(0, result.size() - 1);
    }
  }
}
