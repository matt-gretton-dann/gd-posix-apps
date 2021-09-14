/** \file   src/util/input-file/input-file-getline.cc
 *  \brief  Handle getting a line
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/file.hh"

#include "util-messages.hh"

#include <cassert>
#include <cstdio>
#include <string>

auto GD::InputFile::getline() -> std::string
{
  assert(file_ != nullptr);  // NOLINT
  constexpr unsigned buf_size = 1024;
  std::array<char, buf_size> buf{};
  std::string result;
  while (true) {
    if (std::fgets(buf.data(), buf.size(), file_) == nullptr) {
      if (std::ferror(file_) != 0) {
        if (errno == EINTR || errno == EAGAIN) {
          std::clearerr(file_);
          continue;
        }
        report_error(Msg::file_read_error);
      }

      return result;
    }

    result += buf.data();
    if (result[result.size() - 1] == '\n') {
      return result.substr(0, result.size() - 1);
    }
  }
}
