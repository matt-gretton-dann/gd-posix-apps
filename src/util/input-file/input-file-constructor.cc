/** \file  src/util/input-file/input-file-constructor.cc
 *  \brief Constructors for InputFile
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */
#include "util-messages.hh"
#include "util/file.hh"

#include <stdio.h>

GD::InputFile::InputFile(std::string_view filename, std::string_view mode)
    : filename_(filename), file_(nullptr), is_stdin_(false)
{
  if (filename_ == "-") {
    filename_ = GD::Util::Messages::get().get(GD::Util::Set::util, Msg::stdin_name);
    is_stdin_ = true;
    file_ = stdin;
  }
  else {
    file_ = ::fopen(filename_.data(), mode.data());
    if (file_ == nullptr) {
      report_error(Msg::file_open_error);
      return;
    }
  }
}

GD::InputFile::~InputFile()
{
  if (!is_stdin_ && file_ != nullptr) {
    ::fclose(file_);
  }
}
