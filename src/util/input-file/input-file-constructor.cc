/** \file  src/util/input-file/input-file-constructor.cc
 *  \brief Constructors for InputFile
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */
#include "gd/sys/stat.h"

#include "util/file.hh"

#include "util-messages.hh"

#include <stdio.h>

GD::InputFile::InputFile(std::string_view filename, std::string_view mode)
    : filename_(filename), file_(nullptr), is_stdin_(false), buffer_(nullptr)
{
  if (filename_ == "-") {
    filename_ = Util::Messages::get().get(Util::Set::util, Msg::stdin_name);
    is_stdin_ = true;
    file_ = stdin;
    /* If we've been asked to open a file in binary mode ensure that standard input is indeed in
     * binary mode.  */
    if (mode.find('b') != std::string_view::npos) {
      make_stdin_binary();
    }
  }
  else {
    file_ = ::fopen(filename_.data(), mode.data());
    if (file_ == nullptr) {
      report_error(Msg::file_open_error);
      return;
    }
    stat_.emplace();
    ::fstat(::fileno(file_), &stat_.value());
  }
}

GD::InputFile::~InputFile()
{
  if (!is_stdin_ && file_ != nullptr) {
    ::fclose(file_);
  }
}
