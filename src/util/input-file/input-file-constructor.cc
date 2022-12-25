/** \file  src/util/input-file/input-file-constructor.cc
 *  \brief Constructors for StreamInputFile
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */
#include "util/file.hh"

#include "util-messages.hh"

#include <cstdio>

// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
GD::StreamInputFile::StreamInputFile(std::string_view filename, std::string_view mode)
    : filename_(filename), buffer_(nullptr)
{
  if (filename_ == "-") {
    filename_ = GD::Util::Messages::get().get(GD::Util::Set::util, Msg::stdin_name);
    is_stdin_ = true;
    file_ = stdin;
  }
  else {
    file_ = std::fopen(filename_.data(), mode.data());
    if (file_ == nullptr) {
      report_error(Msg::file_open_error);
      return;
    }
  }
}

GD::StreamInputFile::~StreamInputFile()
{
  if (!is_stdin_ && file_ != nullptr) {
    // Too late to do anything if this goes wrong now.
    (void)std::fclose(file_);
  }
}
