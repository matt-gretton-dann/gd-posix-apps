/** \file   src/util/input-file/input-file-queries.cc
 *  \brief  Basic queries for InputFile
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include <cstdio>
#include <string_view>

#include <util/file.hh>

auto GD::StreamInputFile::error() const -> bool
{
  return file_ == nullptr || (std::ferror(file_) != 0);
}

auto GD::StreamInputFile::eof() const -> bool
{
  return file_ == nullptr || (std::feof(file_) != 0);
}

auto GD::StreamInputFile::filename() const -> std::string_view { return filename_; }
