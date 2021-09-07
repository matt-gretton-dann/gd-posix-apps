/** \file   src/util/input-file/input-file-queries.cc
 *  \brief  Basic queries for InputFile
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include <stdio.h>

#include <string_view>
#include <util/file.hh>

auto GD::InputFile::error() const -> bool { return file_ == nullptr || ::ferror(file_); }
auto GD::InputFile::eof() const -> bool { return file_ == nullptr || ::feof(file_); }

auto GD::InputFile::filename() const -> std::string_view { return filename_; }
