/** \file   src/util/input-file/input-file-queries.cc
 *  \brief  Basic queries for InputFile
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include <stdio.h>

#include <string_view>
#include <util/file.hh>

bool GD::InputFile::error() const { return file_ == nullptr || ::ferror(file_); }
bool GD::InputFile::eof() const { return file_ == nullptr || ::feof(file_); }

std::string_view GD::InputFile::filename() const { return filename_; }
