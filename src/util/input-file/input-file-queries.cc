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

std::string const& GD::InputFile::filename() const { return filename_; }

bool GD::InputFile::is_stdin() const noexcept { return is_stdin_; }

struct timespec GD::InputFile::access_time() const noexcept
{
  assert(stat_.has_value());
  return stat_->st_atim;
}

struct timespec GD::InputFile::modification_time() const noexcept
{
  assert(stat_.has_value());
  return stat_->st_mtim;
}

mode_t GD::InputFile::mode() const noexcept
{
  assert(stat_.has_value());
  return stat_->st_mode;
}

uid_t GD::InputFile::owner() const noexcept
{
  assert(stat_.has_value());
#ifdef _WIN32
  return -1;
#else
  return stat_->st_uid;
#endif
}

gid_t GD::InputFile::group() const noexcept
{
  assert(stat_.has_value());
#ifdef _WIN32
  return -1;
#else
  return stat_->st_gid;
#endif
}
