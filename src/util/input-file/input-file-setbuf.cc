/** \file   src/util/input-file/input-file-setbuf.cc
 *  \brief  InputFile::setbuf methods
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */
#include "util/file.hh"

#include <cassert>
#include <cstdio>
#include <memory>
#include <utility>
#include <vector>

void GD::StreamInputFile::setbuf()
{
  assert(file_ != nullptr);  // NOLINT
  std::setvbuf(file_, nullptr, _IONBF, 0);
}

void GD::StreamInputFile::setbuf(std::unique_ptr<std::vector<char>>&& ptr)
{
  assert(file_ != nullptr);  // NOLINT

  buffer_ = std::move(ptr);
  std::setvbuf(file_, buffer_->data(), _IOFBF, buffer_->size());
}

void GD::StreamInputFile::setbuf(Buffering type, std::unique_ptr<std::vector<char>>&& ptr)
{
  assert(file_ != nullptr);  // NOLINT

  buffer_ = std::move(ptr);
  std::setvbuf(file_, buffer_->data(), static_cast<int>(type), buffer_->size());
}
