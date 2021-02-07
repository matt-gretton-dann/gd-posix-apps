/** \file   src/util/input-file/input-file-setbuf.cc
 *  \brief  InputFile::setbuf methods
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */
#include "util/file.hh"

#include <assert.h>
#include <memory>
#include <stdio.h>
#include <utility>
#include <vector>

void GD::InputFile::setbuf()
{
  assert(file_ != nullptr);
  ::setvbuf(file_, nullptr, _IONBF, 0);
}

void GD::InputFile::setbuf(std::unique_ptr<std::vector<char>>&& ptr)
{
  assert(file_ != nullptr);

  buffer_ = std::move(ptr);
  ::setvbuf(file_, buffer_->data(), _IOFBF, buffer_->size());
}

void GD::InputFile::setbuf(Buffering type, std::unique_ptr<std::vector<char>>&& ptr)
{
  assert(file_ != nullptr);

  buffer_ = std::move(ptr);
  ::setvbuf(file_, buffer_->data(), static_cast<int>(type), buffer_->size());
}
