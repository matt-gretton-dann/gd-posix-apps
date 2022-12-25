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

namespace {
auto setvbuf_impl(std::FILE* stream, char* buffer, int type, std::size_t size)
{
  int saved_errno{errno};
  errno = 0;
  if (std::setvbuf(stream, buffer, type, size) != 0) {
    std::swap(errno, saved_errno);
    if (saved_errno != 0) {
      throw std::system_error(saved_errno, std::generic_category(),
                              "Unable to set file buffering.");
    }

    throw std::runtime_error("Unable to set file buffering.");
  }
}
}  // namespace

void GD::StreamInputFile::setbuf()
{
  assert(file_ != nullptr);  // NOLINT

  setvbuf_impl(file_, nullptr, _IONBF, 0);
}

void GD::StreamInputFile::setbuf(std::unique_ptr<std::vector<char>>&& ptr)
{
  assert(file_ != nullptr);  // NOLINT

  buffer_ = std::move(ptr);
  setvbuf_impl(file_, buffer_->data(), _IOFBF, buffer_->size());
}

void GD::StreamInputFile::setbuf(Buffering type, std::unique_ptr<std::vector<char>>&& ptr)
{
  assert(file_ != nullptr);  // NOLINT

  buffer_ = std::move(ptr);
  setvbuf_impl(file_, buffer_->data(), static_cast<int>(type), buffer_->size());
}
