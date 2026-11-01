/** \file   src/util/input-file/input-file-getc.cc
 *  \brief  Handle getting a character.
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/file.hh"

#include "util-messages.hh"

#include <cassert>
#include <cstdio>

template<bool Locked>
auto GD::StreamInputFile::getc_impl() -> int
{
  assert(file_ != nullptr);  // NOLINT
  auto next = [this] {
    if constexpr (Locked) {
      return std::fgetc(file_);
    }
    else {
#if defined(_WIN32) && defined(_MSC_VER)
      return _fgetc_nolock(file_);
#elif defined(_POSIX_THREAD_SAFE_FUNCTIONS) && _POSIX_THREAD_SAFE_FUNCTIONS > 0
      // POSIX permits a macro: keep the FILE argument free of side effects.
      return getc_unlocked(file_);
#else
      return std::fgetc(file_);
#endif
    }
  };
  int c = 0;
  while ((c = next()) == EOF) {
    if (std::feof(file_) != 0) {
      return EOF;
    }

    assert(std::ferror(file_));  // NOLINT
    if (errno == EINTR || errno == EAGAIN) {
      std::clearerr(file_);
    }
    else {
      report_error(Msg::file_read_error);
      return EOF;
    }
  }

  return c;
}

auto GD::StreamInputFile::getc() -> int { return getc_impl<true>(); }

auto GD::StreamInputFile::getc_exclusive() -> int
{
  // stdin is borrowed and can be shared by otherwise independent readers.
  return is_stdin_ ? getc_impl<true>() : getc_impl<false>();
}
