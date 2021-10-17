/** \file   src/util/rename.cc
 *  \brief  Platform independent version of rename()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/file.hh"

#ifdef _WIN32
#  include "util/utils.hh"

#  include <Windows.h>
#  include <cerrno>
#  include <cstddef>

extern "C" void __support_log(char const* format, ...);  // NOLINT

auto GD::Util::rename(char const* _old, char const* _new) __NOEXCEPT -> int
{
  BOOL success = ::ReplaceFileA(_new, _old, nullptr, 0, nullptr, nullptr);
  if (success == 0) {
    errno = EINVAL;
    DWORD error = ::GetLastError();
    __support_log("ReplaceFileA(%s, %s) failed: %lx\n", _new, _old, error);
    return -1;
  }
  return 0;
}
#else
#  include <cstdio>
auto GD::Util::rename(char const* _old, char const* _new) __NOEXCEPT -> int
{
  return ::rename(_old, _new);
}
#endif
