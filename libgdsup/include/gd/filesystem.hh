/** \file  libgdsup/include/gd/filesystem.hh
 *  \brief Expose Filesystem library
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBGDSUP_INCLUDE_GD_FILESYSTEM_HH_INCLUDED
#define LIBGDSUP_INCLUDE_GD_FILESYSTEM_HH_INCLUDED

#if __has_include(<filesystem>)
#  include <filesystem>
namespace fs = std::filesystem;
#elif __has_include(<experimental/filesystem>)
#  include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
#else
#  error "Unable to locate an appropriate filesystem library"
#endif  // Pick a filesystem header.

#endif  // LIBGDSUP_INCLUDE_GD_FILESYSTEM_HH_INCLUDED
