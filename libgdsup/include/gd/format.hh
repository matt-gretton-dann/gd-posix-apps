/** \file  libgdsup/include/gd/format.hh
 *  \brief Expose Format library
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef GD_FORMAT_HH
#define GD_FORMAT_HH

#if !defined(FORCE_SUPPLEMENTAL_LIBRARY) && __has_include(<format>)
#  include <format>
namespace fmt = std;
#elif __has_include(<fmt/format.h>)
#  include <fmt/format.h>
#endif  // Pick a Format header.

#endif  // GD_FORMAT_HH
