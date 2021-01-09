/** \file  libgdsup/include/gd/format.hh
 *  \brief Expose Format library
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_FORMAT_HH_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_FORMAT_HH_INCLUDED

#if !defined(FORCE_SUPPLEMENTAL_LIBRARY) && __has_include(<format>)
#include <format>
namespace fmt = std;
#elif __has_include(<fmt/core.h>)
#include <fmt/core.h>
#endif // Pick a Format header.

#endif // _LIBGDSUP_INCLUDE_GD_FORMAT_HH_INCLUDED
