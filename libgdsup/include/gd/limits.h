/** \file   libgdsup/include/gd/limits.hh
 *  \brief  Expose limits.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_LIMITS_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_LIMITS_H_INCLUDED

// Everyone has a limits.h.  */
#include <limits.h>

#if defined(FORCE_SUPPLEMENTAL_LIBRARY) || !defined(_POSIX2_LINE_MAX)
#  undef _POSIX2_LINE_MAX
/** \brief Maximum acceptable length, in bytes, of a line of input from a text file.  */
#  define _POSIX2_LINE_MAX 2048
#endif

#if defined(FORCE_SUPPLEMENTAL_LIBRARY) || !defined(NL_MSGMAX)
#  undef NL_MSGMAX
/** \brief Maximum message number in message catalogues.  */
#  define NL_MSGMAX INT32_MAX
#  if NL_MSGMAX < 32767
#    error "NL_MSGMAX has been set to too low a value."
#  endif
#endif  // NL_MSGMAX

#if defined(FORCE_SUPPLEMENTAL_LIBRARY) || !defined(NL_SETMAX)
#  undef NL_SETMAX
/** \brief Maximum set number in message catalogues.  */
#  define NL_SETMAX INT32_MAX
#  if NL_SETMAX < 255
#    error "NL_SETMAX has been set to too low a value."
#  endif
#endif  // NL_SETMAX

#if defined(FORCE_SUPPLEMENTAL_LIBRARY) || !defined(NL_TEXTMAX)
#  undef NL_TEXTMAX
/** \brief Maximum length of message in message catalogue.  */
#  define NL_TEXTMAX INT32_MAX
#  if NL_TEXTMAX < _POSIX2_LINE_MAX
#    error "NL_TEXTMAX has been set to too low a value."
#  endif
#endif  // NL_TEXTMAX

#endif  // _LIBGDSUP_INCLUDE_GD_LIMITS_H_INCLUDED
