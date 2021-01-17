/** \file  libgdsup/include/gd/bits/defines.h
 *  \brief Internal defines for gdsup library
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED

#ifdef __cplusplus
#  define EXTERN_C extern "C"
#  define EXTERN_C_BEGIN extern "C" {
#  define EXTERN_C_END }
#else
#  define EXTERN_C
#  define EXTERN_C_BEGIN
#  define EXTERN_C_END
#endif

#ifdef _WIN32
#  ifndef __BYTE_ORDER__
#    define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__
#  endif
#  ifndef __ORDER_LITTLE_ENDIAN__
#    define __ORDER_LITTLE_ENDIAN__ 1
#  endif
#  ifndef __ORDER_BIG_ENDIAN__
#    define __ORDER_BIG_ENDIAN__ 0
#  endif
#endif

#endif  // _LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED
