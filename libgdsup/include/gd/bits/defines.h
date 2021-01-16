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

#endif  // _LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED
