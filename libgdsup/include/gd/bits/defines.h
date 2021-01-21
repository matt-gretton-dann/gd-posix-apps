/** \file  libgdsup/include/gd/bits/defines.h
 *  \brief Internal defines for gdsup library
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED

/* Extern "C" blocks. */
#ifdef __cplusplus
#  define __EXTERN_C extern "C"
#  define __EXTERN_C_BEGIN extern "C" {
#  define __EXTERN_C_END }
#else
#  define __EXTERN_C
#  define __EXTERN_C_BEGIN
#  define __EXTERN_C_END
#endif

/* Byte order on Windows.  */
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

/** \macro __PATH_SEP
 *  \brief Separator character for paths in environment variables. (':' or ';')
 */

/** \macro __PATH_SEPSTR
 *  \brief Separator character as a string for paths in environment variables. (":" or ";")
 */

/** \macro __DIR_SEP
 *  \brief Separator character for directories. ('/' or '\')
 */

/** \macro __DIR_SEPSTR
 *  \brief Separator character as a string for directories. ("/" or "\")
 */

#ifdef _WIN32
#  define __PATH_SEP ';'
#  define __PATH_SEPSTR ";"
#  define __DIR_SEP '\\'
#  define __DIR_SEPSTR "\\"
#else
#  define __PATH_SEP ':'
#  define __PATH_SEPSTR ":"
#  define __DIR_SEP '/'
#  define __DIR_SEPSTR "/"
#endif

/* Disable various Windows warnings about function usage.
 * Ideally we wouldn't turn off the _CRT_SECURE_NO_WARNINGS
 */
#if _WIN32
#  define _CRT_DECLARE_NONSTDC_NAMES 0
#  define _CRT_SECURE_NO_WARNINGS 1
#  define __STDC_WANT_SECURE_LIB__ 1
#endif

#ifdef _MSC_VER
/* Disable warnings:
 *  4820: Inserting padding.
 *  5045: Adding QSpectre  */
#  pragma warning(disable : 4820 5045)
#endif /* _MSC_VER */

#ifdef _MSC_VER
#  define __SUPPRESS_CONSTANT_CONDITIONAL _Pragma("warning(suppress: 4127)")
#else
#  define __SUPPRESS_CONSTANT_CONDITIONAL
#endif

#endif  // _LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED
