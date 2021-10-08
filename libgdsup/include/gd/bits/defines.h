/** \file   libgdsup/include/gd/bits/defines.h
 *  \brief  Internal defines for gdsup library
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED
#define LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED

/* Extern "C" blocks. */
#ifdef __cplusplus
#  define __EXTERN_C extern "C"          // NOLINT
#  define __EXTERN_C_BEGIN extern "C" {  // NOLINT
#  define __EXTERN_C_END }               // NOLINT
#  define __RESTRICT                     // NOLINT
#  if defined(__APPLE__) || !defined(FORCE_SUPPLEMENTAL_LIBRARY)
#    define __NOEXCEPT  // NOLINT
#  else
#    define __NOEXCEPT noexcept  // NOLINT
#  endif
#else
#  define __EXTERN_C           // NOLINT
#  define __EXTERN_C_BEGIN     // NOLINT
#  define __EXTERN_C_END       // NOLINT
#  define __RESTRICT restrict  // NOLINT
#  define __NOEXCEPT           // NOLINT
#endif

/* Byte order on Windows.  */
#ifdef _WIN32
#  ifndef __BYTE_ORDER__
#    define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__  // NOLINT
#  endif
#  ifndef __ORDER_LITTLE_ENDIAN__
#    define __ORDER_LITTLE_ENDIAN__ 1  // NOLINT
#  endif
#  ifndef __ORDER_BIG_ENDIAN__
#    define __ORDER_BIG_ENDIAN__ 0  // NOLINT
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
#  define __PATH_SEP ';'     // NOLINT
#  define __PATH_SEPSTR ";"  // NOLINT
#  define __DIR_SEP '\\'     // NOLINT
#  define __DIR_SEPSTR "\\"  // NOLINT
#else
#  define __PATH_SEP ':'     // NOLINT
#  define __PATH_SEPSTR ":"  // NOLINT
#  define __DIR_SEP '/'      // NOLINT
#  define __DIR_SEPSTR "/"   // NOLINT
#endif

/* Disable various Windows warnings about function usage.
 * Ideally we wouldn't turn off the _CRT_SECURE_NO_WARNINGS
 */
#if _WIN32
#  define _CRT_DECLARE_NONSTDC_NAMES 0  // NOLINT
#  define _CRT_SECURE_NO_WARNINGS 1     // NOLINT
#  define __STDC_WANT_SECURE_LIB__ 1    // NOLINT
#endif

#ifdef _MSC_VER
/* Disable warnings:
 *  4820: Inserting padding.
 *  5045: Adding QSpectre  */
#  pragma warning(disable : 4820 5045)
#endif /* _MSC_VER */

#ifdef _MSC_VER
#  define __SUPPRESS_CONSTANT_CONDITIONAL _Pragma("warning(suppress: 4127)")  // NOLINT
#  define __DISABLE_NARROWING_WARNING _Pragma("warning(disable: 4267)")       // NOLINT
#else
#  define __SUPPRESS_CONSTANT_CONDITIONAL  // NOLINT
#  define __DISABLE_NARROWING_WARNING      // NOLINT
#endif

#endif  // LIBGDSUP_INCLUDE_GD_BITS_DEFINES_H_INCLUDED
