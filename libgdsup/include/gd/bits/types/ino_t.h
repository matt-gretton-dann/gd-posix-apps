/** \file   libgdsup/include/bits/types/ino_t.h
 *  \brief  Define the pid_t type.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_BITS_TYPES_INO_T_H_INCLUDED
#define _LIBGDSUP_INCLUDE_BITS_TYPES_INO_T_H_INCLUDED

#ifdef _WIN32
typedef unsigned short ino_t;
#endif

#endif  // _LIBGDSUP_INCLUDE_BITS_TYPES_INO_T_H_INCLUDED
