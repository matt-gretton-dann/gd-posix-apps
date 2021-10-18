/** \file   libgdsup/include/bits/types/nlink_t.h
 *  \brief  Define the pid_t type.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_BITS_TYPES_NLINK_T_H_INCLUDED
#define _LIBGDSUP_INCLUDE_BITS_TYPES_NLINK_T_H_INCLUDED

#ifdef _WIN32
typedef unsigned int nlink_t;
#endif

#endif  // _LIBGDSUP_INCLUDE_BITS_TYPES_NLINK_T_H_INCLUDED
