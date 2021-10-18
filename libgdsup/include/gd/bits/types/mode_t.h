/** \file   libgdsup/include/bits/types/mode_t.h
 *  \brief  Define the pid_t type.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_BITS_TYPES_MODE_T_H_INCLUDED
#define _LIBGDSUP_INCLUDE_BITS_TYPES_MODE_T_H_INCLUDED

#ifdef _WIN32
/** mode_t is just an unsigned int.  */
typedef unsigned mode_t;
#endif

#endif  // _LIBGDSUP_INCLUDE_BITS_TYPES_MODE_T_H_INCLUDED
