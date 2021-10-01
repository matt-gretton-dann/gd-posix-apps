
/** \file   libgdsup/include/bits/types/off_t.h
 *  \brief  Define the off_t type.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_BITS_TYPES_OFF_T_H_INCLUDED
#define _LIBGDSUP_INCLUDE_BITS_TYPES_OFF_T_H_INCLUDED

#ifdef _WIN32
typedef long off_t;
#endif

#endif  // _LIBGDSUP_INCLUDE_BITS_TYPES_OFF_T_H_INCLUDED
