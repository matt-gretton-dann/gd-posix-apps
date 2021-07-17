/** \file   libgdsup/include/bits/types/pid_t.h
 *  \brief  Define the pid_t type.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_BITS_TYPES_UID_T_H_INCLUDED
#  define _LIBGDSUP_INCLUDE_BITS_TYPES_UID_T_H_INCLUDED

#ifdef _WIN32
/** On Windows we don't yet properly support user IDs  */
typedef void* uid_t;
#endif

#endif  // _LIBGDSUP_INCLUDE_BITS_TYPES_UID_T_H_INCLUDED