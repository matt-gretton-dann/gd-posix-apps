/** \file   libgdsup/include/bits/types/uid_t.h
 *  \brief  Define the pid_t type.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBGDSUP_INCLUDE_BITS_TYPES_GID_T_H_INCLUDED
#define LIBGDSUP_INCLUDE_BITS_TYPES_GID_T_H_INCLUDED

#ifdef _WIN32
#  include <stdint.h>
/** On Windows we map group IDs to intptrs so we can handle them as Pointers too.  */
typedef intptr_t gid_t;
#endif

#endif  // LIBGDSUP_INCLUDE_BITS_TYPES_UID_T_H_INCLUDED
