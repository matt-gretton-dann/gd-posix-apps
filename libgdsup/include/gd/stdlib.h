/** \file   libgdsup/include/gd/stdlib.h
 *  \brief  Expose stdlib.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_STDLIB_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_STDLIB_H_INCLUDED

/* Everyone has a stdlib.h.  */
#include <stdlib.h>

#if _WIN32
#  include "gd/bits/defines.h"

/** \brief       Create a unique file
 *  \param  temp Template for filename.  Must end in six 'X' characters.
 *  \return      Open file descriptor, or -1 on error.
 */
EXTERN_C int mkstemp(char* temp);
#endif  // _WIN32

#endif  // _LIBGDSUP_INCLUDE_GD_STDLIB_H_INCLUDED
