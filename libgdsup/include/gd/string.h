/** \file   libgdsup/include/gd/string.h
 *  \brief  Expose string.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_STRING_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_STRING_H_INCLUDED

#include "gd/bits/defines.h"

#include <string.h>

#if _WIN32
/** \brief    Duplicate a string
 *  \param  s String to duplicate
 *  \return   Duplicated string, or \c NULL on error - \c errno is updated.
 *
 * On error we set errno to \c ENOMEM to indicate out of memory.
 */
__EXTERN_C char* strdup(char const* s);
#endif  // _WIN32

#endif  // _LIBGDSUP_INCLUDE_GD_STDLIB_H_INCLUDED
