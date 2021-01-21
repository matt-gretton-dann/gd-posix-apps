/** \file   libgdsup/include/gd/stdlib.h
 *  \brief  Expose stdlib.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_STDLIB_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_STDLIB_H_INCLUDED

#include "gd/bits/defines.h"

/* Everyone has a stdlib.h.  */
#include <stdlib.h>

#if _WIN32
/** \brief       Create a unique file
 *  \param  temp Template for filename.  Must end in six 'X' characters.
 *  \return      Open file descriptor, or -1 on error.
 */
__EXTERN_C int mkstemp(char* temp);

/** \brief             Add or change an environment variable
 *  \param  envname    Name of environment variable
 *  \param  envval     New value
 *  \param  overwrite  Set to non-zero to overwrite an existing value.  Otherwise only add new env.
 *  \return            \c 0 on success, \c -1 on error, updating \c errno.
 *
 * A successful return includes the case when \a envname already exists in the environment and
 * \a overwrite is \c 0, so the environment is not updated.
 *
 * \subsection Error returns
 *
 * On error we update errno to the following values
 *
 *  - \c EINVAL - \a envname is NULL, empty, or contains a '='
 */
__EXTERN_C int setenv(char const* envname, char const* envval, int overwrite);
#endif  // _WIN32

#endif  // _LIBGDSUP_INCLUDE_GD_STDLIB_H_INCLUDED
