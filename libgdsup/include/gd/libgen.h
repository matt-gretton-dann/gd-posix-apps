/** \file  libgdsup/include/gd/libgen.h
 *  \brief Expose libgen.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBGDSUP_INCLUDE_GD_LIBGEN_H_INCLUDED
#define LIBGDSUP_INCLUDE_GD_LIBGEN_H_INCLUDED

#if !defined(FORCE_SUPPLEMENTAL_LIBRARY) && __has_include(<libgen.h>)
#  include <libgen.h>
#else
#  include "gd/bits/defines.h"

/** \brief       Return last component of \a path
 *  \param  path Path to return last component of
 *  \return      Last component of path.
 *
 * \a path may be NULL.  In which case we return ".".
 *
 * \a path may be modified.
 *
 * \subsection Errors
 *
 * \fn basename() does not return any errors.
 *
 * \subsection Implementation Notes
 *
 * POSIX allows us to modify \a path.  Which we do in the majority of cases.  In the cases we don't
 * we ensure the output points to modifiable memory.
 *
 * This implementation is thread-safe.
 *
 * For '//' we return '//' - POSIX allows '//' or '/'.
 *
 * \todo: Make '//' handling platform specific.
 */
__EXTERN_C char* basename(char* path) __NOEXCEPT;
/* Work around some GLIBC definitions.  */
#  undef basename
#  define basename basename

/** \brief       Report the parent directory name of \a path - a file pathname
 *  \param  path File pathname to report parent directory name of.
 *  \return      Parent directory name.
 *
 * \a path may be NULL.  In which case we return ".".
 *
 * \a path may be modified.
 *
 * \subsection Errors
 *
 * \fn dirname() does not return any errors.
 *
 * \subsection Implementation Notes
 *
 * POSIX allows us to modify \a path.  Which we do in the majority of cases.  In the cases we don't
 * we ensure the output points to modifiable memory.
 *
 * This implementation is thread-safe.
 *
 * For '//' we return '//' - POSIX allows '//' or '/'.
 *
 * \todo: Make '//' handling platform specific.
 */
__EXTERN_C char* dirname(char*) __NOEXCEPT;
#endif  // Pick a libgen header

#endif  // LIBGDSUP_INCLUDE_GD_LIBGEN_H_INCLUDED
