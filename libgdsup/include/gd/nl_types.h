/** \file   libgdsup/include/gd/nl_types.hh
 *  \brief  Expose nl_types.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 *
 * We provide a complete reimplementation of the cat* functions to match our message catalogue file
 * format.
 */

#ifndef LIBGDSUP_INCLUDE_GD_NL_TYPES_H_INCLUDED
#define LIBGDSUP_INCLUDE_GD_NL_TYPES_H_INCLUDED

#include "gd/bits/defines.h"

/* macOS headers include <nl_types.h> randomly (and incorrectly), which leads us to multiple-
 * definition errors.  We fix this by doing what you should never do and defining include macros
 * etc.
 */
#ifdef __APPLE__
#  ifdef _NL_TYPES_H_  // NOLINT
#    error "<nl_types.h> has already been included please include gd/nl_types.h early"
#  endif
#  define _NL_TYPES_H_  // NOLINT
#  include <_types.h>

#  include <_types/_nl_item.h>
#  include <sys/cdefs.h>
#  include <sys/types.h>
#endif  // __APPLE__

#include <stdint.h>

/** \brief Opaque type representing a Catalogue ID.  */
typedef intptr_t nl_catd;  // NOLINT(modernize-use-using)

/** \brief  Default set ID used by gencat.  */
#undef NL_SETD
#define NL_SETD ((int)1)

/** \brief  Flag to indicate \c LC_MESSAGES environment should be used for locale setting.  */
#undef NL_CAT_LOCALE
#define NL_CAT_LOCALE ((int)1)

/** \brief       Close a message catalogue descriptor.
 *  \param  catd Catalogue descriptor
 *  \return      0 on success, -1 on error.  \c errno is updated.
 *
 * \subsection Error results.
 *
 * catclose() can return the following errors:
 *
 *  - \c EBADF: \a catd does not describe a catalogue descriptor.
 *
 * POSIX also allows us to return \c EINTR if we were interrupted by a signal.  However, this
 * implementation of catclose() will not do that.
 */
// NOLINTNEXTLINE(modernize-use-trailing-return-type)
__EXTERN_C int catclose(nl_catd catd);

/** \brief         Read a program message
 *  \param  catd   Catalogue descriptor
 *  \param  set_id Set ID
 *  \param  msg_id Message ID
 *  \param  s      Default message string if the given message ID cannot be found.
 *  \return        Pointer to message.  Do not modify, as returned pointer may be to \a s.
 *
 * \subsection Error results.
 *
 * If \a s is the return value \c errno will be updated to indicate the error cause.  We use:
 *
 *  - \c ENOMSG - the message identified by the ( \a set_id, \a msg_id ) pair does not exist in the
 *    catalogue.
 *  - \c EBADF - the \a catd catalogue descriptor does not reference a valid message catalogue.
 *  - \c EBADMSG - the message identified by the ( \a set_id, \a msg_id ) pair does not meet
 *    security criteria (see below).
 *  - \c EINVAL - the message catalogue refered to by \a catd has become corrupted.
 *
 * \subsection Valid messages
 *
 * We do not return messages that do not end in a NULL character.
 */
// NOLINTNEXTLINE(modernize-use-trailing-return-type)
__EXTERN_C char* catgets(nl_catd catd, int set_id, int msg_id, char const* s);

/** \brief        Open a message catalogue
 *  \param  name  Path to message catalogue
 *  \param  oflag Open flags (see below)
 *  \return       Catalogue descriptor on success, or -1 on failure.
 *
 * \subsection Open flags
 *
 * The following values may be used in the \a oflag field:
 *
 *  - \c 0 - \c LANG environment variable value is used for the locale to use in NLSPATH expansions.
 *  - \c NL_CAT_LOCALE - \c LC_MESSAGES environment variable value is used for the locale.
 *
 * All other values cause an error return.
 *
 * \subsection Error results.
 *
 * If catopen() fails to open a catalogue it may set \a errno to any of the values open() will use.
 * In particular it will use \c ENOENT if the \a name doesn't exist.
 *
 * It may also return \c ENOMEM if we do not have the memory to store the catalogue.
 *
 * As a POSIX extension: \c EINVAL is set if the \a oflag field is set to an invalid value.
 */
// NOLINTNEXTLINE(modernize-use-trailing-return-type)
__EXTERN_C nl_catd catopen(char const* /*name*/, int /*oflag*/);

#endif  // LIBGDSUP_INCLUDE_GD_NL_TYPES_H_INCLUDED
