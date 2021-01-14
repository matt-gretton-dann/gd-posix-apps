/** \file   libgdsup/include/gd/nl_types.hh
 *  \brief  Expose nl_types.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_NL_TYPES_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_NL_TYPES_H_INCLUDED

#if !defined(FORCE_SUPPLEMENTAL_LIBRARY) && __has_include(<nl_types.h>)
#  include <nl_types.h>
#else
#  include <stdint.h>

/** \brief Opaque type representing a Catalogue ID.  */
typedef intptr_t nl_catd;

/** \brief  Default set ID used by gencat.  */
#  define NL_SETD (1)

#endif  // Pick a nl_types.h header.

#endif  // _LIBGDSUP_INCLUDE_GD_NL_TYPES_H_INCLUDED
