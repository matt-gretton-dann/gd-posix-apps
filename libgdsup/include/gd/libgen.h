/** \file  libgdsup/include/gd/libgen.h
 *  \brief Expose libgen.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_LIBGEN_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_LIBGEN_H_INCLUDED

#if !defined(FORCE_SUPPLEMENTAL_LIBRARY) && __has_include(<libgen.h>)
#  include <libgen.h>
#else
#  include "gd/bits/defines.h"

EXTERN_C char* basename(char*);
EXTERN_C char* dirname(char*);
#endif  // Pick a libgen header

#endif  // _LIBGDSUP_INCLUDE_GD_LIBGEN_H_INCLUDED
