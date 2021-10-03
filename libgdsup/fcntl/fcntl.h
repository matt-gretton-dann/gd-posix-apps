/** \file   libgdsup/fcntl/fcntl.h
 *  \brief  Internal fcntl.h Functions
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef GDSUP_FCNTL_FCNTL_H_INCLUDED
#define GDSUP_FCNTL_FCNTL_H_INCLUDED

/** \brief         \c open() wrapper
 *  \param  path   Path to open
 *  \param  oflags Flags to use (O_CREAT not supported)
 *  \return        File descriptor, or -1 on error.
 *
 * This wrapper handles \c EINTR to repeat.
 */
int __fcntl_open(char const* path, int oflags);  // NOLINT

#endif  //  GDSUP_FCNTL_FCNTL_H_INCLUDED
