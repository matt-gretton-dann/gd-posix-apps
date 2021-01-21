/** \file   libgdsup/unistd/unistd.h
 *  \brief  Internal unitstd.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef _GDSUP_UNITSTD_UNISTD_H_INCLUDED
#define _GDSUP_UNITSTD_UNISTD_H_INCLUDED

#include "gd/bits/defines.h"

/** \brief         \c read() wrapper
 *  \param  fd     File descriptor to read from
 *  \param  buf    Buffer to read data into
 *  \param  nbyte  Maximum number of bytes of read.
 *  \return        Number of bytes read, or -1 on error.
 *
 * This wrapper handles interrupted reads by trying again until we reach one of:
 *  - end-of file,
 *  - Having read \c SSIZE_MAX bytes
 *  - Another error.
 */
__EXTERN_C ssize_t __unistd_read(int fd, void* buf, size_t nbyte);

#endif  //  _GDSUP_UNITSTD_UNISTD_H_INCLUDED
