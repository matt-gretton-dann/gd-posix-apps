/** \file   libgdsup/include/gd/sys/stat.h
 *  \brief  Expose stat.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_SYS_STAT_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_SYS_STAT_H_INCLUDED

#if _WIN32
#  define _CRT_DECLARE_NONSTDC_NAMES 0
#endif

#include <sys/stat.h>

#if _WIN32
#include "gd/bits/types/blkcnt_t.h"
#include "gd/bits/types/blksize_t.h"
#include "gd/bits/types/dev_t.h"
#include "gd/bits/types/gid_t.h"
#include "gd/bits/types/ino_t.h"
#include "gd/bits/types/mode_t.h"
#include "gd/bits/types/nlink_t.h"
#include "gd/bits/types/off_t.h"
#include "gd/bits/types/uid_t.h"

#  define S_IRUSR _S_IREAD
#  define S_IRGRP _S_IREAD
#  define S_IROTH _S_IREAD
#  define S_IWUSR _S_IWRITE
#  define S_IWGRP _S_IWRITE
#  define S_IWOTH _S_IWRITE
#  define S_IXUSR _S_IEXEC
#  define S_IXGRP _S_IEXEC
#  define S_IXOTH _S_IEXEC

__EXTERN_C_BEGIN
struct stat
{
  dev_t st_dev;
  ino_t st_ino;
  mode_t st_mode;
  nlink_t st_nlink;
  uid_t st_uid;
  gid_t st_gid;
  dev_t st_rdev;
  off_t st_size;
  struct timespec st_atim;
  struct timespec st_mtim;
  struct timespec st_ctim;
  blksize_t st_blksize;
  blkcnt_t st_blocks;
};

__EXTERN_C_END

/** \brief        Stat a file based in its file handle.
 *  \param  _fd   File descriptor
 *  \param  _stat Status object to put results in
 *  \return       0 on success, -1 on failure.
 */
__EXTERN_C int fstat(int _fd, struct stat* _stat);
#endif

#endif  // _LIBGDSUP_INCLUDE_GD_SYS_STAT_H_INCLUDED
