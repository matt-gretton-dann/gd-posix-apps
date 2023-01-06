/** \file   libgdsup/include/gd/sys/stat.h
 *  \brief  Expose stat.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_SYS_STAT_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_SYS_STAT_H_INCLUDED  // NOLINT

#if _WIN32
#  define _CRT_DECLARE_NONSTDC_NAMES 0
#endif

#include <sys/stat.h>

#if _WIN32
#  include "gd/bits/defines.h"
#  include "gd/bits/types/blkcnt_t.h"
#  include "gd/bits/types/blksize_t.h"
#  include "gd/bits/types/dev_t.h"
#  include "gd/bits/types/gid_t.h"
#  include "gd/bits/types/ino_t.h"
#  include "gd/bits/types/mode_t.h"
#  include "gd/bits/types/nlink_t.h"
#  include "gd/bits/types/off_t.h"
#  include "gd/bits/types/uid_t.h"

#  include <time.h>

#  define S_IRUSR 00400
#  define S_IRGRP 00040
#  define S_IROTH 00004
#  define S_IWUSR 00200
#  define S_IWGRP 00020
#  define S_IWOTH 00002
#  define S_IXUSR 00100
#  define S_IXGRP 00010
#  define S_IXOTH 00001
#  define S_ISUID 04000
#  define S_ISGID 02000
#  define S_ISVTX 01000

#  define S_ISDIR(m) (((m)&_S_IFMT) == _S_IFDIR)
#  define S_ISCHR(m) (((m)&_S_IFMT) == _S_IFCHR)
#  define S_ISFIFO(m) (((m)&_S_IFMT) == _S_IFIFO)
#  define S_ISREG(m) (((m)&_S_IFMT) == _S_IFREG)

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

/** \brief        Change the mode of a file
 *  \param  _path Path to file
 *  \param  _mode New mode
 *  \return       0 on success, -1 on failure.
 */
__EXTERN_C int chmod(char const* _path, int _mode);

/** \brief        Stat a file based in its file handle.
 *  \param  _fd   File descriptor
 *  \param  _stat Status object to put results in
 *  \return       0 on success, -1 on failure.
 */
__EXTERN_C int fstat(int _fd, struct stat* _stat);

/** \brief         set and get the file mode creation mask.
 *  \param  _cmask New mask
 *  \return        Old mask
 */
__EXTERN_C mode_t umask(mode_t _cmask);

#endif  // ^ _WIN32

#endif  // _LIBGDSUP_INCLUDE_GD_SYS_STAT_H_INCLUDED
