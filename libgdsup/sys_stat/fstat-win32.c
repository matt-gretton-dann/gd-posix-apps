/** \file   libgdsup/sys_stat/fstat-win32.h
 *  \brief  Win32 API implementation of fstat()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/sys/stat.h"

#include <errno.h>
#include <io.h>
#include <windows.h>

#include "time/time.h"

static mode_t get_mode_bits(DWORD file_attributes)
{
  mode_t mode = S_IRUSR | S_IRGRP | S_IROTH;
  if ((file_attributes & FILE_ATTRIBUTE_READONLY) == 0) {
    mode |= S_IWUSR | S_IWGRP | S_IWOTH;
  }

  if ((file_attributes & FILE_ATTRIBUTE_DEVICE) != 0) {
    mode |= _S_IFCHR;
  }
  else if ((file_attributes & FILE_ATTRIBUTE_DIRECTORY) != 0) {
    mode |= _S_IFDIR;
  }
  else {
    mode |= _S_IFREG;
  }

  return mode;
}

int fstat(int _fd, struct stat* _stat)
{
  HANDLE handle = (HANDLE)_get_osfhandle(_fd);  // NOLINT
  if (handle == INVALID_HANDLE_VALUE) {         // NOLINT
    errno = EBADF;
    return -1;
  }

  BY_HANDLE_FILE_INFORMATION file_info;
  BOOL success = GetFileInformationByHandle(handle, &file_info);
  if (!success) {
    errno = EBADF;
    return -1;
  }

  _stat->st_rdev = _stat->st_dev = file_info.dwVolumeSerialNumber;
  _stat->st_ino = file_info.nFileIndexHigh;
  _stat->st_ino <<= sizeof(file_info.nFileIndexHigh) * CHAR_BIT;
  _stat->st_ino += file_info.nFileIndexLow;
  _stat->st_size = file_info.nFileSizeHigh;
  _stat->st_mode = get_mode_bits(file_info.dwFileAttributes);
  _stat->st_nlink = file_info.nNumberOfLinks;
  _stat->st_uid = -1;
  _stat->st_gid = -1;
  _stat->st_size <<= sizeof(file_info.nFileSizeHigh) * CHAR_BIT;
  _stat->st_size += file_info.nFileSizeLow;
  if (__convert_file_time_to_timespec(&_stat->st_atim, file_info.ftLastAccessTime,
                                      WINDOWS_TO_POSIX_BIAS) != 0) {
    return -1;
  }
  if (__convert_file_time_to_timespec(&_stat->st_mtim, file_info.ftLastWriteTime,
                                      WINDOWS_TO_POSIX_BIAS) != 0) {
    return -1;
  }
  if (__convert_file_time_to_timespec(&_stat->st_ctim, file_info.ftCreationTime,
                                      WINDOWS_TO_POSIX_BIAS) != 0) {
    return -1;
  }
  _stat->st_blksize = 1;
  _stat->st_blocks = _stat->st_size;

  return 0;
}
