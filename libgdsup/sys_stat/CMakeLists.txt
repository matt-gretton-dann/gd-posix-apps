# \file   libgdsup/sys_stat/CMakeLists.txt
# \brief  CMakeLists.txt for sys/stat.h implementation files.
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if (WIN32)
  add_gdsup_source(SOURCES chmod-win32.c)
  add_gdsup_source(SOURCES fstat-win32.c)
  add_gdsup_source(UINT_TEST test-umask.cc SOURCES umask-win32.c)
endif()
