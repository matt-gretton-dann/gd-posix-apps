# \file   libgdsup/unistd/CMakeLists.txt
# \brief  CMakeLists.txt for unistd.h implementation files.
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if (WIN32)
  add_gdsup_source(SOURCES close-win32.c read-win32.c write-win32.c)
endif()
