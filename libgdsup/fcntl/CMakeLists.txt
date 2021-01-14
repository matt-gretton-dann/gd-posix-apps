# \file   libgdsup/fcntl/CMakeLists.txt
# \brief  CMakeLists.txt for fcntl.h implementation sources.
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if (WIN32)
  add_gdsup_source(SOURCES open-win32.c)
endif()
