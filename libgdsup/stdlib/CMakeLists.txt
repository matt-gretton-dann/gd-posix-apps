# \file   libgdsup/stdlib/CMakeLists.txt
# \brief  CMakeLists.txt for stdlib.h implementation files.
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if (WIN32)
  add_gdsup_source(SOURCES mkstemp-win32.c)
endif()
