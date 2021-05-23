# \file   libgdsup/libgen/CMakeLists.txt
# \brief  CMakeLists.txt for libgen header files.
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if(WIN32)
add_gdsup_source(
  SOURCES clock_getres-win32.c
          clock_gettime-win32.c
          clock_settime-win32.c
          __support_performance_frequency-win32.c
          time.h)

endif()
