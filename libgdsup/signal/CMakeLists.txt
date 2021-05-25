# \file   libgdsup/signal/CMakeLists.txt
# \brief  CMakeLists.txt for signal.h implementation
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if(WIN32)
add_gdsup_source(
  SOURCES sigaction-win32.c
          sigaddset-win32.c
          sigdelset-win32.c
          sigemptyset-win32.c
          sigfillset-win32.c)

endif()
