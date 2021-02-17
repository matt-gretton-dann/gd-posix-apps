# \file   libgdsup/langinfo/CMakeLists.txt
# \brief  CMakeLists.txt for langinfo header files.
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if(WIN32)
add_gdsup_source(SOURCES nl_langinfo-win32.c)
endif()
