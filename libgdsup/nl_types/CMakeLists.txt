# \file   libgdsup/nl_types/CMakeLists.txt
# \brief  CMakeLists.txt for gdsup nl_types/ directory
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

# Default NLSPATH value.
set(DEFAULT_NLSPATH "${CMAKE_INSTALL_FULL_LOCALEDIR}/%N/%l/%t/%c:${CMAKE_INSTALL_FULL_LOCALEDIR}/%N/%l/%t:${CMAKE_INSTALL_FULL_LOCALEDIR}/%N/%l:${CMAKE_INSTALL_FULL_LOCALEDIR}/%N/%l:${CMAKE_INSTALL_FULL_LOCALEDIR}/%N")

configure_file(nl_types.h.in nl_types.h)
target_add_clang_format_extra(nl_types.h.in)

add_gdsup_source(UNIT_TEST test-catclose.cc SOURCES catclose.c)
add_gdsup_source(SOURCES catclose.c catgets.c catopen.c)
add_gdsup_source(SOURCES __nl_types_check_cat_header.c)

