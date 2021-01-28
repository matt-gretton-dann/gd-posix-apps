# \file   libgdsup/nl_types/CMakeLists.txt
# \brief  CMakeLists.txt for gdsup nl_types/ directory
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

# Default NLSPATH value.
set(DEFAULT_NLSPATH "${CMAKE_INSTALL_FULL_LOCALEDIR}/%N/%N.%l_%t.%c.msg;${CMAKE_INSTALL_FULL_LOCALEDIR}/%N/%N.%l_%t.msg;${CMAKE_INSTALL_FULL_LOCALEDIR}/%N/%N.%l.msg;${CMAKE_INSTALL_FULL_LOCALEDIR}/%N/%N.msg")
file(TO_NATIVE_PATH "${DEFAULT_NLSPATH}" DEFAULT_NLSPATH)
string(REPLACE "\\" "\\\\" DEFAULT_NLSPATH "${DEFAULT_NLSPATH}")

file(TO_NATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}" CMAKE_CURRENT_BINARY_DIR_NATIVE)
string(REPLACE "\\" "\\\\" CMAKE_CURRENT_BINARY_DIR_NATIVE "${CMAKE_CURRENT_BINARY_DIR_NATIVE}")

if(WIN32)
set(DIR_SEP "\\\\")
else()
set(DIR_SEP "/")
endif()

configure_file(nl_types.h.in nl_types.h)
configure_file(test-config.h.in test-config.h)
target_add_clang_format_extra(nl_types.h.in test-config.h.in)

add_gdsup_source(UNIT_TEST test-catclose.cc SOURCES catclose.c)
add_gdsup_source(TEST_SETUP test-setup-catopen.py SETUP_EXE gencat UNIT_TEST test-catopen.cc
                 SOURCES catgets.c catopen.c)
add_gdsup_source(UNIT_TEST test-__nl_types_check_cat_header.cc
                 SOURCES __nl_types_check_cat_header.c)
