# \file   libgdsup/unistd/CMakeLists.txt
# \brief  CMakeLists.txt for unistd.h implementation files.
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if (WIN32)
  add_gdsup_source(SOURCES close-win32.c read-win32.c unlink-win32.c write-win32.c)
endif()

check_cxx_source_compiles(
  "#include <unistd.h>
  int main(char argc, char** argv) {
    return getopt(argc, argv, \":v:\") != -1;
  }" LIBC_PROVIDES_GETOPT)

add_gdsup_source(DISABLE_VAR LIBC_PROVIDES_GETOPT UNIT_TEST test-getopt.cc SOURCES getopt.c)
add_gdsup_source(SOURCES __gd_getopt_reset.c)

add_gdsup_source(SOURCES __unistd_read.c unistd.h)
