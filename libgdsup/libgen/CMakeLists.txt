# \file   libgdsup/libgen/CMakeLists.txt
# \brief  CMakeLists.txt for libgen header files.
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

include(CheckCXXSourceCompiles)

check_cxx_source_compiles(
  "#include <libgen.h>
  int main(int argc, char** argv) {
    return basename(argv[0]) != 0;
  }" LIBC_PROVIDES_BASENAME)

check_cxx_source_compiles(
  "#include <libgen.h>
  int main(int argc, char** argv) {
    return dirname(argv[0]) != 0;
  }" LIBC_PROVIDES_DIRNAME)

add_gdsup_source(DISABLE_VAR LIBC_PROVIDES_BASENAME UNIT_TEST test-basename.cc SOURCES basename.c)
add_gdsup_source(DISABLE_VAR LIBC_PROVIDES_DIRNAME UNIT_TEST test-dirname.cc SOURCES dirname.c)
