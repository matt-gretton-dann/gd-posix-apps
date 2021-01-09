# \file   libgdsup/libgen/CMakeLists.txt
# \brief  CMakeLists.txt for libgen header files.
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

include(CheckCXXSourceCompiles)

check_cxx_source_compiles(
  "#include <libgen.h>
  int main(char argc, char** argv) {
    return basename(argv[0]) != 0;
  }" GDSUP_PROVIDES_BASENAME)

check_cxx_source_compiles(
  "#include <libgen.h>
  int main(char argc, char** argv) {
    return dirname(argv[0]) != 0;
  }" GDSUP_PROVIDES_DIRNAME)

if ($CACHE{FORCE_SUPPLEMENTAL_LIBRARY} OR NOT $CACHE{GDSUP_PROVIDES_BASENAME})
  target_sources(gdsup PRIVATE basename.c)
else()
  target_add_clang_format_extra(basename.c)
endif()

if ($CACHE{FORCE_SUPPLEMENTAL_LIBRARY} OR NOT $CACHE{GDSUP_PROVIDES_BASENAME})
  target_sources(gdsup PRIVATE dirname.c)
else()
  target_add_clang_format_extra(dirname.c)
endif()
