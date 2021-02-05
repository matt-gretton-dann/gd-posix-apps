# \file   src/cksum/CMakeLists.txt
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

# We generate the checksum table automatically...
add_executable(gen-cksum-table EXCLUDE_FROM_ALL gen-cksum-table.cc)
target_link_libraries(gen-cksum-table PUBLIC gdsup)

add_custom_command(
  OUTPUT cksum-table.cc
  COMMAND gen-cksum-table cksum-table.cc
  DEPENDS gen-cksum-table
  COMMENT "Generating cksum-table.cc")

add_utility(NAME cksum CAT_ID cksum SOURCES cksum.cc)
target_sources(cksum PRIVATE cksum-table.cc)
