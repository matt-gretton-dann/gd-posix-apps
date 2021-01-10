# \file   libgdsup/support/CMakeLists.txt
# \brief  CMakeLists.txt for gdsup support/ directory
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

add_gdsup_source(UNIT_TEST test-gdsup-main.cc SOURCES empty.c)
target_sources(gdsup PRIVATE empty.c)
