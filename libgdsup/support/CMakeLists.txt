# \file   libgdsup/support/CMakeLists.txt
# \brief  CMakeLists.txt for gdsup support/ directory
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

add_gdsup_source(UNIT_TEST test-gdsup-main.cc SOURCES empty.c)

# No unit tests for support log as its difficult to test.
add_gdsup_source(SOURCES __support_log.c)

add_gdsup_source(UNIT_TEST test-__support_read_le_u64.cc SOURCES __support_read_le_u64.c)
add_gdsup_source(UNIT_TEST test-__support_read_le_u32.cc SOURCES __support_read_le_u32.c)
