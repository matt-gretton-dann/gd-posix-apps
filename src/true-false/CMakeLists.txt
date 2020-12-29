# \file   CMakeLists.txt
# \brief  Top level CMakeLists.txt
# \author Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

add_utility(NAME false SOURCES true-false.c)
target_compile_definitions(false PRIVATE TRUE_FALSE_EXIT_CODE=1)

add_utility(NAME true SOURCES true-false.c)
target_compile_definitions(true PRIVATE TRUE_FALSE_EXIT_CODE=0)
