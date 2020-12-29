# \file   CMakeLists.txt
# \brief  Top level CMakeLists.txt
# \author Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

add_executable(true true-false.c)
target_compile_definitions(true PRIVATE TRUE_FALSE_EXIT_CODE=0)
add_executable(false true-false.c)
target_compile_definitions(false PRIVATE TRUE_FALSE_EXIT_CODE=1)
