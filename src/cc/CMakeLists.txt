# \file   CMakeLists.txt
# \author Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

add_subdirectory(libcpp)

add_utility(NAME c99 NO_INT_TEST CAT_ID cc SOURCES cc.cc)
target_link_libraries(c99 PUBLIC libcpp)
