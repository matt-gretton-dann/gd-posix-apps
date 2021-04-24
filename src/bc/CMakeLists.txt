# \file   CMakeLists.txt
# \author Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

add_library(libbc)
target_include_directories(libbc PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(libbc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_link_libraries(libbc PUBLIC gdsup util)
set_property(TARGET libbc PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
set_property(TARGET libbc PROPERTY CXX_STANDARD_REQUIRED ON)
target_compile_definitions(libbc PRIVATE _CRT_SECURE_NO_WARNINGS)
set_warnings(libbc)
target_sources(libbc PRIVATE lexer.cc reader.cc token.cc)
target_add_clang_format(libbc)

if(RUN_UNIT_TESTS)
  add_executable(test-libbc)
  target_link_libraries(test-libbc PUBLIC libbc Catch2::Catch2)
  set_property(TARGET test-libbc PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-libbc PROPERTY CXX_STANDARD_REQUIRED ON)
  target_include_directories(test-libbc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_warnings(test-libbc)
  target_sources(test-libbc PRIVATE test-libbc-main.cc)
  target_add_clang_format(test-libbc)
  catch_discover_tests(test-libbc PROPERTIES FIXTURES_REQUIRED testutil_setup EXTRA_ARGS "-s")
endif()

add_utility(NAME bc NO_INT_TEST CAT_ID bc SOURCES bc.cc)
target_link_libraries(bc PUBLIC libbc)
