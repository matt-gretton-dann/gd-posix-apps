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
if (ENABLE_IPO)
  set_property(TARGET libbc PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

target_sources(libbc PRIVATE instruction.cc lexer.cc parser.cc reader.cc token.cc vm.cc)
add_dependencies("libbc" "bc_messages_header")
target_add_clang_format(libbc)

if(RUN_UNIT_TESTS)
  add_executable(test-libbc)
  target_link_libraries(test-libbc PUBLIC libbc Catch2::Catch2)
  set_property(TARGET test-libbc PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-libbc PROPERTY CXX_STANDARD_REQUIRED ON)
  target_include_directories(test-libbc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_warnings(test-libbc)
  target_sources(test-libbc PRIVATE
                 test-lexer.cc
                 test-libbc-main.cc
                 test-number.cc
                 test-parser.cc
                 test-reader.cc)
  target_add_clang_format(test-libbc)
  catch_discover_tests(test-libbc PROPERTIES FIXTURES_REQUIRED testutil_setup EXTRA_ARGS "-s")
  if (ENABLE_IPO)
    set_property(TARGET test-libbc PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
  endif()
endif()

add_utility(NAME bc NO_INT_TEST CAT_ID bc SOURCES bc.cc)
target_link_libraries(bc PUBLIC libbc)

add_executable(find-multiply-split-point  find-multiply-split-point.cc)
target_include_directories(find-multiply-split-point PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(find-multiply-split-point PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_link_libraries(find-multiply-split-point PUBLIC gdsup util)
set_property(TARGET find-multiply-split-point  PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
set_property(TARGET find-multiply-split-point  PROPERTY CXX_STANDARD_REQUIRED ON)
target_compile_definitions(find-multiply-split-point PRIVATE _CRT_SECURE_NO_WARNINGS)
set_warnings(find-multiply-split-point)
if (ENABLE_IPO)
  set_property(TARGET find-multiply-split-point PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()
target_link_libraries(find-multiply-split-point PUBLIC libbc)
add_dependencies(find-multiply-split-point "bc_messages_header")
target_add_clang_format(find-multiply-split-point)


add_custom_command(
  OUTPUT "find-multiply-split-point.hh"
  COMMAND "$<TARGET_FILE:find-multiply-split-point>"
          "${CMAKE_CURRENT_BINARY_DIR}/find-multiply-split-point.hh"
  DEPENDS find-multiply-split-point
  COMMENT "Generating find-multiply-split-point.hh")
add_custom_target(find_multiply_split_point_header DEPENDS find-multiply-split-point.hh)
add_dependencies(bc find_multiply_split_point_header)

add_gdpat_test(bc)
