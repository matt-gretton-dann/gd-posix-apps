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
if(ENABLE_NON_POSIX_EXTENSIONS)
  target_compile_definitions(libbc PRIVATE ENABLE_EXTENSIONS=1)
endif()
set_warnings(libbc)
if (ENABLE_IPO)
  set_property(TARGET libbc PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

target_sources(libbc PRIVATE instruction.cc lexer.cc parser.cc reader.cc token.cc vm.cc)
add_dependencies("libbc" "bc_messages_header")
target_add_clang_format(libbc)

if(RUN_UNIT_TESTS)
  add_executable(test-libbc)
  if(ENABLE_NON_POSIX_EXTENSIONS)
    target_compile_definitions(test-libbc PRIVATE ENABLE_EXTENSIONS=1)
  endif()
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

if(ENABLE_NON_POSIX_EXTENSIONS)
set(bc_skips
    bc/gavinhoward-bc/extra/stdin # Don't support -i command-line option
    bc/gavinhoward-bc/extra/read # Don't support read
    bc/gavinhoward-bc/extra/errors # Errors are implementation specific
    bc/gavinhoward-bc/extra/other # Don't support other extensions
    bc/gavinhoward-bc/tests/print
    bc/gavinhoward-bc/tests/print2
    bc/gavinhoward-bc/tests/parse
#    bc/gavinhoward-bc/tests/length # Needs further investigation
    bc/gavinhoward-bc/tests/shift # Needs further investigation
    bc/gavinhoward-bc/tests/multiply # Should pass
    bc/gavinhoward-bc/tests/divide # Should pass
    bc/gavinhoward-bc/tests/power # Needs further investigation
    bc/gavinhoward-bc/tests/trunc # Needs further investigation
    bc/gavinhoward-bc/tests/places
    bc/gavinhoward-bc/tests/vars
    bc/gavinhoward-bc/tests/boolean
    bc/gavinhoward-bc/tests/comp
    bc/gavinhoward-bc/tests/abs
    bc/gavinhoward-bc/tests/assignments
    bc/gavinhoward-bc/tests/functions
    bc/gavinhoward-bc/tests/scientific
    bc/gavinhoward-bc/tests/engineering
    bc/gavinhoward-bc/tests/globals
    bc/gavinhoward-bc/tests/strings
    bc/gavinhoward-bc/tests/letters
    bc/gavinhoward-bc/tests/exponent
    bc/gavinhoward-bc/tests/log
    bc/gavinhoward-bc/tests/pi
    bc/gavinhoward-bc/tests/arctangent
    bc/gavinhoward-bc/tests/sine
    bc/gavinhoward-bc/tests/cosine
    bc/gavinhoward-bc/tests/bessel
    bc/gavinhoward-bc/tests/misc1
    bc/gavinhoward-bc/tests/misc2
    bc/gavinhoward-bc/tests/misc3
    bc/gavinhoward-bc/tests/misc4
    bc/gavinhoward-bc/tests/misc5
    bc/gavinhoward-bc/tests/misc6
    bc/gavinhoward-bc/tests/void
    bc/gavinhoward-bc/tests/rand
    bc/gavinhoward-bc/tests/lib2
    bc/gavinhoward-bc/script/print
    bc/gavinhoward-bc/script/parse
    bc/gavinhoward-bc/script/add
    bc/gavinhoward-bc/script/subtract
    bc/gavinhoward-bc/script/multiply
    bc/gavinhoward-bc/script/divide
    bc/gavinhoward-bc/script/array
    bc/gavinhoward-bc/script/atan
    bc/gavinhoward-bc/script/bessel
    bc/gavinhoward-bc/script/functions
    bc/gavinhoward-bc/script/globals
    bc/gavinhoward-bc/script/len
    bc/gavinhoward-bc/script/rand
    bc/gavinhoward-bc/script/references
    bc/gavinhoward-bc/script/screen)
else()
set(bc_skips bc/gavinhoward-bc)
endif()

add_gdpat_test(NAME bc SKIPS "${bc_skips}")
