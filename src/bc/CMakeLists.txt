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

if(ENABLE_NON_POSIX_EXTENSIONS AND NOT WIN32)
set(bc_skips
    bc/gavinhoward-bc/extra/stdin # Don't support -i command-line option
    bc/gavinhoward-bc/extra/read # Don't support read
    bc/gavinhoward-bc/extra/errors # Errors are implementation specific
    bc/gavinhoward-bc/extra/other # Don't support other extensions
    bc/gavinhoward-bc/tests/print2 # No support for multi char function names
    bc/gavinhoward-bc/tests/shift # We don't support <<, >>, <<=, >>=
    bc/gavinhoward-bc/tests/trunc # No support for truncation
    bc/gavinhoward-bc/tests/places # Don't support places extensions
    bc/gavinhoward-bc/tests/vars # Don't support arbitary variable names
    bc/gavinhoward-bc/tests/boolean # Don't support booleans outside of conditional statements
    bc/gavinhoward-bc/tests/comp # Don't support comparisons outside of conditional statements
    bc/gavinhoward-bc/tests/assignments # Don't support print.
    bc/gavinhoward-bc/tests/scientific # Don't support scientific notation
    bc/gavinhoward-bc/tests/engineering # Don't support engineering obase
    bc/gavinhoward-bc/tests/strings # Don't support print.
    bc/gavinhoward-bc/tests/letters # Don't support letters G-Z.
    bc/gavinhoward-bc/tests/exponent # Library not yet written
    bc/gavinhoward-bc/tests/log # Library not yet written
    bc/gavinhoward-bc/tests/pi # Library not yet written
    bc/gavinhoward-bc/tests/arctangent # Library not yet written
    bc/gavinhoward-bc/tests/sine # Library not yet written
    bc/gavinhoward-bc/tests/cosine # Library not yet written
    bc/gavinhoward-bc/tests/bessel # Library not yet written
    bc/gavinhoward-bc/tests/misc1 # No support for last
    bc/gavinhoward-bc/tests/misc2 # We're strict about presence of newlines.
    bc/gavinhoward-bc/tests/misc3 # We're strict about presence of newlines, and no support for print
    bc/gavinhoward-bc/tests/misc4 # Don't support maxibase, maxscale
    bc/gavinhoward-bc/tests/misc5 # We're strict about presence of newlines.
    bc/gavinhoward-bc/tests/misc6 # We're strict about presence of newlines.
    bc/gavinhoward-bc/tests/void # Don't support void
    bc/gavinhoward-bc/tests/rand # Don't support rand
    bc/gavinhoward-bc/tests/lib2 # No extended library support
    bc/gavinhoward-bc/script/print # Don't support print
    bc/gavinhoward-bc/script/parse # Do not support continue
    bc/gavinhoward-bc/script/add # Don't support multi-char variables
    bc/gavinhoward-bc/script/subtract # Don't support multi-char variables
    bc/gavinhoward-bc/script/multiply # Don't support multi-char variables
    bc/gavinhoward-bc/script/divide # Don't support multi-char variables
    bc/gavinhoward-bc/script/array # No support for comments
    bc/gavinhoward-bc/script/atan # No support for library
    bc/gavinhoward-bc/script/bessel # No support for library
    bc/gavinhoward-bc/script/functions # No support for library
    bc/gavinhoward-bc/script/len # No support for multi-char function names.
    bc/gavinhoward-bc/script/rand # No support for random numbers.
    bc/gavinhoward-bc/script/references # No support for references
  )
else()
set(bc_skips bc/gavinhoward-bc)
endif()

add_gdpat_test(NAME bc SKIPS "${bc_skips}")
