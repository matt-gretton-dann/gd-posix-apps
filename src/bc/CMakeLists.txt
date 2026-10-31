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

target_sources(libbc PRIVATE instruction.cc lexer.cc parser.cc reader.cc token.cc verification.cc vm.cc)
add_dependencies("libbc" "bc_messages_header")
target_add_clang_format(libbc)

if(RUN_UNIT_TESTS)
  add_executable(test-bc-vm-lifetime test-vm-lifetime.cc)
  target_link_libraries(test-bc-vm-lifetime PRIVATE libbc)
  target_include_directories(test-bc-vm-lifetime PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  add_dependencies(test-bc-vm-lifetime bc_messages_header)
  set_property(TARGET test-bc-vm-lifetime PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-bc-vm-lifetime PROPERTY CXX_STANDARD_REQUIRED ON)
  set_warnings(test-bc-vm-lifetime)
  target_add_clang_format(test-bc-vm-lifetime)
  add_test(NAME bc/vm-lifetime COMMAND "$<TARGET_FILE:test-bc-vm-lifetime>")
  set_tests_properties(bc/vm-lifetime PROPERTIES TIMEOUT 10)
  add_executable(test-libbc)
  add_dependencies(test-libbc test-bc-vm-lifetime)
  if(ENABLE_NON_POSIX_EXTENSIONS)
    target_compile_definitions(test-libbc PRIVATE ENABLE_EXTENSIONS=1)
  endif()
  target_link_libraries(test-libbc PUBLIC libbc Catch2::Catch2)
  set_property(TARGET test-libbc PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-libbc PROPERTY CXX_STANDARD_REQUIRED ON)
  target_include_directories(test-libbc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_warnings(test-libbc)
  target_sources(test-libbc PRIVATE
                 test-benchmark-support.cc
                 test-format.cc
                 test-instruction.cc
                 test-token-format.cc
                 test-lexer.cc
                 test-lexer-shebang.cc
                 test-lexer-comments.cc
                 test-libbc-main.cc
                 test-number.cc
                 test-number-output.cc
                 test-number-length.cc
                 test-number-compare.cc
                 test-number-division.cc
                 test-number-zero-scale.cc
                 test-number-modulo.cc
                 test-number-power.cc
                 test-number-sqrt.cc
                 test-parser.cc
                 test-parser-errors.cc
                 test-reader.cc
                 test-vm-arrays.cc
                 test-vm-execution.cc
                 test-verification.cc)
  target_add_clang_format(test-libbc)
  add_executable(test-bc-signal test-vm-signal.cc)
  target_link_libraries(test-bc-signal PRIVATE libbc)
  target_include_directories(test-bc-signal PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_property(TARGET test-bc-signal PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-bc-signal PROPERTY CXX_STANDARD_REQUIRED ON)
  set_warnings(test-bc-signal)
  target_add_clang_format(test-bc-signal)
  add_test(NAME bc/signal-normal COMMAND "$<TARGET_FILE:test-bc-signal>" normal)
  add_test(NAME bc/signal-exception COMMAND "$<TARGET_FILE:test-bc-signal>" throw)
  set_tests_properties(bc/signal-normal bc/signal-exception PROPERTIES
                       TIMEOUT 10 SKIP_RETURN_CODE 77)
  catch_discover_tests(test-libbc PROPERTIES FIXTURES_REQUIRED testutil_setup TIMEOUT 120
                      EXTRA_ARGS "--rng-seed" "197199")
  add_executable(test-bc-offsets test-vm-offsets.cc)
  target_link_libraries(test-bc-offsets PRIVATE libbc)
  target_include_directories(test-bc-offsets PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_property(TARGET test-bc-offsets PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-bc-offsets PROPERTY CXX_STANDARD_REQUIRED ON)
  set_warnings(test-bc-offsets)
  target_add_clang_format(test-bc-offsets)
  if (ENABLE_IPO)
    set_property(TARGET test-libbc PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
  endif()
endif()

add_utility(NAME bc NO_INT_TEST CAT_ID bc SOURCES bc.cc)
target_link_libraries(bc PUBLIC libbc)

if(ENABLE_BC_COVERAGE)
  if(NOT RUN_UNIT_TESTS OR NOT RUN_INTEGRATION_TESTS)
    message(FATAL_ERROR "bc coverage requires both unit tests and Python CLI tests")
  endif()
  if(NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    message(FATAL_ERROR "bc MC/DC coverage requires a supported Clang compiler")
  endif()
  include(CheckCXXCompilerFlag)
  include(CMakePushCheckState)
  cmake_push_check_state(RESET)
  set(CMAKE_REQUIRED_LINK_OPTIONS -fprofile-instr-generate)
  check_cxx_compiler_flag("-fprofile-instr-generate -fcoverage-mapping -fcoverage-mcdc"
                         GD_BC_HAS_MCDC)
  cmake_pop_check_state()
  if(NOT GD_BC_HAS_MCDC)
    message(FATAL_ERROR "This Clang configuration cannot instrument bc MC/DC coverage")
  endif()
  foreach(_bc_target libbc bc test-libbc)
    target_compile_options(${_bc_target} PRIVATE
                           -fprofile-instr-generate -fcoverage-mapping -fcoverage-mcdc)
  endforeach()
  # Consumers (including the build-time tuner) need the profile runtime when
  # linking the instrumented library. Only asserted test profiles are collected.
  target_link_options(libbc INTERFACE -fprofile-instr-generate)
endif()

# These local CLI regressions run in both extension configurations and on Windows.
if(RUN_INTEGRATION_TESTS)
  add_test(NAME bc/benchmark-cli COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-benchmark-cli.py"
           "${CMAKE_CURRENT_SOURCE_DIR}/scripts/bench-bc.py" --bc "$<TARGET_FILE:bc>")
  set_tests_properties(bc/benchmark-cli PROPERTIES TIMEOUT 30)
  add_test(NAME bc/cli COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-cli.py" "$<TARGET_FILE:bc>"
           --extensions "$<IF:$<BOOL:${ENABLE_NON_POSIX_EXTENSIONS}>,ON,OFF>")
  set_tests_properties(bc/cli PROPERTIES TIMEOUT 60)
  if(TARGET test-bc-offsets)
    add_test(NAME bc/verifier-offsets COMMAND "${Python_EXECUTABLE}"
             "${CMAKE_CURRENT_SOURCE_DIR}/test-vm-offsets.py" "$<TARGET_FILE:test-bc-offsets>")
    set_tests_properties(bc/verifier-offsets PROPERTIES TIMEOUT 30)
  endif()
  add_test(NAME bc/number-oracle COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-number-oracle.py" "$<TARGET_FILE:bc>")
  set_tests_properties(bc/number-oracle PROPERTIES TIMEOUT 90)
  add_test(NAME bc/signal-delivery COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-signal-delivery.py" "$<TARGET_FILE:bc>")
  set_tests_properties(bc/signal-delivery PROPERTIES TIMEOUT 20 SKIP_RETURN_CODE 77)
  add_test(NAME bc/vm-cli COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-vm-cli.py" "$<TARGET_FILE:bc>"
           --extensions "$<IF:$<BOOL:${ENABLE_NON_POSIX_EXTENSIONS}>,ON,OFF>")
  set_tests_properties(bc/vm-cli PROPERTIES TIMEOUT 45)
endif()

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

if(RUN_INTEGRATION_TESTS)
  add_test(NAME bc-tuner/cli COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-tuner-cli.py"
           "$<TARGET_FILE:find-multiply-split-point>")
  set_tests_properties(bc-tuner/cli PROPERTIES TIMEOUT 30)
endif()


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

if (NOT WIN32)
# The imported misc answers discard precision on zero terms, contrary to POSIX.
# Keep its mismatch visible; bc/number-oracle checks all four expressions against
# independent exact arithmetic and radix conversion rather than those answers.
add_gdpat_test(NAME bc SKIPS "${bc_skips}" XFAILS bc/gavinhoward-bc/tests/misc)
endif()
