# \file   CMakeLists.txt
# \author Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

add_library(libbc)
# Shared by library consumers and the independent header-only calibration tool.
add_library(bc-language-mode INTERFACE)
target_include_directories(libbc PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(libbc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_link_libraries(libbc PUBLIC gdsup util bc-language-mode)
set(BC_ARRAY_STORAGE hybrid CACHE STRING "Experimental bc array layout: hybrid, paged, flat, hash or ordered")
set_property(CACHE BC_ARRAY_STORAGE PROPERTY STRINGS hybrid paged flat hash ordered)
set(_bc_array_layouts hybrid paged flat hash ordered)
list(FIND _bc_array_layouts "${BC_ARRAY_STORAGE}" _bc_array_layout)
if(_bc_array_layout LESS 0)
  message(FATAL_ERROR "Unsupported BC_ARRAY_STORAGE: ${BC_ARRAY_STORAGE}")
endif()
target_compile_definitions(libbc PUBLIC GD_BC_ARRAY_STORAGE=${_bc_array_layout})
target_compile_definitions(libbc PUBLIC GD_BC_PREPARED_DIVISORS=$<BOOL:${ENABLE_BC_PREPARED_DIVISORS}>)
option(ENABLE_BC_EXACT_SQUARE "Enable the experimental exact transform square" OFF)
if(ENABLE_BC_EXACT_SQUARE)
  target_compile_definitions(libbc PUBLIC GD_BC_EXPERIMENT_NTT=1)
endif()
set_property(TARGET libbc PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
set_property(TARGET libbc PROPERTY CXX_STANDARD_REQUIRED ON)
target_compile_definitions(libbc PRIVATE _CRT_SECURE_NO_WARNINGS
  GD_BC_COMPACT_EXECUTION=$<BOOL:${ENABLE_BC_COMPACT_EXECUTION}>
  GD_BC_TYPED_RESULTS=$<BOOL:${ENABLE_BC_TYPED_RESULTS}>)
option(ENABLE_BC_THREADED_DISPATCH "Enable experimental threaded dispatch for the typed bc VM" OFF)
if(ENABLE_BC_THREADED_DISPATCH AND ENABLE_BC_TYPED_RESULTS AND ENABLE_BC_COMPACT_EXECUTION
   AND CMAKE_CXX_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$" AND NOT MSVC)
  set_property(SOURCE vm.cc APPEND PROPERTY COMPILE_DEFINITIONS GD_BC_THREADED_DISPATCH=1)
endif()
option(ENABLE_BC_VM_PGO_GENERATE "Collect an LLVM IR profile for the bc VM" OFF)
set(BC_VM_PGO_PROFILE "" CACHE FILEPATH "LLVM IR profile to apply to bc vm.cc")
if(ENABLE_BC_VM_PGO_GENERATE OR BC_VM_PGO_PROFILE)
  if(NOT CMAKE_CXX_COMPILER_ID MATCHES "^(AppleClang|Clang)$" OR MSVC)
    message(FATAL_ERROR "bc VM profiling requires a Clang compiler with GNU-style options")
  endif()
  if(ENABLE_BC_VM_PGO_GENERATE AND BC_VM_PGO_PROFILE)
    message(FATAL_ERROR "Collect and apply the bc VM profile in separate builds")
  endif()
  if(ENABLE_BC_VM_PGO_GENERATE)
    set_property(SOURCE vm.cc APPEND PROPERTY COMPILE_OPTIONS -fprofile-generate)
    target_link_options(libbc INTERFACE -fprofile-generate)
  else()
    if(NOT EXISTS "${BC_VM_PGO_PROFILE}" OR IS_DIRECTORY "${BC_VM_PGO_PROFILE}")
      message(FATAL_ERROR "BC_VM_PGO_PROFILE must name a merged LLVM profile")
    endif()
    set_property(SOURCE vm.cc APPEND PROPERTY COMPILE_OPTIONS
      "-fprofile-use=${BC_VM_PGO_PROFILE}"
      -Werror=profile-instr-out-of-date -Werror=profile-instr-unprofiled)
    set_property(SOURCE vm.cc APPEND PROPERTY OBJECT_DEPENDS "${BC_VM_PGO_PROFILE}")
  endif()
endif()
if(ENABLE_NON_POSIX_EXTENSIONS)
  # bc.hh exposes mode-dependent inline code, so every consumer needs this.
  target_compile_definitions(bc-language-mode INTERFACE ENABLE_EXTENSIONS=1)
endif()
option(ENABLE_BC_STACK_EXECUTION "Enable experimental scalar operand-stack execution" OFF)
if(ENABLE_BC_STACK_EXECUTION)
  target_sources(libbc PRIVATE stack-execution.cc)
  target_compile_definitions(libbc PRIVATE GD_BC_STACK_EXECUTION=1)
endif()
option(ENABLE_BC_NATIVE_SCALAR "Enable experimental bounded native scalar execution" OFF)
set(_bc_native_arch "${CMAKE_SYSTEM_PROCESSOR}")
if(APPLE AND CMAKE_OSX_ARCHITECTURES)
  list(LENGTH CMAKE_OSX_ARCHITECTURES _bc_native_arch_count)
  if(_bc_native_arch_count EQUAL 1)
    set(_bc_native_arch "${CMAKE_OSX_ARCHITECTURES}")
  else()
    set(_bc_native_arch "universal-fallback")
  endif()
endif()
if(WIN32 AND CMAKE_GENERATOR_PLATFORM)
  if(CMAKE_GENERATOR_PLATFORM STREQUAL "x64")
    set(_bc_native_arch "x86_64")
  else()
    set(_bc_native_arch "unsupported-windows-target")
  endif()
endif()
set(_bc_native_source "")
if(ENABLE_BC_NATIVE_SCALAR AND CMAKE_SIZEOF_VOID_P EQUAL 8)
  # The processor name can describe the host in a cross build. Check the
  # compiler's actual ISA before linking a machine-code backend. Recheck
  # when a build directory is reconfigured with different architecture flags.
  include(CheckCXXSourceCompiles)
  if(APPLE AND _bc_native_arch STREQUAL "arm64")
    unset(_bc_native_arm64_target CACHE)
    check_cxx_source_compiles("#if !defined(__aarch64__) || defined(__arm64e__)
      #error unsupported native scalar target
      #endif
      int main() { return 0; }" _bc_native_arm64_target)
    if(_bc_native_arm64_target)
      set(_bc_native_source native-scalar.cc)
    endif()
  elseif(_bc_native_arch MATCHES "^(x86_64|AMD64|amd64)$" AND (WIN32 OR UNIX))
    unset(_bc_native_x64_target CACHE)
    check_cxx_source_compiles("#if (!defined(__x86_64__) && !defined(_M_X64)) || defined(_M_ARM64EC)
      #error unsupported native scalar target
      #endif
      int main() { return 0; }" _bc_native_x64_target)
    if(_bc_native_x64_target)
      set(_bc_native_source native-scalar-x64.cc)
    endif()
  endif()
endif()
if(_bc_native_source)
  target_sources(libbc PRIVATE native-scalar-plan.cc ${_bc_native_source})
  target_compile_definitions(libbc PRIVATE GD_BC_NATIVE_SCALAR=1)
endif()
option(ENABLE_BC_NATIVE_FUNCTIONS "Enable experimental cached native scalar functions" OFF)
set(_bc_native_leaf_selected FALSE)
if(ENABLE_BC_NATIVE_FUNCTIONS AND CMAKE_SIZEOF_VOID_P EQUAL 8)
  include(CheckCXXSourceCompiles)
  if(APPLE AND _bc_native_arch STREQUAL "arm64")
    unset(_bc_native_leaf_arm64_target CACHE)
    check_cxx_source_compiles("#if !defined(__aarch64__) || defined(__arm64e__)
      #error unsupported native scalar function target
      #endif
      int main() { return 0; }" _bc_native_leaf_arm64_target)
    if(_bc_native_leaf_arm64_target)
      target_sources(libbc PRIVATE native-expression-arm64.cc)
      target_compile_definitions(libbc PRIVATE GD_BC_NATIVE_FUNCTIONS=1)
      set(_bc_native_leaf_selected TRUE)
    endif()
  elseif(_bc_native_arch MATCHES "^(x86_64|AMD64|amd64)$"
         AND (WIN32 OR (UNIX AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")))
    unset(_bc_native_leaf_x64_target CACHE)
    check_cxx_source_compiles("#if (!defined(__x86_64__) && !defined(_M_X64)) || defined(_M_ARM64EC)
      #error unsupported native scalar function target
      #endif
      int main() { return 0; }" _bc_native_leaf_x64_target)
    if(_bc_native_leaf_x64_target)
      target_sources(libbc PRIVATE native-expression-x64.cc)
      target_compile_definitions(libbc PRIVATE GD_BC_NATIVE_FUNCTIONS=1)
      set(_bc_native_leaf_selected TRUE)
    endif()
  endif()
endif()
set_warnings(libbc)
gd_apply_ipo(libbc)

target_sources(libbc PRIVATE cli-interrupt.cc execution-plan.cc instruction.cc lexer.cc number-division-cache.cc number-remainder.cc
                            number-sqrt.cc number-sqrt-sparse.cc parser.cc parser-expression.cc reader.cc
                            token.cc verification.cc vm.cc)
if(WIN32)
  target_sources(libbc PRIVATE verification-linear.cc)
endif()
if(ENABLE_BC_PROFILE)
  if(ENABLE_BC_COVERAGE)
    message(FATAL_ERROR "Use separate bc coverage and allocation profiling builds")
  endif()
  target_compile_definitions(libbc PUBLIC GD_BC_PROFILE=1)
  target_sources(libbc PRIVATE profiling.cc)
endif()
add_dependencies("libbc" "bc_messages_header")
target_add_clang_format(libbc)

if(RUN_UNIT_TESTS)
  add_executable(test-bc-profiling profiling.cc test-profiling.cc)
  target_compile_definitions(test-bc-profiling PRIVATE GD_BC_PROFILE=1)
  add_executable(test-bc-profiling-disabled test-profiling-disabled.cc)
  foreach(_profile_test test-bc-profiling test-bc-profiling-disabled)
    set_property(TARGET ${_profile_test} PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
    set_property(TARGET ${_profile_test} PROPERTY CXX_STANDARD_REQUIRED ON)
    set_warnings(${_profile_test})
    target_add_clang_format(${_profile_test})
  endforeach()
  add_test(NAME bc/profiling-allocator COMMAND "$<TARGET_FILE:test-bc-profiling>")
  add_test(NAME bc/profiling-disabled COMMAND "$<TARGET_FILE:test-bc-profiling-disabled>")
  set_tests_properties(bc/profiling-allocator bc/profiling-disabled PROPERTIES TIMEOUT 10)
  add_executable(test-bc-consumer-mode test-consumer-mode.cc)
  target_link_libraries(test-bc-consumer-mode PRIVATE libbc)
  target_include_directories(test-bc-consumer-mode PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  target_compile_definitions(test-bc-consumer-mode PRIVATE
                             GD_BC_EXPECT_EXTENSIONS=$<BOOL:${ENABLE_NON_POSIX_EXTENSIONS}>)
  add_dependencies(test-bc-consumer-mode bc_messages_header)
  set_property(TARGET test-bc-consumer-mode PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-bc-consumer-mode PROPERTY CXX_STANDARD_REQUIRED ON)
  set_warnings(test-bc-consumer-mode)
  target_add_clang_format(test-bc-consumer-mode)
  add_test(NAME bc/consumer-mode COMMAND "$<TARGET_FILE:test-bc-consumer-mode>")
  set_tests_properties(bc/consumer-mode PROPERTIES TIMEOUT 10)
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
  # Also exercise the replacement allocator through whole-program optimisation.
  # The ordinary fixture does not request IPO, but callers can inherit it.
  set(_bc_lifetime_ipo_target "")
  if(ENABLE_IPO)
    add_executable(test-bc-vm-lifetime-ipo test-vm-lifetime.cc)
    target_link_libraries(test-bc-vm-lifetime-ipo PRIVATE libbc)
    target_include_directories(test-bc-vm-lifetime-ipo PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
    target_compile_definitions(test-bc-vm-lifetime-ipo PRIVATE
      GD_BC_EXPECT_INTEGER_FUNCTIONS=$<BOOL:${ENABLE_BC_INTEGER_FUNCTIONS}>)
    add_dependencies(test-bc-vm-lifetime-ipo bc_messages_header)
    set_property(TARGET test-bc-vm-lifetime-ipo PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
    set_property(TARGET test-bc-vm-lifetime-ipo PROPERTY CXX_STANDARD_REQUIRED ON)
    set_warnings(test-bc-vm-lifetime-ipo)
    target_add_clang_format(test-bc-vm-lifetime-ipo)
    gd_apply_ipo(test-bc-vm-lifetime-ipo)
    add_test(NAME bc/vm-lifetime-ipo COMMAND "$<TARGET_FILE:test-bc-vm-lifetime-ipo>")
    set_tests_properties(bc/vm-lifetime-ipo PROPERTIES TIMEOUT 10)
    set(_bc_lifetime_ipo_target test-bc-vm-lifetime-ipo)
  endif()
  if(_bc_native_source)
    foreach(_bc_native_fault_target test-bc-vm-lifetime ${_bc_lifetime_ipo_target})
      target_compile_definitions(${_bc_native_fault_target} PRIVATE GD_BC_TEST_NATIVE_SCALAR=1)
    endforeach()
  endif()
  if(_bc_native_leaf_selected)
    foreach(_bc_native_fault_target test-bc-vm-lifetime ${_bc_lifetime_ipo_target})
      target_compile_definitions(${_bc_native_fault_target} PRIVATE GD_BC_TEST_NATIVE_FUNCTIONS=1)
    endforeach()
  endif()
  add_executable(test-libbc)
  add_dependencies(test-libbc test-bc-vm-lifetime test-bc-consumer-mode ${_bc_lifetime_ipo_target})
  target_link_libraries(test-libbc PUBLIC libbc Catch2::Catch2)
  target_compile_definitions(test-libbc PRIVATE
    GD_BC_EXPECT_TYPED_RESULTS=$<AND:$<BOOL:${ENABLE_BC_TYPED_RESULTS}>,$<BOOL:${ENABLE_BC_COMPACT_EXECUTION}>>)
  add_dependencies(test-libbc test-bc-profiling test-bc-profiling-disabled)
  set_property(TARGET test-libbc PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-libbc PROPERTY CXX_STANDARD_REQUIRED ON)
  target_include_directories(test-libbc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_warnings(test-libbc)
  target_sources(test-libbc PRIVATE
                 test-array-storage.cc
                 test-execution-plan.cc
                 test-benchmark-support.cc
                 test-format.cc
                 test-instruction.cc
                 test-token-format.cc
                 test-lexer.cc
                 test-lexer-shebang.cc
                 test-lexer-comments.cc
                 test-libbc-main.cc
                 test-digit-storage.cc
                 test-number.cc
                 test-number-radix.cc
                 test-number-workspace.cc
                 test-number-output.cc
                 test-number-length.cc
                 test-number-compare.cc
                 test-number-division.cc
                 test-number-prepared.cc
                 test-divisor-cache-fixtures.hh
                 test-number-cached.cc
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
  if(_bc_native_source)
    target_sources(test-libbc PRIVATE test-native-scalar.cc)
  endif()
  if(_bc_native_leaf_selected)
    target_sources(test-libbc PRIVATE test-native-expression.cc)
  endif()
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
  add_executable(test-divisor-cache-lifetime test-divisor-cache-lifetime.cc
                 test-divisor-cache-fixtures.hh)
  target_link_libraries(test-divisor-cache-lifetime PRIVATE libbc)
  target_include_directories(test-divisor-cache-lifetime PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_property(TARGET test-divisor-cache-lifetime PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-divisor-cache-lifetime PROPERTY CXX_STANDARD_REQUIRED ON)
  set_warnings(test-divisor-cache-lifetime)
  target_add_clang_format(test-divisor-cache-lifetime)
  gd_apply_ipo(test-divisor-cache-lifetime)
  add_test(NAME bc/divisor-cache-lifetime COMMAND test-divisor-cache-lifetime)
  set_tests_properties(bc/divisor-cache-lifetime PROPERTIES TIMEOUT 20)
  gd_apply_ipo(test-libbc)
endif()

# Keep -l as ordinary source, embedded without recognizing functions in the VM.
# A source edit must regenerate the header on the next ordinary build.
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
             "${CMAKE_CURRENT_SOURCE_DIR}/math-library.bc")
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/math-library.bc" GD_BC_LIBRARY_SOURCE)
configure_file(math-library.hh.in bc-math-library.hh @ONLY NEWLINE_STYLE LF)
unset(GD_BC_LIBRARY_SOURCE)

add_utility(NAME bc NO_INT_TEST CAT_ID bc SOURCES bc.cc math-library.cc)
target_link_libraries(bc PUBLIC libbc)
# Windows keeps the source loader because the image regressed ordinary array
# workloads. Coverage configurations exercise the source loader. Cross builds cannot run
# the generator on the build host and always use the source loader.
set(_bc_library_image_default ON)
if(WIN32 OR ENABLE_BC_COVERAGE OR CMAKE_CROSSCOMPILING)
  set(_bc_library_image_default OFF)
endif()
option(ENABLE_BC_PREPARSED_LIBRARY "Parse the ordinary bc math source at build time"
       ${_bc_library_image_default})
unset(_bc_library_image_default)
if(ENABLE_BC_PREPARSED_LIBRARY AND NOT CMAKE_CROSSCOMPILING)
  add_executable(bc-preparse-math-library preparse-math-library.cc math-library.cc)
  target_link_libraries(bc-preparse-math-library PRIVATE libbc)
  # The generator must link libbc even when it contains IPO bitcode.
  gd_apply_ipo(bc-preparse-math-library)
  target_include_directories(bc-preparse-math-library PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_property(TARGET bc-preparse-math-library PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET bc-preparse-math-library PROPERTY CXX_STANDARD_REQUIRED ON)
  set_warnings(bc-preparse-math-library)
  target_add_clang_format(bc-preparse-math-library)
  set(_bc_library_image "${CMAKE_CURRENT_BINARY_DIR}/bc-math-library-image.inc")
  add_custom_command(OUTPUT "${_bc_library_image}"
    COMMAND bc-preparse-math-library "${_bc_library_image}.tmp"
    COMMAND "${CMAKE_COMMAND}" -E rename "${_bc_library_image}.tmp" "${_bc_library_image}"
    DEPENDS bc-preparse-math-library
    VERBATIM)
  target_sources(bc PRIVATE "${_bc_library_image}")
  target_compile_definitions(bc PRIVATE GD_BC_PREPARSED_LIBRARY=1)
  target_compile_definitions(libbc PRIVATE GD_BC_PREPARSED_LIBRARY=1)
endif()
if(ENABLE_BC_PROFILE)
  target_sources(bc PRIVATE profiling-entry.cc)
endif()

if(RUN_UNIT_TESTS OR ENABLE_BC_COMPONENT_BENCHMARKS)
  add_executable(bc-components benchmark-components.cc)
  target_link_libraries(bc-components PRIVATE libbc)
  target_include_directories(bc-components PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_property(TARGET bc-components PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET bc-components PROPERTY CXX_STANDARD_REQUIRED ON)
  gd_apply_ipo(bc-components)
  set_warnings(bc-components)
  target_add_clang_format(bc-components)
  if(TARGET test-libbc)
    add_dependencies(test-libbc bc-components)
  endif()
endif()

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
  foreach(_bc_target libbc bc test-libbc test-bc-consumer-mode)
    target_compile_options(${_bc_target} PRIVATE
                           -fprofile-instr-generate -fcoverage-mapping -fcoverage-mcdc)
  endforeach()
  # Consumers (including the build-time tuner) need the profile runtime when
  # linking the instrumented library. Only asserted test profiles are collected.
  target_link_options(libbc INTERFACE -fprofile-instr-generate)
endif()

# These local CLI regressions run in both extension configurations and on Windows.
if(RUN_INTEGRATION_TESTS)
  add_test(NAME bc/bessel-zero-proof COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-bessel-zero-proof.py")
  set_tests_properties(bc/bessel-zero-proof PROPERTIES TIMEOUT 10)
  add_test(NAME bc/bessel-precision-proof COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-bessel-precision-proof.py")
  set_tests_properties(bc/bessel-precision-proof PROPERTIES TIMEOUT 60)
  add_test(NAME bc/bessel-zero-evidence COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-bessel-zero-evidence.py")
  set_tests_properties(bc/bessel-zero-evidence PROPERTIES TIMEOUT 10)
  add_test(NAME bc/bessel-classification-oracles COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/scripts/generate-bessel-classification-oracles.py" --check)
  set_tests_properties(bc/bessel-classification-oracles PROPERTIES TIMEOUT 60)
  add_test(NAME bc/bessel-oracle-portability COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-bessel-oracle-portability.py")
  set_tests_properties(bc/bessel-oracle-portability PROPERTIES TIMEOUT 300)
  # Native performance inputs use the 64-bit-host Number model's index range.
  # All layouts still have generic 8/16/32-bit storage tests on other targets.
  if(CMAKE_SIZEOF_VOID_P GREATER 4)
    add_test(NAME bc/array-benchmark COMMAND "${Python_EXECUTABLE}"
             "${CMAKE_CURRENT_SOURCE_DIR}/test-array-benchmark.py" "$<TARGET_FILE:bc>")
    set_tests_properties(bc/array-benchmark PROPERTIES TIMEOUT 60)
  endif()
  # Committed exact enclosures keep this check independent of optional FLINT.
  add_test(NAME bc/math-library-cli COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-math-library-cli.py" "$<TARGET_FILE:bc>")
  set_tests_properties(bc/math-library-cli PROPERTIES TIMEOUT 650)
  add_test(NAME bc/math-library-source COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-math-library-source.py"
           "${CMAKE_CURRENT_SOURCE_DIR}/math-library.bc"
           "${CMAKE_CURRENT_BINARY_DIR}/bc-math-library.hh")
  set_tests_properties(bc/math-library-source PROPERTIES TIMEOUT 10)
  if(TARGET bc-preparse-math-library)
    add_test(NAME bc/math-library-image COMMAND "${Python_EXECUTABLE}"
             "${CMAKE_CURRENT_SOURCE_DIR}/test-math-library-source.py"
             "${CMAKE_CURRENT_SOURCE_DIR}/math-library.bc"
             "${CMAKE_CURRENT_BINARY_DIR}/bc-math-library.hh"
             "$<TARGET_FILE:bc-preparse-math-library>" "${_bc_library_image}")
    set_tests_properties(bc/math-library-image PROPERTIES TIMEOUT 10)
  endif()
  add_test(NAME bc/math-library-decisions COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-math-library-coverage.py" "$<TARGET_FILE:bc>")
  set_tests_properties(bc/math-library-decisions PROPERTIES TIMEOUT 650)
  if(USE_GD_POSIX_APPS_TESTS)
    # Exercise the actual imported programs using numerical enclosures rather
    # than another implementation's exact final-digit snapshots.
    add_test(NAME bc/math-imported COMMAND "${Python_EXECUTABLE}"
             "${CMAKE_CURRENT_SOURCE_DIR}/test-math-imported.py" "$<TARGET_FILE:bc>"
             "${gdpat_TEST_SOURCE_DIR}/testsuites/gavinhoward-bc/tests/bc")
    set_tests_properties(bc/math-imported PROPERTIES TIMEOUT 650)
  endif()
  add_test(NAME bc/math-oracle COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-math-oracle.py")
  set_tests_properties(bc/math-oracle PROPERTIES TIMEOUT 30)
  add_test(NAME bc/gd-build-provenance COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-gd-build.py"
           "${CMAKE_CURRENT_SOURCE_DIR}/scripts/build-gd-bc.py")
  set_tests_properties(bc/gd-build-provenance PROPERTIES TIMEOUT 20)
  add_test(NAME bc/multiply-config COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-multiply-config.py" "${CMAKE_COMMAND}"
           "${CMAKE_SOURCE_DIR}/cmake/BcMultiplyConfig.cmake")
  set_tests_properties(bc/multiply-config PROPERTIES TIMEOUT 20)
  add_test(NAME bc/coverage-tools COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-coverage-tools.py")
  set_tests_properties(bc/coverage-tools PROPERTIES TIMEOUT 20)
  add_test(NAME bc/profiling-tools COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-profiling-tools.py"
           "${CMAKE_CURRENT_SOURCE_DIR}/scripts/profile-bc.py")
  set_tests_properties(bc/profiling-tools PROPERTIES TIMEOUT 20)
  if(TARGET bc-components)
    add_test(NAME bc/components COMMAND "${Python_EXECUTABLE}"
             "${CMAKE_CURRENT_SOURCE_DIR}/test-components.py" "$<TARGET_FILE:bc-components>")
    set_tests_properties(bc/components PROPERTIES TIMEOUT 30)
    add_test(NAME bc/prepared-component-evidence COMMAND "${Python_EXECUTABLE}"
             "${CMAKE_CURRENT_SOURCE_DIR}/scripts/verify-prepared-components.py")
    add_test(NAME bc/prepared-evidence-verifier COMMAND "${Python_EXECUTABLE}"
             "${CMAKE_CURRENT_SOURCE_DIR}/test-prepared-evidence.py")
    set_tests_properties(bc/prepared-component-evidence bc/prepared-evidence-verifier
                         PROPERTIES TIMEOUT 20)
  endif()
  add_test(NAME bc/comparator-builds COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-comparator-builds.py"
           "${CMAKE_CURRENT_SOURCE_DIR}/scripts/build-bc-comparators.py")
  set_tests_properties(bc/comparator-builds PROPERTIES TIMEOUT 20)
  add_test(NAME bc/ipo-policy COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-ipo-policy.py"
           --module "${CMAKE_SOURCE_DIR}/cmake/GDInterproceduralOptimization.cmake"
           --cmake "${CMAKE_COMMAND}" --compiler "${CMAKE_CXX_COMPILER}"
           --generator "${CMAKE_GENERATOR}")
  set_tests_properties(bc/ipo-policy PROPERTIES TIMEOUT 180)
  add_test(NAME bc/benchmark-ci COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-benchmark-ci.py")
  add_test(NAME bc/benchmark-acceptance COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-benchmark-acceptance.py")
  set_tests_properties(bc/benchmark-acceptance PROPERTIES TIMEOUT 15)
  add_test(NAME bc/startup-calibration COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-startup-calibration.py")
  set_tests_properties(bc/startup-calibration PROPERTIES TIMEOUT 15)
  add_test(NAME bc/control-audit COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-control-audit.py")
  set_tests_properties(bc/control-audit PROPERTIES TIMEOUT 30)
  # The fixed sparse protocol exercises 828 serial fixture processes, plus a
  # separate 15-second deadline test. Allow slower hosted process startup while
  # retaining every output/sample assertion and the children's own deadlines.
  set_tests_properties(bc/benchmark-ci PROPERTIES TIMEOUT 180)
  add_test(NAME bc/codegen-evidence COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-codegen-evidence.py")
  set_tests_properties(bc/codegen-evidence PROPERTIES TIMEOUT 30)
  add_test(NAME bc/math-benchmark-ci COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-benchmark-math-ci.py")
  set_tests_properties(bc/math-benchmark-ci PROPERTIES TIMEOUT 30)
  add_test(NAME bc/windows-image-probe COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-windows-image-probe.py" --bc "$<TARGET_FILE:bc>")
  set_tests_properties(bc/windows-image-probe PROPERTIES TIMEOUT 60)
  add_test(NAME bc/image-controls COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-image-controls.py")
  set_tests_properties(bc/image-controls PROPERTIES TIMEOUT 30)
  add_test(NAME bc/process-controls COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-process-controls.py")
  set_tests_properties(bc/process-controls PROPERTIES TIMEOUT 30)
  add_test(NAME bc/benchmark-cli COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-benchmark-cli.py"
           "${CMAKE_CURRENT_SOURCE_DIR}/scripts/bench-bc.py" --bc "$<TARGET_FILE:bc>")
  set_tests_properties(bc/benchmark-cli PROPERTIES TIMEOUT 30)
  if(ENABLE_BC_PROFILE)
    add_test(NAME bc/profiling-cli COMMAND "${Python_EXECUTABLE}"
             "${CMAKE_CURRENT_SOURCE_DIR}/test-profiling-cli.py" "$<TARGET_FILE:bc>")
    set_tests_properties(bc/profiling-cli PROPERTIES TIMEOUT 30)
  endif()
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
  add_test(NAME bc/divisor-reuse-cli COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-divisor-reuse-cli.py" "$<TARGET_FILE:bc>")
  set_tests_properties(bc/divisor-reuse-cli PROPERTIES TIMEOUT 30)
  if(ENABLE_BC_PROFILE)
    add_test(NAME bc/divisor-reuse-profile COMMAND "${Python_EXECUTABLE}"
             "${CMAKE_CURRENT_SOURCE_DIR}/test-divisor-reuse-profile.py" "$<TARGET_FILE:bc>"
             --enabled "$<IF:$<BOOL:${ENABLE_BC_PREPARED_DIVISORS}>,ON,OFF>")
    set_tests_properties(bc/divisor-reuse-profile PROPERTIES TIMEOUT 30)
  endif()
  add_test(NAME bc/vm-cli COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-vm-cli.py" "$<TARGET_FILE:bc>"
           --extensions "$<IF:$<BOOL:${ENABLE_NON_POSIX_EXTENSIONS}>,ON,OFF>")
  set_tests_properties(bc/vm-cli PROPERTIES TIMEOUT 45)
endif()

add_executable(find-multiply-split-point  find-multiply-split-point.cc)
target_include_directories(find-multiply-split-point PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(find-multiply-split-point PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_link_libraries(find-multiply-split-point PUBLIC gdsup util bc-language-mode)
set_property(TARGET find-multiply-split-point  PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
set_property(TARGET find-multiply-split-point  PROPERTY CXX_STANDARD_REQUIRED ON)
target_compile_definitions(find-multiply-split-point PRIVATE _CRT_SECURE_NO_WARNINGS)
set_warnings(find-multiply-split-point)
gd_apply_ipo(find-multiply-split-point)
# The tuner exercises header-only arithmetic. Keep it independent of libbc's
# diagnostic definition and allocator backend so instrumentation cannot tune it.
# Share only the language-mode requirement with the library.
add_dependencies(find-multiply-split-point "bc_messages_header")
target_add_clang_format(find-multiply-split-point)

if(RUN_INTEGRATION_TESTS)
  add_test(NAME bc-tuner/cli COMMAND "${Python_EXECUTABLE}"
           "${CMAKE_CURRENT_SOURCE_DIR}/test-tuner-cli.py"
           "$<TARGET_FILE:find-multiply-split-point>")
  set_tests_properties(bc-tuner/cli PROPERTIES TIMEOUT 30)
endif()


include(BcMultiplyConfig)
gd_bc_multiply_choice("${BC_MULTIPLY_SPLIT_POINT}" "${CMAKE_CROSSCOMPILING}"
                      GD_BC_MULTIPLY_MODE GD_BC_MULTIPLY_FIXED_POINT)
if(GD_BC_MULTIPLY_MODE STREQUAL "auto")
  # Keep native calibration separate from a fixed header. Reconfiguring back
  # to auto must never reuse a fixed value as though it had been calibrated.
  configure_file(find-multiply-split-point-auto.hh.in find-multiply-split-point.hh @ONLY)
  add_custom_command(
    OUTPUT "find-multiply-split-point-auto.hh"
    COMMAND "$<TARGET_FILE:find-multiply-split-point>"
            "${CMAKE_CURRENT_BINARY_DIR}/find-multiply-split-point-auto.hh"
    DEPENDS find-multiply-split-point
    COMMENT "Calibrating bc multiplication dispatch")
  set(GD_BC_MULTIPLY_HEADER_DEPS find-multiply-split-point.hh find-multiply-split-point-auto.hh)
else()
  if(BC_MULTIPLY_SPLIT_POINT STREQUAL "portable")
    set(GD_BC_MULTIPLY_DESCRIPTION "stable portable default")
  elseif(BC_MULTIPLY_SPLIT_POINT STREQUAL "auto")
    set(GD_BC_MULTIPLY_DESCRIPTION "portable cross-build fallback")
  else()
    set(GD_BC_MULTIPLY_DESCRIPTION "explicit BC_MULTIPLY_SPLIT_POINT")
  endif()
  configure_file(find-multiply-split-point-fixed.hh.in find-multiply-split-point.hh @ONLY)
  set(GD_BC_MULTIPLY_HEADER_DEPS find-multiply-split-point.hh)
  message(STATUS "bc multiplication split point: ${GD_BC_MULTIPLY_FIXED_POINT} (${GD_BC_MULTIPLY_DESCRIPTION})")
endif()
add_custom_target(find_multiply_split_point_header DEPENDS ${GD_BC_MULTIPLY_HEADER_DEPS})
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
    bc/gavinhoward-bc/tests/exponent # Numerical calls covered by bc/math-imported; exact text differs
    bc/gavinhoward-bc/tests/log # Numerical calls covered by bc/math-imported; exact text differs
    bc/gavinhoward-bc/tests/pi # Numerical calls covered by bc/math-imported; exact text differs
    bc/gavinhoward-bc/tests/arctangent # Numerical calls covered by bc/math-imported; exact text differs
    bc/gavinhoward-bc/tests/sine # Numerical calls covered by bc/math-imported; exact text differs
    bc/gavinhoward-bc/tests/cosine # Numerical calls covered by bc/math-imported; exact text differs
    bc/gavinhoward-bc/tests/bessel # Numerical calls covered by bc/math-imported; exact text differs
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
    bc/gavinhoward-bc/script/atan # Numerical calls covered by bc/math-imported; exact text differs
    bc/gavinhoward-bc/script/bessel # Numerical calls covered by bc/math-imported; exact text differs
    bc/gavinhoward-bc/script/functions # Numerical calls covered by bc/math-imported; exact text differs
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
