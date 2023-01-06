# \file   cmake/FindClangTidy.cmake
# \brief  Module for finding clang-tidy
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

option(RUN_CLANG_TIDY "Add clang-tidy target to format sources" ON)

if (CMAKE_VERSION VERSION_LESS 3.17)
  set(_ct_CHECK_START STATUS)
  set(_ct_CHECK_PASS STATUS)
  set(_ct_CHECK_FAIL STATUS)
else ()
  set(_ct_CHECK_START CHECK_START)
  set(_ct_CHECK_PASS CHECK_PASS)
  set(_ct_CHECK_FAIL CHECK_FAIL)
endif ()

if (RUN_CLANG_TIDY)
  if (NOT ClangTidy_COMMAND)
    foreach (_ct_N RANGE 6 20)
      list(PREPEND _ct_NAMES "clang-tidy-${_ct_N}")
    endforeach ()
    list(APPEND _ct_NAMES "clang-tidy")
    find_program(ClangTidy_COMMAND NAMES ${_ct_NAMES})
  endif ()
  if (ClangTidy_COMMAND AND NOT EXISTS "${ClangTidy_COMMAND}")
    message(WARNING "Specified path doesn't exist: ${ClangTidy_COMMAND}")
    unset(ClangTidy_COMMAND)
  endif ()

  set(ClangTidy_COMMAND "${ClangTidy_COMMAND}" CACHE INTERNAL "Clang Tidy command.")
else ()
  unset(ClangTidy_COMMAND CACHE)
endif ()

if (ClangTidy_COMMAND)
  message(${_ct_CHECK_START} "Determining clang-format version")
  execute_process(COMMAND "${ClangTidy_COMMAND}" "--version"
          OUTPUT_VARIABLE _ct_VERSION
          ERROR_QUIET)
  string(STRIP "${_ct_VERSION}" _ct_VERSION)
  string(REGEX MATCH "[0-9][0-9.]+" ClangTidy_VERSION "${_ct_VERSION}")
  if (ClangTidy_VERSION)
    message(${_ct_CHECK_PASS} "Version: ${ClangTidy_VERSION}")
  else ()
    message(${_ct_CHECK_FAIL} "Unable to determine version from '${_ct_VERSION}'")
  endif ()
endif ()

set(ClangTidy_VERSION "${ClangTidy_VERSION}" CACHE INTERNAL "Clang Tidy version")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ClangTidy
        FOUND_VAR ClangTidy_FOUND
        REQUIRED_VARS ClangTidy_COMMAND
        VERSION_VAR ClangTidy_VERSION
        )

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
if (NOT ClangTidy_COMMAND)
  set(CMAKE_CXX_CLANG_TIDY "" CACHE STRING "" FORCE)
  set(CMAKE_C_CLANG_TIDY "" CACHE STRING "" FORCE)
elseif (WIN32)
  if (NOT FORCE_SUPPLEMENTAL_LIBRARY)
    message(WARNING Disabling clang-tidy on Windows as FORCE_SUPPLEMENTAL_LIBRARY is not set)
    set(CMAKE_CXX_CLANG_TIDY "" CACHE STRING "" FORCE)
    set(CMAKE_C_CLANG_TIDY "" CACHE STRING "" FORCE)
  endif ()
  # Win32 needs to use native paths, and the source directory needs the \'s escaped so that they're
  # Used in the regex properly.
  file(TO_NATIVE_PATH "${ClangTidy_COMMAND}" _ct_ClangTidy_COMMAND)
  file(TO_NATIVE_PATH "${CMAKE_BINARY_DIR}" _ct_CMAKE_BINARY_DIR)
  file(TO_NATIVE_PATH "${CMAKE_SOURCE_DIR}" _ct_CMAKE_SOURCE_DIR)
  string(REPLACE "\\" "\\\\" _ct_CMAKE_SOURCE_DIR "${_ct_CMAKE_SOURCE_DIR}")
  # Clang-tidy doesn't pick up MSVC's exception settings so ensure they are enabled.
  # Windows makes it very hard to avoid bugprone-exception-escape so ignore at the moment.
  set(CMAKE_CXX_CLANG_TIDY
          "${_ct_ClangTidy_COMMAND}"
          "-p=${_ct_CMAKE_BINARY_DIR}"
          "--extra-arg-before=-Xclang"
          "--extra-arg-before=-fcxx-exceptions"
          "--extra-arg=-Wno-ignored-optimization-argument"
          "-checks=-bugprone-exception-escape"
          "--header-filter=^${_ct_CMAKE_SOURCE_DIR}\\\\(src|libgdsup)\\.*((\\.hh?)|(\\\\[^.]+))$")
  set(CMAKE_C_CLANG_TIDY
          "${_ct_ClangTidy_COMMAND}"
          "--extra-arg=-Wno-ignored-optimization-argument"
          "-checks=-cppcoreguidelines-*"
          "-p=${_ct_CMAKE_BINARY_DIR}"
          "--header-filter=^${_ct_CMAKE_SOURCE_DIR}\\\\(src|libgdsup)\\.*\\.h$")
else ()
  set(CMAKE_CXX_CLANG_TIDY
          "${ClangTidy_COMMAND}"
          "-p=${CMAKE_BINARY_DIR}"
          "--extra-arg=-Wno-ignored-optimization-argument"
          "--header-filter=^${CMAKE_SOURCE_DIR}/(src|libgdsup)/.*((\\.hh?)|(/[^.]+))$")
  set(CMAKE_C_CLANG_TIDY
          "${ClangTidy_COMMAND}"
          "--extra-arg=-Wno-ignored-optimization-argument"
          "--checks=-cppcoreguidelines-*"
          "-p=${CMAKE_BINARY_DIR}"
          "--header-filter=^${CMAKE_SOURCE_DIR}/(src|libgdsup)/.*\\.h$")
endif ()
