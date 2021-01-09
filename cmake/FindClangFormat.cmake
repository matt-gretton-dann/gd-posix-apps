# \file   cmake/FindClangFormat.cmake
# \brief  Module for finding clang-format
# \author Copyright 2020, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

option(RUN_CLANG_FORMAT "Add clang-format target to format sources" ON)

if(CMAKE_VERSION VERSION_LESS 3.17)
  set(_cf_CHECK_START STATUS)
  set(_cf_CHECK_PASS STATUS)
  set(_cf_CHECK_FAIL STATUS)
else()
  set(_cf_CHECK_START CHECK_START)
  set(_cf_CHECK_PASS CHECK_PASS)
  set(_cf_CHECK_FAIL CHECK_FAIL)
endif()

if(RUN_CLANG_FORMAT)
  if(NOT ClangFormat_COMMAND)
    foreach(_cf_N RANGE 6 12)
      list(PREPEND _cf_NAMES "clang-format-${_cf_N}")
    endforeach()
    list(APPEND _cf_NAMES "clang-format")
    find_program(ClangFormat_COMMAND NAMES ${_cf_NAMES})
  endif()
  if(ClangFormat_COMMAND AND NOT EXISTS "${ClangFormat_COMMAND}")
    message(WARNING "Specified path doesn't exist: ${ClangFormat_COMMAND}")
    unset(ClangFormat_COMMAND)
  endif()

  set(ClangFormat_COMMAND "${ClangFormat_COMMAND}" CACHE INTERNAL "Clang Format command.")
else()
  unset(ClangFormat_COMMAND CACHE)
endif()

if(ClangFormat_COMMAND)
  message(${_cf_CHECK_START} "Determining clang-format version")
  execute_process(COMMAND "${ClangFormat_COMMAND}" "-version"
    OUTPUT_VARIABLE _cf_VERSION
    ERROR_QUIET)
  string(STRIP "${_cf_VERSION}" _cf_VERSION)
  string(REGEX MATCH "[0-9][0-9.]+" ClangFormat_VERSION "${_cf_VERSION}")
  if (ClangFormat_VERSION)
    message(${_cf_CHECK_PASS} "Version: ${ClangFormat_VERSION}")
  else()
    message(${_cf_CHECK_FAIL} "Unable to determine version from '${_cf_VERSION}'")
  endif()
endif()

set(ClangFormat_VERSION "${ClangFormat_VERSION}" CACHE INTERNAL "Clang Format version")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ClangFormat
  FOUND_VAR ClangFormat_FOUND
  REQUIRED_VARS ClangFormat_COMMAND
  VERSION_VAR ClangFormat_VERSION
)

# Ensure we have the clang-format target.
if(ClangFormat_FOUND AND RUN_CLANG_FORMAT)
  add_custom_target(clang-format)
endif()

function(_cf_FindScript)
  if (ClangFormat_VERSION VERSION_LESS 10)
    # If ClangFormat is version 9 or before We need Python 3 here.
    find_package(Python 3.6 REQUIRED COMPONENTS Interpreter)

    file(TO_NATIVE_PATH "${CMAKE_SOURCE_DIR}/cmake/clang-format-check.py" _cf_CHECK_SCRIPT)
    set(_cf_CHECK_SCRIPT "${_cf_CHECK_SCRIPT}" PARENT_SCOPE)
  endif()
endfunction()

# Run clang-format over the sources of a target.
# target: Target to run clang-format over.
#
# Adds several targets:
#  ${target}-clang-format: Just run clang-format on the given target
#  clang-format: Run clang-format on all targets that have been given.
#
# Modifies ${target} so that it will check that clang-format passes (without making any changes.
function(target_add_clang_format target)
  if(NOT RUN_CLANG_FORMAT)
    return()
  endif()
  if(NOT ClangFormat_COMMAND)
    return()
  endif()

  _cf_FindScript()

  # Get the absolute path of each source we need
  get_target_property(target_sources ${target} SOURCES)
  foreach(target_source ${target_sources})
    get_filename_component(target_abs_source ${target_source} ABSOLUTE)
    file(RELATIVE_PATH target_rel_source "${CMAKE_SOURCE_DIR}" "${target_abs_source}")
    string(MAKE_C_IDENTIFIER "${target_rel_source}" c_id)

    # Will run clang-format over the sources for this target
    if(NOT TARGET "clang-format-${c_id}")
      add_custom_target(
        "clang-format-${c_id}"
        COMMAND "${ClangFormat_COMMAND}" -style=file -i "${target_abs_source}"
        COMMENT "Formatting source: ${target_rel_source}")
      add_dependencies(clang-format "clang-format-${c_id}")
    endif()

    # Update the target TARGET to do a dry-run of clang-format to highlight any issues.
    if(ClangFormat_VERSION VERSION_LESS 10)
      add_custom_command(
        TARGET ${target}
        POST_BUILD
        COMMAND "${Python_EXECUTABLE}" "${_cf_CHECK_SCRIPT}" "${ClangFormat_COMMAND}" -style=file "${target_abs_source}"
        COMMENT "Checking formatting of source: ${target_rel_source}")
    else()
      add_custom_command(
        TARGET ${target}
        POST_BUILD
        COMMAND "${ClangFormat_COMMAND}" -style=file --dry-run --ferror-limit=1 -Werror "${target_abs_source}"
        COMMENT "Checking formatting of source: ${target_rel_source}")
    endif()
  endforeach()
endfunction()

# Run clang-format over code that isn't explicitly listed as sources of another target
function(target_add_clang_format_extra)
  if (NOT RUN_CLANG_FORMAT)
    return()
  endif()
  if (NOT ClangFormat_COMMAND)
    return()
  endif()

  _cf_FindScript()

  foreach(source ${ARGV})
    get_filename_component(abs_source "${source}" ABSOLUTE)
    file(RELATIVE_PATH rel_source "${CMAKE_SOURCE_DIR}" "${abs_source}")
    string(MAKE_C_IDENTIFIER "${rel_source}" c_id)
    set(target "clang-format-${c_id}")

    # Will run clang-format over the sources for this target
    if(NOT TARGET "${target}")
      add_custom_target(
        "${target}"
        COMMAND "${ClangFormat_COMMAND}" -style=file -i "${abs_source}"
        COMMENT "Formatting source: ${rel_source}")
      add_dependencies(clang-format "${target}")
    endif()

    # Update all to do a dry-run of clang-format to highlight any issues.
    if(ClangFormat_VERSION VERSION_LESS 10)
      add_custom_target(
        "${target}.clang-format-check" ALL
        COMMAND "${Python_EXECUTABLE}" "${_cf_CHECK_SCRIPT}" "${ClangFormat_COMMAND}" -style=file "${abs_source}"
        COMMENT "Checking formatting of: ${rel_source}")
    else()
      add_custom_target(
        "${target}.clang-format-check" ALL
        COMMAND "${ClangFormat_COMMAND}" -style=file --dry-run --ferror-limit=1 "${abs_source}"
        COMMENT "Checking formatting of: ${rel_source}")
    endif()
  endforeach()
endfunction()
