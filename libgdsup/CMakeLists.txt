# \file   libgdsup/CMakeLists.txt
# \brief  CMakeLists.txt for gdsup Supplemental Standard Library
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

include(FetchContent)

function(check_start MSG)
  if ("CMAKE_VERSION" VERSION_LESS 3.17)
    message(STATUS "Checking ${MSG}")
    set(_check_message_cache "${MSG}" PARENT_SCOPE)
  else()
    message(CHECK_START "${MSG}")
  endif()
endfunction()

function(check_pass MSG)
  if ("CMAKE_VERSION" VERSION_LESS 3.17)
    message(STATUS "Checking ${_check_message_cache} - ${MSG}")
  else()
    message(CHECK_PASS "${MSG}")
  endif()
endfunction()

function(check_fail MSG)
  if ("CMAKE_VERSION" VERSION_LESS 3.17)
    message(STATUS "Checking ${_check_message_cache} - ${MSG}")
  else()
    message(CHECK_FAIL "${MSG}")
  endif()
endfunction()

# Download and instal fmtlib/fmt
function(include_external_fmtlib)
  message(STATUS "Fetching fmtlib/fmt 7.1.3")
  FetchContent_Declare(fmt
                       GIT_REPOSITORY https://github.com/fmtlib/fmt
                       GIT_TAG 7.1.3)
  FetchContent_GetProperties(fmt)
  if(NOT fmt_POPULATED)
    FetchContent_Populate(fmt)
    add_subdirectory(${fmt_SOURCE_DIR} ${fmt_BINARY_DIR})
  endif()
endfunction()
# External libraries

# Sets the variable CXX_STD_FILESYSTEM_LIB to the library you need to link to to add filesystem
# support
function(find_std_filesystem)
  check_start("Looking for Library for <filesystem>")

  if (DEFINED CACHE{CXX_STD_FILESYSTEM_LIBRARY})
    if (CXX_STD_FILESYSTEM_LIBRARY)
      check_pass("found: ${CXX_STD_FILESYSTEM_LIBRARY} (cached)")
    else()
      check_pass("found: No extra library needed (cached)")
    endif()
    return()
  endif()

  foreach(_ccxxfs_lib "" "stdc++fs" "c++fs" "c++fs;c++experimental")
    try_compile(_ccxxfs_present
                "${CMAKE_CURRENT_BINARY_DIR}/cmake-test"
                SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/cmake/check-filesystem.cc"
                CXX_STANDARD "${CMAKE_CXX_STANDARD}" CXX_STANDARD_REQUIRED ON
                LINK_LIBRARIES "${_ccxxfs_lib}")
    if(_ccxxfs_present)
      set(CXX_STD_FILESYSTEM_LIBRARY "${_ccxxfs_lib}" CACHE INTERNAL "Libraries for <filesystem>")
      if (CXX_STD_FILESYSTEM_LIBRARY)
        check_pass("found: ${CXX_STD_FILESYSTEM_LIBRARY}")
      else()
        check_pass("found: No extra library needed")
      endif()
      return()
    endif()
  endforeach()

  unset(CXX_STD_FILESYSTEM_LIBRARY CACHE)
  check_fail("not found")
  message(FATAL_ERROR "Unable to find library needed to use std::filesystem")
endfunction()

# Sets the vairable CXX_STD_FORMAT_LIB to the library you need to add <format> support
function(find_std_format)
  check_start("Looking for library for <format>")

  if (DEFINED CACHE{CXX_DOWNLOAD_FORMAT_LIBRARY})
    if(CXX_DOWNLOAD_FORMAT_LIBRARY)
      if(NOT ALLOW_THIRD_PARTY_DOWNLOADS)
        message(FATAL_ERROR "Inconsistency in Cmake cache, please reconfigure from scratch")
      endif()
      include_external_fmtlib()
    endif()
  endif()

  if (DEFINED CACHE{CXX_STD_FORMAT_LIBRARY})
    if(CXX_STD_FORMAT_LIBRARY)
      check_pass("found: ${CXX_STD_FORMAT_LIBRARY} (cached)")
    else()
      check_pass("found: No extra library needed (cached)")
    endif()
    return()
  endif()

  # Unless forced to use the supplemental library we prefer to use a builtin library.
  if (NOT FORCE_SUPPLEMENTAL_LIBRARY)
    try_compile(_ccxxfmt_present
                "${CMAKE_CURRENT_BINARY_DIR}/cmake-test"
                SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/cmake/check-format.cc"
                CXX_STANDARD "${CMAKE_CXX_STANDARD}" CXX_STANDARD_REQUIRED ON)
    if(_ccxxfmt_present)
      check_pass("found: Using standard library")
      set(CXX_STD_FORMAT_LIB "" CACHE INTERNAL "Library needed for <format>")
      unset(CXX_DOWNLOAD_FORMAT_LIBRARY CACHE)
      return()
    endif()
  endif()

  if(ALLOW_THIRD_PARTY_DOWNLOADS)
    include_external_fmtlib()
    check_pass("found: Using downloaded library fmt::fmt")
    set(CXX_STD_FORMAT_LIB "fmt::fmt" CACHE INTERNAL "Libray needed for <format>")
    set(CXX_DOWNLOAD_FORMAT_LIBRARY ON CACHE INTERNAL "Do we need to include fmtlib subproject?")
    return()
  endif()

  unset(CXX_DOWNLOAD_FORMAT_LIBRARY CACHE)

  find_package(fmt)
  if (NOT fmt_Found)
    check_fail("not found: Either set ALLOW_THIRD_PARTY_DOWNLOADS to on, or install fmt library")
    unset(CXX_STD_FORMAT_LIB CACHE)
    return()
  endif()

  set(CXX_STD_FORMAT_LIB "fmt::fmt" CACHE INTERNAL "Libray needed for <format>")

endfunction()

function(add_gdsup_source)
  cmake_parse_arguments(_ags "" "DISABLE_VAR;UNIT_TEST;TEST_SETUP;SETUP_EXE" "SOURCES" ${ARGN})

  if(NOT DEFINED _ags_SOURCES)
    message(FATAL_ERROR "Must specify SOURCES to add_gdsup_source")
  endif()

  if(DEFINED _ags_TEST_SETUP AND NOT DEFINED _ags_UNIT_TEST)
    message(FATAL_ERROR "TEST_SETUP makes no sense without UNIT_TEST in add_gdsup_source")
  endif()

  if(DEFINED _ags_TEST_SETUP AND NOT DEFINED _ags_SETUP_EXE)
    message(FATAL_ERROR "Must specify both (or none) of TEST_SETUP and SETUP_EXE to add_gdsup_source")
  endif()

  if(NOT DEFINED _ags_TEST_SETUP AND DEFINED _ags_SETUP_EXE)
    message(FATAL_ERROR "Must specify both (or none) of TEST_SETUP and SETUP_EXE to add_gdsup_source")
  endif()

  if(NOT DEFINED _ags_DISABLE_VAR)
    set(_ags_DISABLED OFF)
  else()
    if(NOT DEFINED CACHE{${_ags_DISABLE_VAR}})
      message(FATAL_ERROR "${_ags_DISABLE_VAR} needs to be defined in the cache")
    endif()
    set(_ags_DISABLED "$CACHE{${_ags_DISABLE_VAR}}")
  endif()

  if($CACHE{FORCE_SUPPLEMENTAL_LIBRARY} OR NOT _ags_DISABLED)
    target_sources(gdsup PRIVATE ${_ags_SOURCES})

    if(DEFINED _ags_UNIT_TEST)
      if(RUN_UNIT_TESTS)
        target_sources(test-gdsup PRIVATE ${_ags_UNIT_TEST})
        if(DEFINED _ags_TEST_SETUP)
          file(TO_NATIVE_PATH "${CMAKE_SOURCE_DIR}/python-lib" _ags_python_lib)
          file(TO_NATIVE_PATH "${CMAKE_BINARY_DIR}/pycache" _ags_python_cache)
          if($ENV{PYTHONPATH})
              if(CMAKE_HOST_UNIX)
                  set(_ags_path_sep ":")
              elseif(CMAKE_HOST_WIN32)
                  set(_ags_path_sep ";")
              else()
                  message(FATAL_ERROR "Unable to determine path separator - host is not UNIX or WIN32")
              endif()
              set(_ags_python_lib "${_ags_python_lib}${_ags_path_sep}$ENV{PYTHONPATH}")
          endif()

          file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${_ags_TEST_SETUP}" _ags_TEST_PATH)
          file(TO_NATIVE_PATH "${_ags_SETUP_EXE}" _ags_SETUP_EXE)
          file(TO_NATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}" _ags_TEST_OUTPUT)

          add_test(NAME "${_ags_TEST_SETUP}"
                  COMMAND "${Python_EXECUTABLE}" "${_ags_TEST_PATH}" "$<TARGET_FILE:${_ags_SETUP_EXE}>"
                          "--output-dir" "${_ags_TEST_OUTPUT}" --keep)
          set_tests_properties("${_ags_TEST_SETUP}"
                               PROPERTIES ENVIRONMENT
                               "PYTHONPATH=${_ags_python_lib};PYTHONPYCACHEPREFIX=${_ags_python_cache}")
          set_tests_properties("${_ags_TEST_SETUP}"
                               PROPERTIES FIXTURES_SETUP "gdsup_setup")
        endif()
      else()
        target_add_clang_format_extra(${_ags_UNIT_TEST})
      endif()
    endif()
  else()
    target_add_clang_format_extra(${_ags_SOURCES})
    target_add_clang_format_extra(${_ags_UNIT_TEST})
  endif()
endfunction()

# Library
add_library(gdsup)
set_property(TARGET gdsup PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
set_property(TARGET gdsup PROPERTY CXX_STANDARD_REQUIRED ON)
target_include_directories(gdsup PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include")
target_include_directories(gdsup PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_include_directories(gdsup PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
set_warnings(gdsup)

# Unit test executable
add_executable(test-gdsup)
target_link_libraries(test-gdsup PUBLIC gdsup Catch2::Catch2)
set_property(TARGET test-gdsup PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
set_property(TARGET test-gdsup PROPERTY CXX_STANDARD_REQUIRED ON)
target_include_directories(test-gdsup PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_include_directories(test-gdsup PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
set_warnings(test-gdsup)

if($CACHE{FORCE_SUPPLEMENTAL_LIBRARY})
target_compile_definitions(gdsup PUBLIC "$CACHE{FORCE_SUPPLEMENTAL_LIBRARY}")
endif()

target_sources(gdsup PRIVATE
               include/gd/bits/defines.h
               include/gd/sys/stat.h
               include/gd/bit.hh
               include/gd/fcntl.h
               include/gd/filesystem.hh
               include/gd/format.hh
               include/gd/libgen.h
               include/gd/limits.h
               include/gd/nl_types.h
               include/gd/stdlib.h
               include/gd/string.h
               include/gd/unistd.h)

find_std_filesystem()
find_std_format()

if(CXX_STD_FILESYSTEM_LIBRARY)
  target_link_libraries(gdsup PUBLIC "${CXX_STD_FILESYSTEM_LIBRARY}")
endif()
if(CXX_STD_FORMAT_LIB)
  target_link_libraries(gdsup PUBLIC "${CXX_STD_FORMAT_LIB}")
endif()

add_subdirectory(fcntl)
add_subdirectory(libgen)
add_subdirectory(nl_types)
add_subdirectory(stdlib)
add_subdirectory(string)
add_subdirectory(support)
add_subdirectory(unistd)

# Ensure everything gets clang formatted and found.
target_add_clang_format(gdsup)
if(TARGET test-gdsup)
  target_add_clang_format(test-gdsup)
  catch_discover_tests(test-gdsup PROPERTIES FIXTURES_REQUIRED gdsup_setup EXTRA_ARGS "-s")
endif()
