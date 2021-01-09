# \file   CMakeLists.txt
# \brief  Top level CMakeLists.txt
# \author Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

cmake_minimum_required(VERSION 3.15)
project(gd-posix-apps
        VERSION 0.0
        DESCRIPTION "Implementation of POSIX Utilities and Shell"
        HOMEPAGE_URL "https://github.com/matt-gretton-dann/gd-posix-apps"
        LANGUAGES C CXX)

# Cached variables
set(ALLOW_THIRD_PARTY_DOWNLOADS ON CACHE BOOL "Do we allow third-party downloads?")
set(FORCE_SUPPLEMENTAL_LIBRARY OFF CACHE BOOL "Force use of libgdsup?")
set(ENABLE_TESTS ON CACHE BOOL "Do we enable testing?")

# Package inclusions
list(PREPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(FetchContent)

# Find packages
find_package(ClangFormat)
find_package(Python 3.8 COMPONENTS Interpreter)

# Global testing setup
if (ENABLE_TESTS)
enable_testing()
endif()

# Integration testing setup
if(ENABLE_TESTS)
set(RUN_INTEGRATION_TESTS ON)
if(NOT Python_FOUND)
message(WARNING
        "Unable to find appropriate Python 3 module (require 3.8+) - integration testing disabled.")
set(RUN_INTEGRATION_TESTS OFF)
endif()
else()
set(RUN_INTEGRATION_TESTS OFF)
endif()

# Unit testing setup
if(ENABLE_TESTS)
  if($CACHE{ALLOW_THIRD_PARTY_DOWNLOADS})
    message(STATUS "Downloading Catch2 v2.13.1")
    FetchContent_Declare(
      Catch2
      GIT_REPOSITORY https://github.com/catchorg/Catch2.git
      GIT_TAG        v2.13.1)
    FetchContent_MakeAvailable(Catch2)
    set(RUN_UNIT_TESTS ON)
  else()
    find_packacge(Catch2 2.13.1)
    if(NOT Catch2_FOUND)
      message(WARNING "Unable to find Catch2 - unit tests will be disabled")
      set(RUN_UNIT_TESTS OFF)
    else()
      set(RUN_UNIT_TESTS ON)
    endif()
  endif()
else()
  set(RUN_UNIT_TESTS OFF)
endif()

# And recurse to the subdirectories
add_subdirectory(libgdsup)
add_subdirectory(src)

# Display summary information
message("Integration testing: ${RUN_INTEGRATION_TESTS}")
message("Unit testing: ${RUN_UNIT_TESTS}")
