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
set(BUILD_TESTING ON CACHE BOOL "Enable testing")

# Package inclusions
list(PREPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(FetchContent)
include(GNUInstallDirs)
include(Warnings)
include(CheckIPOSupported)

# Find packages
find_package(ClangFormat)
find_package(Python 3.6 COMPONENTS Interpreter)

# Global testing setup
if (BUILD_TESTING)
enable_testing()
endif()

# Integration testing setup
if(BUILD_TESTING)
set(RUN_INTEGRATION_TESTS ON)
if(NOT Python_FOUND)
message(WARNING
        "Unable to find appropriate Python 3 module (require 3.6+) - integration testing disabled.")
set(RUN_INTEGRATION_TESTS OFF)
endif()
else()
set(RUN_INTEGRATION_TESTS OFF)
endif()

# Unit testing setup
if(BUILD_TESTING)
  if($CACHE{ALLOW_THIRD_PARTY_DOWNLOADS})
    message(STATUS "Downloading Catch2 v2.13.1")
    FetchContent_Declare(
      Catch2
      GIT_REPOSITORY https://github.com/catchorg/Catch2.git
      GIT_TAG        v2.13.1)
    FetchContent_MakeAvailable(Catch2)
    message(STATUS "Catch2 sources: ${Catch2_SOURCE_DIR}")
    list(PREPEND CMAKE_MODULE_PATH "${Catch2_SOURCE_DIR}/contrib")
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

if(RUN_UNIT_TESTS)
  include(Catch)
endif()

set(CMAKE_C_STANDAD 11)
set(CMAKE_C_STANDARD_REQUIRED 11)

# Detect C++ version
list(FIND CMAKE_CXX_COMPILE_FEATURES "cxx_std_20" _has_cxx20)
if(NOT _has_cxx20 EQUAL -1)
  set(CMAKE_CXX_STANDARD 20)
  message(STATUS "Using C++20 Standard")
else()
  list(FIND CMAKE_CXX_COMPILE_FEATURES "cxx_std_17" _has_cxx17)
  if(NOT _has_cxx17 EQUAL -1)
    set(CMAKE_CXX_STANDARD 17)
    message(STATUS "Using C++17 Standard")
    else()
    message(FATAL_ERROR "Require C++17 compiler at least.")
  endif()
endif()

# Enable IPO support if possible:
check_ipo_supported(RESULT ENABLE_IPO OUTPUT IPO_WARNINGS LANGUAGES C CXX)
if (ENABLE_IPO)
message(STATUS "Inter-procedural optimizations are enabled.")
else()
  message(WARNING "Inter-procedural optimizations are not supported for this build.")
endif()

# And recurse to the subdirectories - order here is important.
add_subdirectory(src)
add_subdirectory(libgdsup)

# Display summary information
message("Integration testing: ${RUN_INTEGRATION_TESTS}")
message("Unit testing: ${RUN_UNIT_TESTS}")
