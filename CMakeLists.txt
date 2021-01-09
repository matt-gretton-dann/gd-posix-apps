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
set(ENABLE_TESTS ON CACHE BOOL "Do we enable testing?")

# Package inclusions
list(PREPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

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

# And recurse to the subdirectories
add_subdirectory(src)

# Display summary information
message("Integration testing: ${RUN_INTEGRATION_TESTS}")
