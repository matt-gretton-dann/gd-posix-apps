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

add_subdirectory(src)