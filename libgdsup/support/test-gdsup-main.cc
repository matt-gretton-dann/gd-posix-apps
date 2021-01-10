/** \file   libgdsup/support/test-gdsup-main.cc
 *  \brief  Main file for gdsup unit tests
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#define CATCH_CONFIG_MAIN
#include <catch2/catch.hpp>

TEST_CASE("Ensure we have a test", "[support]") { REQUIRE(1 == 1); }
