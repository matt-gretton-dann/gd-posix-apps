/** \file   libgdsup/support/test-util-main.cc
 *  \brief  Main file for gdsup unit tests
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

auto main(int argc, char* argv[]) -> int
{
  int const result = Catch::Session().run(argc, argv);

  return result;
}
