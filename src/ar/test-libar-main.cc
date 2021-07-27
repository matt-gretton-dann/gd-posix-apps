/** \file   src/ar/test-ar-main.cc
 *  \brief  Main file for ar unit tests
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

int main(int argc, char* argv[])
{
  int result = Catch::Session().run(argc, argv);

  return result;
}
