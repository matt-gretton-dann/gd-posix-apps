/** \file   libgdsup/support/test-gdsup-main.cc
 *  \brief  Main file for gdsup unit tests
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "support/support.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

int main(int argc, char* argv[])
{
  __support_logging_enabled = 0;

  int result = Catch::Session().run(argc, argv);

  return result;
}
