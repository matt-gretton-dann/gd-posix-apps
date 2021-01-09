/** \file   cmake/check-format.cc
 *  \brief  Simple test to check we can build and link std::format using
 * <format>. \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include <format>
#include <iostream>

int main(int argc, char *argv) {
  for (int i = 0; i < argc; ++i) {
    std::cout << std::format("ARGV[{}] = {}\n", i, argv[i]);
  }
  return 0;
}
