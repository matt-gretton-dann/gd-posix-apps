/** \file   cmake/check-filesystem.cc
 *  \brief  Simple test to check we can build and link std::filesystem.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#if __has_include(<filesystem>)
#include <filesystem>
namespace fs = ::std::filesystem;
#elif __has_include(<experimental/filesystem>)
#include <experimental/filesystem>
namespace fs = ::std::experimental::filesystem;
#endif

int main(int argc, char **argv) {
  fs::path p(argv[0]);
  return fs::exists(p);
}
