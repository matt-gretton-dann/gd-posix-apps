/** \file   include/util/file.hh
 *  \brief  File utilities
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_INCLUDE_UTIL_FILE_HH_INCLUDED
#define _SRC_INCLUDE_UTIL_FILE_HH_INCLUDED

#include <cassert>
#include <fstream>
#include <string_view>

namespace GD {

/** \brief           Call a function for all files named on the command-line, handling '-' as stdin.
 *  \tparam Fn       Type of \a apply_fn.
 *  \param  argc     Argument count (>= 0).
 *  \param  argv     Argument vector, argv[0...argc - 1] should be file names, argv[argc] a nullptr.
 *  \param  apply_fn Function to apply.
 *  \return          True if all applications succeed.
 *
 * If \a argc is zero, then \a apply_fn is called with the file name "-".  Otherwise it is called
 * on every element in \a argv.  If an application fails we still carry on with all the rest.
 *
 * \a apply_fn should have a signature compatible with `bool apply_fn(std::string_view fname)`.  The
 * passed value is the name of the file, and the return value should be whether the application
 * succeeded or not.
 */
template<typename Fn>
bool for_each_file(int argc, char** argv, Fn apply_fn)
{
  assert(argv != nullptr);
  assert(argc >= 0);

  bool success = true;
  if (argc == 0) {
    success &= apply_fn("-");
  }
  else {
    while (argc > 0) {
      assert(*argv != nullptr);
      success &= apply_fn(*argv);
      ++argv;
      --argc;
    }
  }

  assert(argc == 0);
  assert(*argv == nullptr);

  return success;
}

}  // namespace GD

#endif  // _SRC_INCLUDE_UTIL_FILE_HH_INCLUDED
