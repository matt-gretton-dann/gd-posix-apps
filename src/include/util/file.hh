/** \file   include/util/file.hh
 *  \brief  File utilities
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_INCLUDE_UTIL_FILE_HH_INCLUDED
#define _SRC_INCLUDE_UTIL_FILE_HH_INCLUDED

#include <assert.h>
#include <fstream>
#include <string>
#include <string_view>

namespace GD::Util {
enum class Msg;
}

namespace GD {

/** \brief An input file FILE*.
 *
 * We use this instead of std::istream, so that we can avoid some locale dependencies, and also
 * to handle the "-" maps to standard input magic.
 *
 */
class InputFile
{
public:
  /** \brief          Constructor
   *  \param filename Name of file to open, '-' for stdin.
   *  \param mode     Mode to open file in, default read-only text.
   *
   * Reports an erorr if we can't open the file.
   */
  InputFile(std::string_view filename, std::string_view mode = "r");

  /** \brief Destructor
   *
   * Will close the open file if we're not stdin.
   */
  ~InputFile();

  InputFile(InputFile const&) = delete;
  InputFile& operator=(InputFile const&) = delete;
  InputFile(InputFile&&) = delete;
  InputFile& operator=(InputFile&&) = delete;

  /** \brief  Get the next character in the stream.
   *  \return EOF on end-of-file or error.
   *
   * Reports an error if we can't read from the file.
   *
   * Returnns EOF on error or end-of-file.
   */
  int getc();

  /** \brief  Get a line of text.  Strips off the \n terminator.
   *  \return Found line.
   *
   * On error, sets error flag.  On EOF set EOF flag.  In both cases may return valid string.
   */
  std::string getline();

  /** \brief  Is the error flag set on the stream?
   *  \return \c true if the error flag is set.
   */
  bool error() const;

  /** \brief  Is the EOF flag set on the stream?
   *  \return \c true iff the end-of-file flag is set.
   */
  bool eof() const;

  /** \brief  Get the printable name of the file.  */
  std::string_view filename() const;

private:
  using Msg = GD::Util::Msg;

  /** \brief     Report an error on the stream.
   *  \param msg Message ID
   */
  void report_error(Msg msg);

  std::string filename_; /**< File name.  */
  FILE* file_;           /**< File handle.  */
  bool is_stdin_;        /**< Is the File handle standard input?  */
};

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
