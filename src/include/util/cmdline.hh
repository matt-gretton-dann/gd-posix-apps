/**
 * \file      src/include/util/cmdline.hh
 * \brief     Header defining Command line parsing class.
 * \author    Matthew Gretton-Dann
 * \copyright 2021, Matthew Gretton-Dann
 *            SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_INCLUDE_UTIL_CMDLINE_HH_INCLUDED
#define _SRC_INCLUDE_UTIL_CMDLINE_HH_INCLUDED

#include <functional>
#include <string>
#include <unordered_map>
#include <variant>

namespace GD {
/** \brief  Command line parser
 *
 * \subsection Usage
 *
 * There are two stages to usage.  Firstly building the option list.  Secondly parsing.
 *
 * Building the option list is done by calling the various `add_option()` overloads, which add
 * support for flag and argument options.
 *
 * Parsing is done by calling `parse_args()` and giving `argc` and `argv` as passed into `main()`.
 * This returns either:
 *  * the index in the `argv` vector of the first operand to be processed,
 *  * `argc` if there are no operands to process.
 *  * a value greater than `argc` if there was an error.
 *
 * Errors during parsing are reported and a usage message is displayed.
 */
class CmdlineParser
{
public:
  /** \brief  Function type that parses a flag option.
   *
   * Types of OptFn must be able to be called with a `char` parameter indicating the option letter
   * given on the command line, and return a success flag.
   */
  using OptFn = std::function<bool(char)>;

  /** \brief  Function type that parses an argument option.
   *
   * Types of OptFn must be able to be called with a `char` parameter indicating the option letter
   * given on the command line, and a `char const*` parameter giving the argument.  They return a
   * success flag.
   */
  using ArgFn =
    std::function<bool(char, char const*)>; /* Function that process option with argument.  */

  /** \brief  Default constructor. */
  CmdlineParser();

  /** \brief    Add support for a flag option.
   *  \param c  Option character
   *  \param fn Function which handles processing option.
   *
   * `fn` should be able to be called as `bool fn(char c)` where the parameter is the character of
   * the flag.  The return value should be `true` for success and `false` otherwise.  If the parsing
   * fails `fn` should report any errors, but not the usage message.
   */
  void add_option(char c, OptFn fn);

  /** \brief    Add support for an argument option.
   *  \param c  Option character
   *  \param fn Function which handles processing option.
   *
   * `fn` should be able to be called as `bool fn(char c, char const* a)` where the first parameter
   * is the character of the flag, and the second argument is the argument for the option.  The
   * return value should be `true` for success and `false` otherwise.  If the parsing fails `fn`
   * should report any errors, but not the usage message.
   */
  void add_option(char c, ArgFn fn);

  /** \brief      Add support for a flag option (helper - just set the flag).
   *  \param c    Option character
   *  \param flag Variable to set (by non-const reference)
   *
   * Will set \a flag to \c true whenever \a c is passed as an option on the command line.
   */
  void add_option(char c, bool& flag);

  /** \brief       Parse the command line
   *  \param  argc Argument count
   *  \param  argv Argument vector
   *  \return      See below.
   *
   * The return value is one of three values:
   *
   *  * < \a argc Index of first operand after all arguments have been processed.
   *  * == \a argc No operands to process.
   *  * > \a argc An error occured.
   */
  int parse_args(int argc, char** argv) const;

private:
  /** \brief  Variant of all supported function types.  */
  using AllFns = std::variant<OptFn, ArgFn>;

  /** \brief  Map type from character to function to call.  */
  using OptMap = std::unordered_map<int, AllFns>;

  /** \brief    Handle a missing argument.
   *  \param  c Option with the missing argument.
   *  \return   \c false
   */
  bool missing_argument(char c) const;

  /** \brief    Handle an invalid option
   *  \param  c Invalid option.
   *  \return   \c false
   */
  bool invalid_option(char c) const;

  std::string args_; /**< Argument string.  */
  OptMap opts_;      /**< Option map.  */
};

}  // namespace GD
#endif /* _SRC_INCLUDE_UTIL_CMDLINE_HH_INCLUDED */
