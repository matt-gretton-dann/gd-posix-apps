/** \file   src/echo/echo.cc
 *  \brief  Implement `echo` utility
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/stdlib.h"

#include "util/utils.hh"

#include "echo-messages.hh"

#include <cassert>
#include <clocale>
#include <iostream>

namespace {
using Messages = GD::Echo::Messages;
using Msg = GD::Echo::Msg;

/** \brief  Are we doing the XSI Implementation or the plain posix one?  */
constexpr bool DO_XSI_IMPLEMENTATION = true;

/** \brief       Report a warning
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
void warn(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << Messages::get().format(GD::Echo::Set::echo, msg, args...) << '\n';
}

/** \brief  Functor to echo an argument. */
class EchoArg
{
public:
  /** \brief      Echo a given argument.
   *  \param  arg Argument to echo.
   *  \return     \c true to continue processing, \c false to stop processing and suppress newline.
   */
  void operator()(std::string_view arg)
  {
    if (early_terminate_) {
      return;
    }

    if (need_early_space_) {
      std::cout << ' ';
    }

    if (!DO_XSI_IMPLEMENTATION) {
      /* Non-XSI implementation is just output the args given.  */
      std::cout << arg;
    }

    /* On an XSI implementation we need to process escape sequences.  */
    enum class State { normal, escape, octal1, octal2, octal3 };
    State state = State::normal;
    unsigned int c = 0;
    constexpr int base_octal = 8;

    for (std::string_view::iterator it = arg.begin(); it != arg.end(); ++it) {
      switch (state) {
      case State::normal:
        if (*it == '\\') {
          state = State::escape;
        }
        else {
          std::cout << *it;
        }
        break;
      case State::escape:
        state = State::normal;
        switch (*it) {
        case 'a':
          std::cout << '\a';
          break;
        case 'b':
          std::cout << '\b';
          break;
        case 'c':
          early_terminate_ = true;
          break;
        case 'f':
          std::cout << '\f';
          break;
        case 'n':
          std::cout << '\n';
          break;
        case 'r':
          std::cout << '\r';
          break;
        case 't':
          std::cout << '\t';
          break;
        case 'v':
          std::cout << '\v';
          break;
        case '\\':
          std::cout << '\\';
          break;
        case '0':
          c = 0;
          state = State::octal1;
          break;
        default:
          warn(Msg::unrecognised_escape_sequence, *it);
          std::cout << '\\' << *it;
          break;
        }
        break;
      case State::octal1:
        if (*it >= '0' && *it < '0' + base_octal) {
          c = c * base_octal + (*it - '0');
          state = State::octal2;
        }
        else {
          std::cout << static_cast<char>(c);
          state = State::normal;
          --it; /* Reprocess the current argument.  */
        }
        break;
      case State::octal2:
        if (*it >= '0' && *it < '0' + base_octal) {
          c = c * base_octal + (*it - '0');
          state = State::octal3;
        }
        else {
          std::cout << static_cast<char>(c);
          state = State::normal;
          --it; /* Reprocess the current argument.  */
        }
        break;
      case State::octal3:
        state = State::normal;
        if (*it >= '0' && *it < '0' + base_octal) {
          c = c * base_octal + (*it - '0');
          if (c > std::numeric_limits<unsigned char>::max()) {
            warn(Msg::octal_number_out_of_range, c);
          }
          else {
            std::cout << static_cast<char>(c);
          }
        }
        else {
          --it; /* Reprocess the current argument.  */
          std::cout << static_cast<char>(c);
        }
        break;
      default:
        abort();
      }
    }

    /* Ensure we've emitted any final characters.  */
    switch (state) {
    case State::octal1:
    case State::octal2:
    case State::octal3:
      std::cout << static_cast<char>(c);
      break;
    case State::escape:
      warn(Msg::missing_escape_sequence, arg);
      std::cout << '\\';
      break;
    case State::normal:
      break;
    default:
      abort();
    }

    need_early_space_ = true;
  }

  void emit_final_newline() const
  {
    if (!early_terminate_) {
      std::cout << '\n';
    }
  }

private:
  /// Have we stopped processing args, and not echoing a newline? (i.e. seen \c)
  bool early_terminate_ = false;
  /// Do we need to emit a space before anything else? (i.e. is not the first argument?)
  bool need_early_space_ = false;
};

}  // namespace

auto main(int argc, char** argv) -> int
{
  GD::Span::span<char*> const args(argv, argc);
  GD::Span::span<char*>::iterator it = args.begin();
  GD::program_name(*it++);
  (void)std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)

  /* POSIX spec explicitly says to treat everything as something to print, so no argument or --
   * special handling.
   */

  EchoArg processor;

  processor = std::for_each(it, args.end(), processor);
  processor.emit_final_newline();

  return EXIT_SUCCESS;
}
