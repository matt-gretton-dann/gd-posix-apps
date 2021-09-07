/** \file   src/echo/echo.cc
 *  \brief  Implement `echo` utility
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/stdlib.h"

#include "util/utils.hh"

#include "echo-messages.hh"

#include <assert.h>
#include <iostream>
#include <locale.h>

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

/** \brief      Echo a given argument.
 *  \param  arg Argument to echo.
 *  \return     \c true to continue processing, \c false to stop processing and suppress newline.
 */
auto echo_arg(char const* arg) -> bool
{
  if (!DO_XSI_IMPLEMENTATION) {
    /* Non-XSI implementation is just output the args given.  */
    std::cout << arg;
    return true;
  }

  /* On an XSI implementation we need to process escape sequences.  */
  enum class State { normal, escape, octal1, octal2, octal3 };
  State state = State::normal;
  unsigned int c = 0;
  char const* whole_arg = arg;

  for (; *arg != '\0'; ++arg) {
    switch (state) {
    case State::normal:
      if (*arg == '\\') {
        state = State::escape;
      }
      else {
        std::cout << *arg;
      }
      break;
    case State::escape:
      state = State::normal;
      switch (*arg) {
      case 'a':
        std::cout << '\a';
        break;
      case 'b':
        std::cout << '\b';
        break;
      case 'c':
        return false;
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
        warn(Msg::unrecognised_escape_sequence, *arg);
        std::cout << '\\' << *arg;
        break;
      }
      break;
    case State::octal1:
      if (*arg >= '0' && *arg <= '7') {
        c = c * 8 + (*arg - '0');
        state = State::octal2;
      }
      else {
        std::cout << (char)c;
        state = State::normal;
        --arg; /* Reprocess the current argument.  */
      }
      break;
    case State::octal2:
      if (*arg >= '0' && *arg <= '7') {
        c = c * 8 + (*arg - '0');
        state = State::octal3;
      }
      else {
        std::cout << (char)c;
        state = State::normal;
        --arg; /* Reprocess the current argument.  */
      }
      break;
    case State::octal3:
      state = State::normal;
      if (*arg >= '0' && *arg <= '7') {
        c = c * 8 + (*arg - '0');
        if (c > 255) {
          warn(Msg::octal_number_out_of_range, c);
        }
        else {
          std::cout << (char)c;
        }
      }
      else {
        --arg; /* Reprocess the current argument.  */
        std::cout << (char)c;
      }
      break;
    default:
      assert(false);
    }
  }

  /* Ensure we've emitted any final characters.  */
  switch (state) {
  case State::octal1:
  case State::octal2:
  case State::octal3:
    std::cout << (char)c;
    break;
  case State::escape:
    warn(Msg::missing_escape_sequence, whole_arg);
    std::cout << '\\';
    break;
  case State::normal:
    break;
  default:
    assert(false);
  }

  return true;
}
}  // namespace

auto main(int argc, char** argv) -> int
{
  GD::program_name(argv[0]);
  ::setlocale(LC_ALL, "");

  --argc;
  ++argv;

  /* POSIX spec explicitly says to treat everything as something to print, so no argument or --
   * special handling.
   */

  bool emit_newline = true; /* Do we emit a newline at the end? */

  while (argc > 0) {
    assert(*argv != nullptr);
    emit_newline = echo_arg(*argv);
    if (!emit_newline) {
      break;
    }
    --argc;
    ++argv;
    if (argc != 0) {
      std::cout << ' ';
    }
  }

  if (emit_newline) {
    std::cout << '\n';
  }

  assert(argc > 0 || *argv == nullptr);

  return EXIT_SUCCESS;
}
