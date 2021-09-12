/** \file   src/printf/printf.cc
 *  \brief  Implement `printf` utility
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/stdlib.h"
#include "gd/unistd.h"

#include "util/utils.hh"

#include "printf-messages.hh"

#include <cassert>
#include <clocale>
#include <iostream>
#include <sstream>
#include <utility>

namespace {
using Messages = GD::Printf::Messages;
using Msg = GD::Printf::Msg;

/** \brief       Report a warning
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
void warn(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << Messages::get().format(GD::Printf::Set::printf, msg, args...) << '\n';
}

/** \brief       Report a warning
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
void error(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << Messages::get().format(GD::Printf::Set::printf, msg, args...) << '\n'
            << Messages::get().format(GD::Printf::Set::printf, Msg::usage, GD::program_name())
            << '\n';
  ::exit(EXIT_FAILURE);
}

/** \brief  A decoded formatting_specifier.  */
struct FormatState
{
  bool left_justified_ = false;          /** Are we doing left justified output? */
  bool force_positive_ = false;          /** Do we output '+' for positive? */
  bool force_positive_space_ = false;    /** Do we output ' ' for positive? */
  bool alternative_form_ = false;        /** Do we use the alternative form? */
  bool leading_zeroes_ = false;          /** Do we output leading zeroes? */
  std::string::size_type min_width_ = 0; /** Minimum width.  */
  int32_t precision_ = -1;               /** Precision, -1 means unspecified.  */
};

enum class State {
  normal,    /**< Normal character.  */
  escape,    /**< Escape '\' specifier.  */
  octal1,    /**< Expecting first octal digit.  */
  octal2,    /**< Expecting second octal digit.  */
  octal3,    /**< Expecting third octal digit.  */
  flags,     /**< Expecting flags part of format specifier.  */
  width,     /**< Expecting width part of format specifier.  */
  precision, /**< Expecting precision part of format specifier.  */
  specifier, /**< Expecting format specifier.  */
  terminated /**< We should stop all processing.  */
};

/** \brief           Parse the second character in an escape sequence.
 *  \param  c        Character.
 *  \param  os       Stream for any output
 *  \param  extended Are we accepting extended output or not?
 *  \return          A pair: first element is resulting parse state, and second is value of octal
 *                   value being parsed.
 *
 * If \a extended we expect '0' to start an octal sequence, and 'c' will be treated as in echo
 * (cause parsing to reach State::terminated).
 *
 * If \a extended is false we allow any octal digit to start and octal sequence, and 'c' is a bad
 * escape sequence.
 */
auto parse_escape_char(char c, std::ostream& os, bool extended) -> std::pair<State, unsigned>
{
  State state = State::normal;
  int o = 0;
  switch (c) {
  case '0':
    state = extended ? State::octal1 : State::octal2;
    break;
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
    if (!extended) {
      o = c - '0';
      state = State::octal2;
    }
    else {
      error(Msg::bad_escape_character, c);
    }
    break;
  case '\\':
    os << '\\';
    break;
  case 'a':
    os << '\a';
    break;
  case 'b':
    os << '\b';
    break;
  case 'c':
    if (extended) {
      state = State::terminated;
    }
    else {
      error(Msg::bad_escape_character, c);
    }
    break;
  case 'f':
    os << '\f';
    break;
  case 'n':
    os << '\n';
    break;
  case 'r':
    os << '\r';
    break;
  case 't':
    os << '\t';
    break;
  case 'v':
    os << '\v';
    break;
  default:
    error(Msg::bad_escape_character, c);
    break;
  }

  return std::make_pair(state, o);
}

/** \brief              Print a string
 *  \param format_state Formatting state
 *  \param s            String to print
 *
 * Prints the string \a s ensuring that it prints at least the minimum width given in the
 * \a format_state and aligning to left or right as appropriate.
 */
void print_string(FormatState const& format_state, std::string const& s)
{
  if (s.length() >= format_state.min_width_) {
    std::cout << s;
  }
  else if (format_state.left_justified_) {
    std::cout << s << std::string(format_state.min_width_ - s.length(), ' ');
  }
  else {
    std::cout << std::string(format_state.min_width_ - s.length(), ' ') << s;
  }
}

/** \brief              Process a %s format specifier.
 *  \param format_state Formatting state
 *  \param s            String to print
 */
void process_string(FormatState const& format_state, std::string const& s)
{
  if (format_state.precision_ != -1 &&
      s.length() > static_cast<unsigned int>(format_state.precision_)) {
    print_string(format_state, s.substr(0, format_state.precision_));
  }
  else {
    print_string(format_state, s);
  }
}

/** \brief              Process a %c format specifier.
 *  \param format_state Formatting state
 *  \param arg          Argument to process
 */
void process_char(FormatState const& format_state, char const* arg)
{
  /* Unspecified behaviour we show nothing.  */
  if (arg == nullptr) {
    return;
  }

  print_string(format_state, std::string(1, arg[0]));
}

/** \brief               Process a %c format specifier.
 *  \param  format_state Formatting state
 *  \param  arg          String to print
 *  \return              Parsing state
 *
 * Will return \c State::terminated if we encounter a \\c escape string.  Otherwise will return
 * \c State::normal.
 */
auto process_escaped_string(FormatState const& format_state, char const* arg) -> State
{
  std::ostringstream os;       /* Build output string in here.  */
  State state = State::normal; /* State for parsing output.  */
  unsigned int o = 0;          /* Octal character.  */

  /* Handle the simple case where the argument is null.  */
  if (arg == nullptr) {
    process_string(format_state, "");
    return State::normal;
  }

  for (char const* c = arg; *c != '\0' && state != State::terminated; ++c) {
    switch (state) {
    case State::normal:
      switch (*c) {
      case '\\':
        state = State::escape;
        break;
      default:
        os << *c;
        break;
      }
      break;
    case State::escape: {
      auto r = parse_escape_char(*c, os, true);
      state = r.first;
      o = r.second;
    } break;
    case State::octal1:
    case State::octal2:
    case State::octal3:
      if (*c >= '0' && *c <= '7') {
        o = o * 8 + (*c - '0');
        if (o > 255) {
          warn(Msg::octal_overflow, o);
          state = State::normal;
        }
        else if (state == State::octal1) {
          state = State::octal2;
        }
        else if (state == State::octal2) {
          state = State::octal3;
        }
        else {
          state = State::normal;
          os << static_cast<char>(o);
        }
      }
      else {
        state = State::normal;
        os << static_cast<char>(o);
        --c;
      }
      break;
    default:
      assert(false);
    }
  }

  switch (state) {
  case State::octal1:
  case State::octal2:
  case State::octal3:
    os << static_cast<char>(o);
    state = State::normal;
    break;
  case State::escape:
    error(Msg::unterminated_escape, arg);
    state = State::normal;
    break;
  case State::normal:
  case State::terminated:
    break;
  default:
    assert(false);
  }

  process_string(format_state, os.str());
  return state;
}

/** \brief            Convert an unsigned number to a string in a particular base.
 *  \param  v         Number to convert
 *  \param  log2base  Log2 of base (supported values in range [1, 4])
 *  \param  use_upper Use upper case letters
 *  \return           String
 *
 * Returned string has no leading zeroes, and will return an empty string if v is zero.
 */
auto to_based_string(uint32_t v, unsigned log2base, bool use_upper) -> std::string
{
  assert(log2base >= 1 && "log2base must be in range [1, 4]");
  assert(log2base <= 4 && "log2base must be in range [1, 4]");

  /* Lookup table of digits to use.  */
  static char const values_lower[] = "0123456789abcdef";
  static char const values_upper[] = "0123456789ABCDEF";
  char const* values = use_upper ? values_upper : values_lower;

  std::string result;
  bool adding_digits = false;
  const unsigned basem1 = (1 << log2base) - 1;
  unsigned shift = ((31 + log2base) / log2base) * log2base;
  while (shift > 0) {
    shift -= log2base;
    unsigned c = (v >> shift) & (basem1);
    if (c != 0) {
      adding_digits = true;
    }
    if (adding_digits) {
      result += values[c];
    }
  }

  return result;
}

/** \brief      Parse a string into a 32-bit signed integer.
 *  \param  arg string to convert.
 *  \return     Integer value.
 *
 * If the first character of \a arg is a ' or " will return the value of the second character.
 *
 * Will warn if \a arg isn't a pure 32-bit integer.
 *
 */
auto parse_int(char const* arg) -> int32_t
{
  if (arg == nullptr) {
    return 0;
  }
  int32_t sign = 1;
  int32_t v = 0;

  static char const* digits = "0123456789abcdef"
                              "0123456789ABCDEF";
  char const* c = arg;

  /* Check first character for sign and quotes.  */
  if (*c == '-') {
    sign = -1;
    ++c;
  }
  else if (*c == '+') {
    ++c;
  }
  else if (*c == '\'' || *c == '"') {
    if (c[1] == '\0') {
      /* TBD: Handle MBCS.  */
      warn(Msg::missing_character_after_quote, *c);
      return 0;
    }
    return c[1];
  }

  int base = 10;
  if (*c == '0') {
    ++c;
    if (*c == 'x' || *c == 'X') {
      base = 16;
      ++c;
    }
    else {
      base = 8;
    }
  }

  {
    const auto* p = strchr(digits, *c);
    if (p == nullptr || ((p - digits) & 0xf) >= base) {
      warn(Msg::expected_decimal_argument, arg);
      return 0;
    }
  }

  for (; *c != '\0'; ++c) {
    const auto* p = strchr(digits, *c);
    if (p == nullptr || ((p - digits) & 0xf) >= base) {
      break;
    }

    int d = (p - digits) & 0xf;

    if (INT32_MAX / base < v) {
      /* For overflow we warn and clamp to INT32_MAX.  */
      warn(sign == 1 ? Msg::decimal_argument_overflow : Msg::decimal_argument_underflow, arg);
      v = INT32_MAX;
      /* Skip any remaining digits.  */
      for (++c; *c != '\0'; ++c) {
        const auto* p2 = strchr(digits, *c);
        if (p2 == nullptr || ((p2 - digits) & 0xf) >= base) {
          break;
        }
      }
      break;
    }

    v = v * base + d;
  }

  if (*c != '\0') {
    warn(Msg::extra_characters_in_decimal_argument, arg);
  }

  return v * sign;
}

/** \brief                        Print a number
 *  \param format_state           Format specifier state
 *  \param v                      String form of number to print.
 *  \param prefix                 A prefix to put in front of the number (or "" for none).
 *  \param leading_zero_precision If leading zeros are needed, what precision should we use?
 *
 * This handles formatting a number: ensuring it has enough digits, and then delegates to
 * print_string for alignment and width.
 */
void print_number(FormatState const& format_state, std::string&& v, const char* prefix,
                  std::string::size_type leading_zero_precision)
{
  assert(prefix != nullptr);

  std::string s(std::move(v));

  /* Do we need to do leading zero justification?  */
  const bool leading_zeroes =
    format_state.precision_ == -1 && format_state.leading_zeroes_ && !format_state.left_justified_;
  /* How many digits must we have? */
  std::string::size_type precision =
    format_state.precision_ > -1 ? static_cast<std::string::size_type>(format_state.precision_)
                                 : (leading_zeroes ? leading_zero_precision : 1);

  if (s == "0" && precision == 0) {
    /* Zero precision and a zero value prints nothing. */
    s.clear();
  }
  else if (s.length() < precision) {
    s = std::string(precision - s.length(), '0') + s;
  }

  s = prefix + s;
  print_string(format_state, s);
}

/** \brief              Process the %d or %i format specifier
 *  \param format_state Formatting state
 *  \param arg          Argument to format.  Maybe NULL.
 */
void process_decimal(FormatState const& format_state, char const* arg)
{
  char const* sign =
    format_state.force_positive_ ? "+" : (format_state.force_positive_space_ ? " " : "");

  int32_t sv = parse_int(arg);
  if (sv < 0) {
    sign = "-";
    sv = -sv;
  }

  std::string::size_type leading_zero_precision =
    *sign == '\0' ? format_state.min_width_
                  : (format_state.min_width_ > 1 ? format_state.min_width_ - 1 : 1);
  print_number(format_state, std::to_string(sv), sign, leading_zero_precision);
}

/** \brief              Process the %u format specifier
 *  \param format_state Formatting state
 *  \param arg          Argument to format.  Maybe NULL.
 */
void process_unsigned(FormatState const& format_state, char const* arg)
{
  int32_t vs = parse_int(arg);
  if (vs < 0) {
    warn(Msg::negative_decimal_to_unsigned, arg, 'u');
  }
  auto v = static_cast<uint32_t>(vs);
  print_number(format_state, std::to_string(v), "", format_state.min_width_);
}

/** \brief              Process the %o format specifier
 *  \param format_state Formatting state (may be modified)
 *  \param arg          Argument to format.  Maybe NULL.
 */
void process_octal(FormatState& format_state, char const* arg)
{
  int32_t vs = parse_int(arg);
  if (vs < 0) {
    warn(Msg::negative_decimal_to_unsigned, arg, 'o');
  }

  auto v = static_cast<uint32_t>(vs);
  std::string s = to_based_string(v, 3, false);
  if ((format_state.precision_ == -1 ||
       static_cast<std::string::size_type>(format_state.precision_) <= s.length()) &&
      format_state.alternative_form_) {
    if (s.length() >= INT32_MAX) {
      format_state.precision_ = INT32_MAX;
    }
    else {
      format_state.precision_ = static_cast<uint32_t>(s.length()) + 1;
    }
  }
  print_number(format_state, std::move(s), "", format_state.min_width_);
}

/** \brief              Process the %x and %X format specifiers
 *  \param format_state Formatting state
 *  \param arg          Argument to format.  Maybe NULL.
 *  \param upper        Use upper case characters or lower case ones?
 */
void process_hex(FormatState const& format_state, char const* arg, bool upper)
{
  int32_t vs = parse_int(arg);
  if (vs < 0) {
    warn(Msg::negative_decimal_to_unsigned, arg, upper ? 'X' : 'x');
  }

  auto v = static_cast<uint32_t>(vs);
  bool has_prefix = v != 0 && format_state.alternative_form_;
  char const* prefix = !has_prefix ? "" : (upper ? "0X" : "0x");
  std::string::size_type leading_zero_precision =
    !has_prefix ? format_state.min_width_
                : (format_state.min_width_ > 2 ? format_state.min_width_ - 2 : 1);
  print_number(format_state, to_based_string(v, 4, upper), prefix, leading_zero_precision);
}

/** \brief         Process a format string
 *  \param  format The format string
 *  \param  argv   Arguments to the format string
 *  \return        Pointer to first unused argument.
 */
auto process_format(char const* format, char** argv) -> char**
{
  assert(format != nullptr);
  assert(argv != nullptr);

  State state = State::normal;
  FormatState format_state;
  unsigned int o = 0;

  for (char const* c = format; *c != '\0' && state != State::terminated; ++c) {
    switch (state) {
    case State::normal:
      switch (*c) {
      case '\\':
        state = State::escape;
        break;
      case '%':
        state = State::flags;
        break;
      default:
        std::cout << *c;
        break;
      }
      break;
    case State::escape: {
      auto r = parse_escape_char(*c, std::cout, false);
      state = r.first;
      o = r.second;
    } break;
    case State::octal2:
    case State::octal3:
      if (*c >= '0' && *c <= '7') {
        o = o * 8 + (*c - '0');
        if (o > 255) {
          error(Msg::octal_overflow, o);
        }
        else if (state == State::octal2) {
          state = State::octal3;
        }
        else {
          state = State::normal;
          std::cout << static_cast<char>(o);
        }
      }
      else {
        state = State::normal;
        std::cout << static_cast<char>(o);
        --c;
      }
      break;
    case State::flags:
      switch (*c) {
      case '-':
        format_state.left_justified_ = true;
        break;
      case '+':
        format_state.force_positive_ = true;
        break;
      case ' ':
        format_state.force_positive_space_ = true;
        break;
      case '#':
        format_state.alternative_form_ = true;
        break;
      case '0':
        format_state.leading_zeroes_ = true;
        break;
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        state = State::width;
        --c;
        break;
      case '.':
        state = State::precision;
        format_state.precision_ = 0;
        break;
      default:
        state = State::specifier;
        --c;
        break;
      }
      break;
    case State::width:
      switch (*c) {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        if ((INT32_MAX / 10) < format_state.min_width_) {
          warn(Msg::width_overflow);
          format_state.min_width_ = INT32_MAX;
          while (c[1] >= '0' && c[1] <= '9') {
            ++c;
          }
        }
        else {
          format_state.min_width_ *= 10;
          format_state.min_width_ += *c - '0';
        }
        break;
      case '.':
        state = State::precision;
        format_state.precision_ = 0;
        break;
      default:
        state = State::specifier;
        --c;
        break;
      }
      break;
    case State::precision:
      switch (*c) {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        if ((INT32_MAX / 10) < format_state.precision_) {
          warn(Msg::precision_overflow);
          format_state.precision_ = INT32_MAX;
          while (c[1] >= '0' && c[1] <= '9') {
            ++c;
          }
        }
        else {
          format_state.precision_ *= 10;
          format_state.precision_ += *c - '0';
        }
        break;
      default:
        state = State::specifier;
        --c;
        break;
      }
      break;
    case State::specifier: {
      state = State::normal;
      if (*c == '%') {
        std::cout << '%';
        format_state = FormatState();
        break;
      }
      char const* arg = *argv;
      if (*argv != nullptr) {
        ++argv;
      }
      switch (*c) {
      case 'b':
        state = process_escaped_string(format_state, arg);
        break;
      case 'd':
      case 'i':
        process_decimal(format_state, arg);
        break;
      case 'u':
        process_unsigned(format_state, arg);
        break;
      case 'o':
        process_octal(format_state, arg);
        break;
      case 'x':
        process_hex(format_state, arg, false);
        break;
      case 'X':
        process_hex(format_state, arg, true);
        break;
      case 'c':
        process_char(format_state, arg);
        break;
      case 's':
        process_string(format_state, arg);
        break;
      default:
        error(Msg::bad_format_specifier, *c);
        break;
      }
      format_state = FormatState();
      break;
    }

    default:
      assert(false);
    }
  }

  switch (state) {
  case State::octal2:
  case State::octal3:
    std::cout << static_cast<char>(o);
    break;
  case State::escape:
    error(Msg::unterminated_escape, format);
    break;
  case State::flags:
  case State::width:
  case State::specifier:
  case State::precision:
    error(Msg::unterminated_format_specifier, format);
    break;
  case State::normal:
    break;
  case State::terminated:
    /* Swallow the argument vector so that we show no more work needing to be done.  */
    while (*argv != nullptr) {
      ++argv;
    }
    break;
  default:
    assert(false);
  }

  return argv;
}
}  // namespace

auto main(int argc, char** argv) -> int
{
  GD::program_name(argv[0]);
  ::setlocale(LC_ALL, "");

  int c = 0;
  while ((c = ::getopt(argc, argv, ":")) != -1) {
    if (c == '?' || c == ':') {
      error(Msg::unexpected_argument, optopt);
    }
  }

  argc -= optind;
  argv += optind;

  if (argc == 0) {
    error(Msg::missing_format);
  }

  char const* format = *(argv++);
  --argc;

  do {
    char** initial_argv = argv;
    argv = process_format(format, argv);
    if (argv == initial_argv && *argv != nullptr) {
      error(Msg::no_format_characters);
    }
  } while (*argv != nullptr);

  return EXIT_SUCCESS;
}
