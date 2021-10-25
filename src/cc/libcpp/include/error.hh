/** \file   libcpp/include/error.hh
 *  \brief  Error handling class
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_ERROR_HH_INCLUDED_
#define CC_LIBCPP_ERROR_HH_INCLUDED_

#include "gd/format.hh"

#include "cc-messages.hh"

#include <iostream>
#include <locale>
#include <utility>

#include "location.hh"

namespace GD::CPP {
class FileStore;

/** \brief  List of error codes.  */
using ErrorCode = GD::Cc::Msg;

/** \brief List of error severities.  */
enum class ErrorSeverity {
  note,         ///< Just a note
  info,         ///< Informational
  warning,      ///< Warning
  error,        ///< Error - but an attempt will be made to continue
  fatal_error,  ///< Error - causes compilation to abort immediately
  ice           ///< Internal Compiler Error
};

/** \brief  Error Manager
 *
 * This manages errors, handling translation of error codes into error messages, and their severity.
 */
class ErrorManager
{
public:
  /** \brief    Construct the error manager
   *  \param os Stream to output errors on.
   */
  explicit ErrorManager(std::ostream& os);

  ErrorManager() = delete;
  ErrorManager(ErrorManager const&) = delete;
  ErrorManager(ErrorManager&&) noexcept = delete;
  auto operator=(ErrorManager const&) -> ErrorManager& = delete;
  auto operator=(ErrorManager&&) noexcept -> ErrorManager& = delete;
  ~ErrorManager() = default;

  /** \brief Report an internal compiler error.
   *  \param format Format string for error message
   *  \param args   Arguments
   *
   * Does not return instead aborts the code.
   */
  template<typename... Args>
  [[noreturn]] static void ice(std::string const& format, Args&&... args)
  {
    std::cerr << "!!! INTERNAL COMPILER ERROR !!!\n";
    std::cerr << fmt::format(format, args...);
    std::cerr << "\nAborting compiler.\n";
    std::abort();
  }

  /** \brief  Set the file store to use when looking up locations.  */
  void file_store(FileStore& fs);

  /** \brief         Generate an error
   *  \param   code  Error code
   *  \param   args  Arguments for error message
   *  \return        Error object
   *
   * If the error has severity ICE we will abort, if the error has severity fatal_error then we
   * will exit with a failure code.  Otherwise the function will return after printing the error.
   */
  template<typename... Args>
  void error([[maybe_unused]] ErrorCode code, Range range, [[maybe_unused]] Args&&... args)
  {
    return do_error(code, range,
                    GD::Cc::Messages::get().format(GD::Cc::Set::errors, code, args...));
  }

  /** \brief                 Set the maximum number of errors before we stop producing errors.
   *  \param max_error_count Maximum number of errors.
   */
  void max_error_count(std::uint32_t max_error_count);

private:
  /** \brief        Generic part of implementation of ErrorManager::error()
   *  \param  code  Error code
   *  \param  msg   Error message
   *  \return       Error object.
   */
  void do_error(ErrorCode code, Range range, std::string msg);

  constexpr static std::uint32_t default_max_error_count_ = 128;

  std::ostream& os_;                 ///< Output stream for errors
  FileStore* file_store_ = nullptr;  ///< File store to use to find locations.
  std::uint32_t error_count_ = 0;    ///< Number of errors produced
  std::uint32_t max_error_count_ = default_max_error_count_;  ///< Maximum number of errors
};

/** \brief   To string for a char32_t.  Here as mostly used in debugging.
 *  \param i Character to convert
 *  \return  String representing character.
 */
inline auto to_string(char32_t i) -> std::string
{
  auto const& facet =
    std::use_facet<std::codecvt<char32_t, char, std::mbstate_t>>(std::locale());  // NOLINT
  auto mbstate = std::mbstate_t{};
  std::string e(facet.max_length(), '\0');
  char32_t const* i_next = nullptr;
  char* e_next = nullptr;
  // NOLINTNEXTLINE
  if (facet.out(mbstate, &i, &i + 1, i_next, e.data(), e.data() + e.size(), e_next) ==
      std::codecvt_base::ok) {
    e.resize(e_next - e.data());
    return e;
  }
  return fmt::format("CHARACTER({0:08x})", static_cast<uint32_t>(i));
}
}  // namespace GD::CPP

/** \brief       assert() equivalent that ICE's on failure
 *  \param CHECK Boolean expression to check
 *  \param MSG   Message to help understand the check.
 */
// NOLINTNEXTLINE
#define assert_ice(CHECK, MSG)                                                                     \
  if (!(CHECK)) {                                                                                  \
    ::GD::CPP::ErrorManager::ice("Assertion failed in {0} line {1}: {2}\nExplanation: {3}\n",      \
                                 __FILE__, __LINE__, #CHECK, MSG);                                 \
  }

/** \brief  Format a error severity
 */
template<>
struct fmt::formatter<GD::CPP::ErrorSeverity>
{
  static constexpr auto parse(format_parse_context& ctx)
  {
    auto it = ctx.begin();  // NOLINT
    for (; it != ctx.end(); ++it) {
      switch (*it) {
      case '}':
        return it;
      default:
        assert_ice(false, "Invalid parse format.");
      }
    }

    if (it != ctx.end()) {
      assert_ice(false, "Invalid parse format - not terminated.");
    }

    return it;
  }

  template<typename FormatContext>
  auto format(GD::CPP::ErrorSeverity severity, FormatContext& ctx)
  {
    GD::Cc::Msg msg = GD::Cc::Msg::ice;
    switch (severity) {
    case GD::CPP::ErrorSeverity::note:
      msg = GD::Cc::Msg::note;
      break;
    case GD::CPP::ErrorSeverity::info:
      msg = GD::Cc::Msg::info;
      break;
    case GD::CPP::ErrorSeverity::warning:
      msg = GD::Cc::Msg::warning;
      break;
    case GD::CPP::ErrorSeverity::error:
      msg = GD::Cc::Msg::error;
      break;
    case GD::CPP::ErrorSeverity::fatal_error:
      msg = GD::Cc::Msg::fatal_error;
      break;
    default:
      break;
    }
    return vformat_to(ctx.out(), "{0}", fmt::make_format_args(GD::Cc::Messages::get().get(msg)));
  }
};

#endif  // CC_LIBCPP_ERROR_HH_INCLUDED_
