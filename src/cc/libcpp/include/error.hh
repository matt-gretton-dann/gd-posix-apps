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
#include <utility>

namespace GD::CPP {
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

/** \brief  Class containing error information.
 *
 * Use ErrorManager to create these an error.
 */
class Error
{
public:
  Error() = delete;
  Error(Error const&) = default;
  Error(Error&&) noexcept = default;
  ~Error() = default;

  auto operator=(Error const&) -> Error& = default;
  auto operator=(Error&&) noexcept -> Error& = default;

  auto id() const noexcept -> std::string const&;
  auto severity() const noexcept -> ErrorSeverity;
  auto message() const noexcept -> std::string const&;

private:
  friend class ErrorManager;
  /** \brief          Construct an error
   *  \param id       Error ID
   *  \param severity Severity
   *  \param msg      Message string
   */
  Error(std::string id, ErrorSeverity severity, std::string msg);

  std::string id_;          ///< Error ID.
  ErrorSeverity severity_;  ///< Error severity.
  std::string msg_;         ///< Error message.
};

/** \brief  Error Manager
 *
 * This manages errors, handling translation of error codes into error messages, and their severity.
 */
class ErrorManager
{
public:
  ErrorManager() = default;
  ErrorManager(ErrorManager const&) = delete;
  ErrorManager(ErrorManager&&) noexcept = default;
  auto operator=(ErrorManager const&) -> ErrorManager& = delete;
  auto operator=(ErrorManager&&) noexcept -> ErrorManager& = default;
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
    std::cerr << "Aborting compiler.\n";
    std::abort();
  }

  /** \brief         Generate an error
   *  \param   code  Error code
   *  \param   args  Arguments for error message
   *  \return        Error object
   */
  template<typename... Args>
  auto error([[maybe_unused]] ErrorCode code, [[maybe_unused]] Args&&... args) -> Error
  {
    return do_error(code, GD::Cc::Messages::get().format(GD::Cc::Set::errors, code, args...));
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
  auto do_error(ErrorCode code, std::string msg) -> Error;

  std::uint32_t error_count_ = 0;        ///< Number of errors produced
  std::uint32_t max_error_count_ = 128;  ///< Maximum number of errors
};
}  // namespace GD::CPP

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
  constexpr auto parse(format_parse_context& ctx)
  {
    auto it = ctx.begin();
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
