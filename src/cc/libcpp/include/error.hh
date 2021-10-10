/** \file   include/error.hh
 *  \brief  Error handling class
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_ERROR_HH_INCLUDED_
#define CC_LIBCPP_ERROR_HH_INCLUDED_

#include "gd/format.hh"

namespace GD::CPP {
/** \brief  List of error codes.  */
enum class ErrorCode {
  no_error,
};

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

  /** \brief         Generate an error
   *  \param   code  Error code
   *  \param   args  Arguments for error message
   *  \return        Error object
   */
  template<typename... Args>
  auto error([[maybe_unused]] ErrorCode code, [[maybe_unused]] Args&&... args) -> Error
  {
    if (error_count_++ >= max_error_count_) {
      return {"X2000", ErrorSeverity::fatal_error, "Too many errors."};
    }

    return {"UNKNOWN", ErrorSeverity::error, "UNKNOWN"};
  }

  /** \brief                 Set the maximum number of errors before we stop producing errors.
   *  \param max_error_count Maximum number of errors.
   */
  void max_error_count(std::uint32_t max_error_count);

private:
  std::uint32_t error_count_ = 0;        ///< Number of errors produced
  std::uint32_t max_error_count_ = 128;  ///< Maximum number of errors
};
}  // namespace GD::CPP

#endif  // CC_LIBCPP_ERROR_HH_INCLUDED_
