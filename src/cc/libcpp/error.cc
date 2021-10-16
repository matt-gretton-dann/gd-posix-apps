/** \file   libcpp/error.cc
 *  \brief  Error handling classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "error.hh"

#include <map>
#include <string>

namespace {
/** \brief  Get the string ID of an error.  */
auto get_id(GD::CPP::ErrorCode code) -> std::string
{
  constexpr std::size_t num_len = 4;
  auto result = std::string(num_len, '0') +
                std::to_string(static_cast<std::underlying_type_t<GD::CPP::ErrorCode>>(code));
  return std::string{"W"} + result.substr(result.size() - num_len);
}

using EC = GD::CPP::ErrorCode;
using ES = GD::CPP::ErrorSeverity;

/* Map of error codes to severities - should really be auto-generated.  */
std::map<GD::CPP::ErrorCode, GD::CPP::ErrorSeverity> severities = {  // NOLINT
  {EC::too_many_errors, ES::fatal_error},
  {EC::file_error, ES::fatal_error}};

auto get_severity(GD::CPP::ErrorCode code) -> GD::CPP::ErrorSeverity
{
  auto it = severities.find(code);
  if (it == severities.end()) {
    return GD::CPP::ErrorSeverity::ice;
  }

  return it->second;
}
}  // namespace

auto GD::CPP::Error::id() const noexcept -> std::string const& { return id_; }
auto GD::CPP::Error::severity() const noexcept -> ErrorSeverity { return severity_; }
auto GD::CPP::Error::message() const noexcept -> std::string const& { return msg_; }

GD::CPP::Error::Error(std::string id, ErrorSeverity severity, std::string msg)
    : id_(std::move(id)), severity_(severity), msg_(std::move(msg))
{
}

void GD::CPP::ErrorManager::max_error_count(std::uint32_t max_error_count)
{
  max_error_count_ = max_error_count;
}

auto GD::CPP::ErrorManager::do_error(ErrorCode code, std::string msg) -> Error
{
  if (error_count_++ >= max_error_count_) {
    code = ErrorCode::too_many_errors;
    msg = GD::Cc::Messages::get().format(GD::Cc::Set::errors, code, max_error_count_);
  }

  auto id = get_id(code);
  auto severity = get_severity(code);
  if (severity == ErrorSeverity::ice) {
    ice("{0} - {1}", id, msg);
  }
  return {id, severity, std::move(msg)};
}
