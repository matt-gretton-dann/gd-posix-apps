/** \file   libcpp/error.cc
 *  \brief  Error handling classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "error.hh"

#include <map>
#include <string>

#include "file-store.hh"
#include "location.hh"

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
// NOLINTNEXTLINE
std::map<GD::CPP::ErrorCode, GD::CPP::ErrorSeverity> severities = {
  {EC::too_many_errors, ES::fatal_error},
  {EC::file_error, ES::fatal_error},
  {EC::missing_newline_at_end_of_file, ES::error},
  {EC::splice_followed_by_end_of_file, ES::error},
  {EC::malformed_character_80, ES::error},
  {EC::malformed_character_not_80, ES::error},
  {EC::malformed_character_early_line_end, ES::error},
  {EC::trigraph_replacement, ES::warning},
};

auto get_severity(GD::CPP::ErrorCode code) -> GD::CPP::ErrorSeverity
{
  auto it = severities.find(code);
  if (it == severities.end()) {
    return GD::CPP::ErrorSeverity::ice;
  }

  return it->second;
}
}  // namespace

GD::CPP::ErrorManager::ErrorManager(std::ostream& os) : os_(os) {}

void GD::CPP::ErrorManager::file_store(FileStore& fs) { file_store_ = std::addressof(fs); }

void GD::CPP::ErrorManager::max_error_count(std::uint32_t max_error_count)
{
  max_error_count_ = max_error_count;
}

void GD::CPP::ErrorManager::do_error(ErrorCode code, Range range, std::string msg)
{
  if (error_count_++ >= max_error_count_) {
    code = ErrorCode::too_many_errors;
    msg = GD::Cc::Messages::get().format(GD::Cc::Set::errors, code, max_error_count_);
  }

  auto id = get_id(code);
  auto severity = get_severity(code);
  auto loc = range.begin();
  std::string fname = file_store_ != nullptr ? file_store_->logical_filename(loc) : "??";
  Line line = file_store_ != nullptr ? file_store_->logical_line(loc) : Line{0};
  Column column = file_store_ != nullptr ? file_store_->logical_column(loc) : Column{0};
  auto to_output = fmt::format("{0}:{1}:{2}:{3}:{4}:{5}", fname, line, column, id, severity, msg);
  if (severity == ErrorSeverity::ice) {
    ice("{0}", to_output);
  }
  else {
    os_ << to_output << '\n';
  }
  if (severity == ErrorSeverity::fatal_error) {
    std::exit(1);  // NOLINT(concurrency-mt-unsafe)
  }
}
