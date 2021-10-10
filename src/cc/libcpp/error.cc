/** \file   cpp/error.cc
 *  \brief  Error handling classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "error.hh"

#include <string>

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
