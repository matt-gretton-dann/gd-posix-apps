/** \file   parsed-program.cc
 *  \brief  awk Parsed program
 *  \author Copyright 2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/stdlib.h"

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include "awk.hh"

auto GD::Awk::ParsedProgram::begin_instructions() const noexcept -> InstructionIterators
{
  return std::make_pair(begin_.begin(), begin_.end());
}

auto GD::Awk::ParsedProgram::per_record_instructions() const noexcept -> InstructionIterators
{
  return std::make_pair(per_record_.begin(), per_record_.end());
}

auto GD::Awk::ParsedProgram::end_instructions() const noexcept -> InstructionIterators
{
  return std::make_pair(end_.begin(), end_.end());
}

auto GD::Awk::ParsedProgram::function_instructions(std::string const& name) const noexcept
  -> InstructionIterators
{
  auto it{functions_.find(name)};
  if (it == functions_.end()) {
    return std::make_pair(begin_.end(), begin_.end());
  }

  return std::make_pair(it->second.begin(), it->second.end());
}

auto GD::Awk::ParsedProgram::begin() noexcept -> Instructions& { return begin_; }

auto GD::Awk::ParsedProgram::begin() const noexcept -> Instructions const& { return begin_; }

auto GD::Awk::ParsedProgram::per_record() noexcept -> Instructions& { return per_record_; }

auto GD::Awk::ParsedProgram::per_record() const noexcept -> Instructions const&
{
  return per_record_;
}

auto GD::Awk::ParsedProgram::end() noexcept -> Instructions& { return end_; }

auto GD::Awk::ParsedProgram::end() const noexcept -> Instructions const& { return end_; }

auto GD::Awk::ParsedProgram::function(std::string const& name) noexcept -> Instructions&
{
  return functions_.insert({name, Instructions{}}).first->second;
}

auto GD::Awk::ParsedProgram::function(std::string const& name) const -> Instructions const&
{
  auto it{functions_.find(name)};
  if (it == functions_.end()) {
    throw std::logic_error("Function doesn't exist");
  }

  return it->second;
}
