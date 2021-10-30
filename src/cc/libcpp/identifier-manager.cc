/** \file   libcpp/identifier-manager.cc
 *  \brief  Identifier management
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/format.hh"

#include "cc-messages.hh"

#include <cstdint>
#include <iostream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "identifier-manager.hh"
#include "location.hh"
#include "preprocessor-tokenizer.hh"

GD::CPP::IdentifierManager::IdentifierManager() = default;

auto GD::CPP::IdentifierManager::id(std::u32string const& id) -> IdentID
{
  return map_.find_or_insert(id);
}

auto GD::CPP::IdentifierManager::display_name(IdentID id) const -> std::string
{
  return map_.display_name(id);
}
