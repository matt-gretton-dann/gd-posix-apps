/** \file   libcpp/ppnumber-manager.cc
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

#include "location.hh"
#include "ppnumber-manager.hh"
#include "preprocessor-tokenizer.hh"

GD::CPP::PPNumberManager::PPNumberManager() = default;

auto GD::CPP::PPNumberManager::id(std::u32string const& id) -> PPNumberID
{
  return map_.find_or_insert(id);
}

auto GD::CPP::PPNumberManager::display_name(PPNumberID id) const -> std::string
{
  return map_.display_name(id);
}
