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
  constexpr char32_t uU_split = 0x10000;  // At uU_split and above we use \U otherwise \u.

  std::string result;
  for (auto c : map_.get(id)) {
    if (Details::is_nondigit(c) || Details::is_digit(c)) {
      result.push_back(static_cast<char>(c));
    }
    else if (c < uU_split) {
      result += fmt::format("\\u{0:04x}", static_cast<uint32_t>(c));
    }
    else {
      result += fmt::format("\\U{0:08x}", static_cast<uint32_t>(c));
    }
  }

  return result;
}
