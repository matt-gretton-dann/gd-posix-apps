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
  constexpr char32_t uU_split = 0x10000;  // At uU_split and above we use \U otherwise \u.

  std::string result;
  bool last_hex = false;
  for (auto c : map_.get(id)) {
    if (!Details::is_ucn(c)) {
      if (std::isprint(static_cast<int>(c)) != 0 && (!last_hex || !Details::is_hex_digit(c))) {
        result.push_back(static_cast<char>(c));
        last_hex = false;
      }
      else {
        result += fmt::format("\\x{0:x}", static_cast<uint32_t>(c));
        last_hex = true;
      }
    }
    else if (c < uU_split) {
      result += fmt::format("\\u{0:04x}", static_cast<uint32_t>(c));
      last_hex = false;
    }
    else {
      result += fmt::format("\\U{0:08x}", static_cast<uint32_t>(c));
      last_hex = false;
    }
  }

  return result;
}
