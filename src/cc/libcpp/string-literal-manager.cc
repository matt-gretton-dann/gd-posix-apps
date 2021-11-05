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

#include "location.hh"
#include "preprocessor-tokenizer.hh"
#include "string-literal-manager.hh"

GD::CPP::StringLiteralManager::StringLiteralManager() = default;

auto GD::CPP::StringLiteralManager::id(std::string const& id) -> StringLiteralID
{
  return map_.find_or_insert(id);
}

auto GD::CPP::StringLiteralManager::display_name(StringLiteralID id) const -> std::string
{
  std::string result = "\"";
  bool last_hex = false;
  for (auto c : map_.get(id)) {
    if (c == U'\n') {
      result += "\\n";
      last_hex = false;
    }
    else if (c == U'\a') {
      result += "\\a";
      last_hex = false;
    }
    else if (c == '\b') {
      result += "\\b";
      last_hex = false;
    }
    else if (c == '\f') {
      result += "\\f";
      last_hex = false;
    }
    else if (c == '\t') {
      result += "\\t";
      last_hex = false;
    }
    else if (c == '\r') {
      result += "\\r";
      last_hex = false;
    }
    else if (c == '\v') {
      result += "\\v";
      last_hex = false;
    }
    else if (c == '\\') {
      result += "\\\\";
      last_hex = false;
    }
    else if (c == '"') {
      result += "\\\"";
      last_hex = false;
    }
    else if (std::isprint(static_cast<int>(c)) != 0 && (!last_hex || !Details::is_hex_digit(c))) {
      result.push_back(static_cast<char>(c));
      last_hex = false;
    }
    else {
      result += fmt::format("\\x{0:02x}", static_cast<unsigned char>(c));
      last_hex = true;
    }
  }

  return result + "\"";
}

GD::CPP::WideStringLiteralManager::WideStringLiteralManager() = default;

auto GD::CPP::WideStringLiteralManager::id(std::u32string const& id) -> WideStringLiteralID
{
  return map_.find_or_insert(id);
}

auto GD::CPP::WideStringLiteralManager::display_name(WideStringLiteralID id) const -> std::string
{
  constexpr char32_t uU_split = 0x10000;  // At uU_split and above we use \U otherwise \u.

  std::string result = "\"";
  bool last_hex = false;
  for (auto c : map_.get(id)) {
    if (c == U'\n') {
      result += "\\n";
      last_hex = false;
    }
    else if (c == U'\a') {
      result += "\\a";
      last_hex = false;
    }
    else if (c == U'\b') {
      result += "\\b";
      last_hex = false;
    }
    else if (c == U'\f') {
      result += "\\f";
      last_hex = false;
    }
    else if (c == U'\t') {
      result += "\\t";
      last_hex = false;
    }
    else if (c == U'\r') {
      result += "\\r";
      last_hex = false;
    }
    else if (c == U'\v') {
      result += "\\v";
      last_hex = false;
    }
    else if (c == U'\\') {
      result += "\\\\";
      last_hex = false;
    }
    else if (c == U'"') {
      result += "\\\"";
      last_hex = false;
    }
    else if (!Details::is_ucn(c)) {
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

  return result + "\"";
}
