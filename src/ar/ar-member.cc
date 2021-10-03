/** \file   ar-member.cc
 *  \brief  Implement member for archive.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/filesystem.hh"

#include "util/utils.hh"

#include <cassert>
#include <cstddef>
#include <istream>
#include <limits>
#include <map>
#include <string>
#include <utility>
#include <vector>

#include "ar.hh"

void GD::Ar::Details::MemberHeader::update_format()
{
  if (name_ == Details::symbol_table_name(Format::bsd)) {  // NOLINT(bugprone-branch-clone)
    format_ = Format::bsd;
  }
  else if (name_ == Details::symbol_table_name(Format::darwin)) {
    format_ = Format::darwin;
  }
  else if (name_ == Details::symbol_table_name(Format::svr4)) {
    format_ = Format::svr4;
  }
  else if (name_ == Details::symbol_table_name(Format::gnu)) {
    format_ = Format::gnu;
  }
  else if (name_ == Details::symbol_table_name(Format::gnu_thin)) {
    format_ = Format::gnu_thin;
  }
  else if (name_ == Details::symbol_table_name(Format::win32)) {
    format_ = Format::win32;
  }
  else if (name_.back() == '/') {
    format_ = Format::svr4;
    name_.erase(name_.length() - 1);
  }
  else {
    format_ = Format::bsd;
  }
}

auto GD::Ar::Details::MemberHeader::operator==(MemberHeader const& rhs) const noexcept -> bool
{
  return name_ == rhs.name_ && mtime_ == rhs.mtime_ && uid_ == rhs.uid_ && gid_ == rhs.gid_ &&
         mode_ == rhs.mode_ && size_ == rhs.size_ && header_size_ == rhs.header_size_ &&
         format_ == rhs.format_;
}

auto GD::Ar::Details::operator!=(MemberHeader const& lhs, MemberHeader const& rhs) noexcept -> bool
{
  return !(lhs == rhs);
}

auto GD::Ar::Details::MemberHeader::name() const noexcept -> std::string const& { return name_; }
auto GD::Ar::Details::MemberHeader::mtime() const noexcept -> std::time_t { return mtime_; }
auto GD::Ar::Details::MemberHeader::uid() const noexcept -> uid_t { return uid_; }
auto GD::Ar::Details::MemberHeader::gid() const noexcept -> gid_t { return gid_; }
auto GD::Ar::Details::MemberHeader::mode() const noexcept -> mode_t { return mode_; }
auto GD::Ar::Details::MemberHeader::size() const noexcept -> size_t { return size_; }
auto GD::Ar::Details::MemberHeader::header_size() const noexcept -> size_t { return header_size_; }
auto GD::Ar::Details::MemberHeader::format() const noexcept -> GD::Ar::Format { return format_; }

GD::Ar::Member::Member(Details::MemberHeader&& header, MemberID id, Data data, Symbols symbols)
    : header_(std::move(header)), id_(id), offset_(0), data_(std::move(std::move(data))),
      symbols_(std::move(std::move(symbols)))
{
  assert(data_ != nullptr);                 // NOLINT
  assert(header_.size() == data_->size());  // NOLINT
}

auto GD::Ar::Member::operator==(Member const& rhs) const noexcept -> bool
{
  return header_ == rhs.header_ && id_ == rhs.id_ && data_ == rhs.data_;
}

auto GD::Ar::operator!=(Member const& lhs, Member const& rhs) noexcept -> bool
{
  return !(lhs == rhs);
}

auto GD::Ar::Member::name() const noexcept -> std::string const& { return header_.name(); }
auto GD::Ar::Member::mtime() const noexcept -> std::time_t { return header_.mtime(); }
auto GD::Ar::Member::uid() const noexcept -> uid_t { return header_.uid(); }
auto GD::Ar::Member::gid() const noexcept -> gid_t { return header_.gid(); }
auto GD::Ar::Member::mode() const noexcept -> mode_t { return header_.mode(); }
auto GD::Ar::Member::offset_bytes() const noexcept -> size_t { return offset_; }
auto GD::Ar::Member::size_bytes() const noexcept -> size_t { return header_.size(); }
auto GD::Ar::Member::id() const noexcept -> GD::Ar::MemberID { return id_; }
auto GD::Ar::Member::format() const noexcept -> GD::Ar::Format { return header_.format(); }
auto GD::Ar::Member::symbols() const noexcept -> GD::Ar::Symbols { return symbols_; }

auto GD::Ar::Member::data() const noexcept -> std::span<std::byte const>
{
  return std::span<std::byte const>(*data_);
}

void GD::Ar::Member::offset_bytes(std::size_t offset)
{
  if (size_bytes() < offset) {
    throw std::runtime_error("Tried to set offset outside member.");
  }
  offset_ = offset;
}

namespace {

auto get_symbols_bsd([[maybe_unused]] GD::Ar::Member const& symbol_table) -> GD::Ar::SymbolMap
{
  return GD::Ar::SymbolMap{};
}

auto get_symbols_svr4(GD::Ar::Member const& symbol_table) -> GD::Ar::SymbolMap
{
  auto data = symbol_table.data();
  auto data_it = data.begin();
  auto count = GD::read_be<std::uint32_t>(data_it);
  data_it += 4;
  auto strings_it = data_it + count * 4;
  GD::Ar::SymbolMap symbol_map;
  for (uint32_t i = 0; i < count; ++i) {
    auto mid(static_cast<GD::Ar::MemberID>(GD::read_be<std::uint32_t>(data_it)));
    data_it += 4;
    auto it = symbol_map.find(mid);
    if (it == symbol_map.end()) {
      auto result =
        symbol_map.insert(std::make_pair(mid, std::make_shared<std::vector<std::string>>()));
      it = result.first;
    }
    std::string ins;
    auto strings_it2 = std::find(strings_it, data.end(), std::byte(0));
    assert(strings_it2 != data.end());  // NOLINT
    std::transform(strings_it, strings_it2, std::back_inserter(ins),
                   [](std::byte b) { return static_cast<char>(b); });
    it->second->push_back(ins);
    strings_it = strings_it2 + 1;
  }

  return symbol_map;
}

auto get_symbols_win32([[maybe_unused]] GD::Ar::SymbolMap const& symbol_table1,
                       [[maybe_unused]] GD::Ar::Member const& symbol_table2) -> GD::Ar::SymbolMap
{
  return GD::Ar::SymbolMap{};
}
}  // namespace

auto GD::Ar::Details::get_symbols(Member const& symbol_table) -> GD::Ar::SymbolMap
{
  switch (symbol_table.format()) {
  case Format::bsd:
  case Format::darwin:
    return get_symbols_bsd(symbol_table);
  case Format::gnu:
  case Format::gnu_thin:
  case Format::svr4:
  case Format::win32:
    return get_symbols_svr4(symbol_table);
  default:
    abort();
  }
}

auto GD::Ar::Details::get_symbols([[maybe_unused]] SymbolMap const& symbol_table1,
                                  Member const& symbol_table2) -> GD::Ar::SymbolMap
{
  switch (symbol_table2.format()) {
  case Format::bsd:
  case Format::darwin:
  case Format::gnu:
  case Format::gnu_thin:
  case Format::svr4:
    throw std::runtime_error("Unexpected number of symbol tables.");
  case Format::win32:
    return get_symbols_win32(symbol_table1, symbol_table2);
  default:
    abort();
  }
}
