/** \file   ar-member.cc
 *  \brief  Implement member for archive.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/filesystem.hh"

#include <assert.h>
#include <istream>
#include <limits>
#include <map>
#include <stddef.h>
#include <string>
#include <vector>

#include "ar.hh"

void GD::Ar::Details::MemberHeader::update_format()
{
  if (name_ == Details::symbol_table_name(Format::bsd)) {
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

bool GD::Ar::Details::MemberHeader::operator==(MemberHeader const& rhs) const noexcept
{
  return name_ == rhs.name_ && mtime_ == rhs.mtime_ && uid_ == rhs.uid_ && gid_ == rhs.gid_ &&
         mode_ == rhs.mode_ && size_ == rhs.size_ && header_size_ == rhs.header_size_ &&
         format_ == rhs.format_;
}

bool GD::Ar::Details::operator!=(MemberHeader const& lhs, MemberHeader const& rhs) noexcept
{
  return !(lhs == rhs);
}

std::string const& GD::Ar::Details::MemberHeader::name() const noexcept { return name_; }
std::time_t GD::Ar::Details::MemberHeader::mtime() const noexcept { return mtime_; }
uid_t GD::Ar::Details::MemberHeader::uid() const noexcept { return uid_; }
gid_t GD::Ar::Details::MemberHeader::gid() const noexcept { return gid_; }
mode_t GD::Ar::Details::MemberHeader::mode() const noexcept { return mode_; }
size_t GD::Ar::Details::MemberHeader::size() const noexcept { return size_; }
size_t GD::Ar::Details::MemberHeader::header_size() const noexcept { return header_size_; }
GD::Ar::Format GD::Ar::Details::MemberHeader::format() const noexcept { return format_; }

GD::Ar::Member::Member(Details::MemberHeader&& header, MemberID id, Data data)
    : header_(std::move(header)), id_(id), data_(data)
{
  assert(data_ != nullptr);
  assert(header_.size() == data_->size());
}

bool GD::Ar::Member::operator==(Member const& rhs) const noexcept
{
  return header_ == rhs.header_ && id_ == rhs.id_ && data_ == rhs.data_;
}

bool GD::Ar::operator!=(Member const& lhs, Member const& rhs) noexcept { return !(lhs == rhs); }

std::string const& GD::Ar::Member::name() const noexcept { return header_.name(); }
std::time_t GD::Ar::Member::mtime() const noexcept { return header_.mtime(); }
uid_t GD::Ar::Member::uid() const noexcept { return header_.uid(); }
gid_t GD::Ar::Member::gid() const noexcept { return header_.gid(); }
mode_t GD::Ar::Member::mode() const noexcept { return header_.mode(); }
size_t GD::Ar::Member::offset_bytes() const noexcept { return offset_; }
size_t GD::Ar::Member::size_bytes() const noexcept { return header_.size(); }
GD::Ar::MemberID GD::Ar::Member::id() const noexcept { return id_; }
GD::Ar::Format GD::Ar::Member::format() const noexcept { return header_.format(); }

std::span<std::byte const> GD::Ar::Member::data() const noexcept
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
template<typename R, typename It>
R from_be(It begin, It end)
{
  R result = 0;
  R shift = sizeof(R) * 8;
  while (begin != end) {
    assert(shift != 0);
    shift -= 8;
    result |= static_cast<R>(*begin++) << shift;
  }
  return result;
}

GD::Ar::SymbolMap get_symbols_bsd([[maybe_unused]] GD::Ar::Member symbol_table)
{
  return GD::Ar::SymbolMap{};
}

GD::Ar::SymbolMap get_symbols_svr4(GD::Ar::Member symbol_table)
{
  auto data = symbol_table.data();
  auto data_it = data.begin();
  uint32_t count = from_be<uint32_t>(data_it, data_it + 4);
  data_it += 4;
  auto strings_it = data_it + count * 4;
  GD::Ar::SymbolMap symbol_map;
  for (uint32_t i = 0; i < count; ++i) {
    GD::Ar::MemberID mid(static_cast<GD::Ar::MemberID>(from_be<uint32_t>(data_it, data_it + 4)));
    data_it += 4;
    auto it = symbol_map.find(mid);
    if (it == symbol_map.end()) {
      auto result =
        symbol_map.insert(std::make_pair(mid, std::make_shared<std::vector<std::string>>()));
      it = result.first;
    }
    std::string ins;
    auto strings_it2 = std::find(strings_it, data.end(), std::byte(0));
    assert(strings_it2 != data.end());
    std::transform(strings_it, strings_it2, std::back_inserter(ins),
                   [](std::byte b) { return static_cast<char>(b); });
    it->second->push_back(ins);
    strings_it = strings_it2 + 1;
  }

  return symbol_map;
}

GD::Ar::SymbolMap get_symbols_win32([[maybe_unused]] GD::Ar::SymbolMap const& symbol_table1,
                                    [[maybe_unused]] GD::Ar::Member symbol_table2)
{
  return GD::Ar::SymbolMap{};
}
}  // namespace

GD::Ar::SymbolMap GD::Ar::Details::get_symbols(Member symbol_table)
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

GD::Ar::SymbolMap GD::Ar::Details::get_symbols([[maybe_unused]] SymbolMap const& symbol_table1,
                                               Member symbol_table2)
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
