/** \file   ar-member.cc
 *  \brief  Implement member for archive.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/filesystem.hh"

#include <assert.h>
#include <istream>
#include <limits>
#include <stddef.h>
#include <string>
#include <vector>

#include "ar.hh"

void GD::Ar::Details::MemberHeader::guess_format()
{
  std::string name(name_.substr(0, name_.find(' ')));
  if (name == Details::symbol_table_name(Format::bsd)) {
    format_ = Format::bsd;
  }
  else if (name == Details::symbol_table_name(Format::darwin)) {
    format_ = Format::darwin;
  }
  else if (name == Details::symbol_table_name(Format::svr4)) {
    format_ = Format::svr4;
  }
  else if (name == Details::symbol_table_name(Format::gnu)) {
    format_ = Format::gnu;
  }
  else if (name == Details::symbol_table_name(Format::gnu_thin)) {
    format_ = Format::gnu_thin;
  }
  else if (name == Details::symbol_table_name(Format::win32)) {
    format_ = Format::win32;
  }
  else if (name.back() == '/') {
    format_ = Format::svr4;
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
uint64_t GD::Ar::Details::MemberHeader::mtime() const noexcept { return mtime_; }
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
uint64_t GD::Ar::Member::mtime() const noexcept { return header_.mtime(); }
uid_t GD::Ar::Member::uid() const noexcept { return header_.uid(); }
gid_t GD::Ar::Member::gid() const noexcept { return header_.gid(); }
mode_t GD::Ar::Member::mode() const noexcept { return header_.mode(); }
size_t GD::Ar::Member::offset_bytes() const noexcept { return offset_; }
size_t GD::Ar::Member::size_bytes() const noexcept { return header_.size(); }
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
