/** \file   ar-member.cc
 *  \brief  Implement member for archive.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/filesystem.hh"

#include <cassert>
#include <cstddef>
#include <istream>
#include <limits>
#include <string>
#include <vector>

#include "ar.hh"

void GD::Ar::Details::MemberHeader::guess_format()
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
  else {
    auto rit = std::find_if_not(name_.rbegin(), name_.rend(), [](char c) { return c == ' '; });
    if (rit == name_.rend()) {
      throw std::runtime_error("Archive member with no name!");
    }
    else if (*rit == '/') {
      format_ = Format::svr4;
    }
    else {
      format_ = Format::bsd;
    }
  }
}

std::string const& GD::Ar::Details::MemberHeader::name() const noexcept { return name_; }
uint64_t GD::Ar::Details::MemberHeader::mtime() const noexcept { return mtime_; }
uid_t GD::Ar::Details::MemberHeader::uid() const noexcept { return uid_; }
gid_t GD::Ar::Details::MemberHeader::gid() const noexcept { return gid_; }
mode_t GD::Ar::Details::MemberHeader::mode() const noexcept { return mode_; }
size_t GD::Ar::Details::MemberHeader::size() const noexcept { return size_; }
size_t GD::Ar::Details::MemberHeader::header_size() const noexcept { return header_size_; }

GD::Ar::Member::Member(Details::MemberHeader&& header, MemberID id, Data data)
    : header_(std::move(header)), id_(id), data_(data)
{
  assert(data_ != nullptr);
  assert(header_.size() == data_->size());
}

std::string const& GD::Ar::Member::name() const noexcept { return header_.name(); }
uint64_t GD::Ar::Member::mtime() const noexcept { return header_.mtime(); }
uid_t GD::Ar::Member::uid() const noexcept { return header_.uid(); }
gid_t GD::Ar::Member::gid() const noexcept { return header_.gid(); }
mode_t GD::Ar::Member::mode() const noexcept { return header_.mode(); }
size_t GD::Ar::Member::offset_bytes() const noexcept { return offset_; }
size_t GD::Ar::Member::size_bytes() const noexcept { return header_.size(); }
std::span<std::byte const> GD::Ar::Member::data() const noexcept
{
  return std::span(data_->begin(), header_.size());
}

void GD::Ar::Member::offset_bytes(std::size_t offset)
{
  if (size_bytes() < offset) {
    throw std::runtime_error("Tried to set offset outside member.");
  }
  offset_ = offset;
}
