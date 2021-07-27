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

namespace {
constexpr std::size_t NAME_LEN = 16;
constexpr std::size_t MTIME_LEN = 12;
constexpr std::size_t UID_LEN = 6;
constexpr std::size_t GID_LEN = 6;
constexpr std::size_t MODE_LEN = 6;
constexpr std::size_t SIZE_LEN = 10;
constexpr std::size_t FMAG_LEN = 2;

template<typename It>
void check_no_trailing_nonspace(It begin, It end)
{
  std::for_each(begin, end, [](char c) {
    if (c != ' ') {
      throw std::runtime_error("Non-trailing space in member header.");
    }
  });
}

template<typename T, unsigned Base = 10>
T to_number(std::string_view v, T def = 0)
{
  static_assert(Base <= 10);

  if (v.length() > std::numeric_limits<T>::digits) {
    throw std::logic_error("String can overflow digit type.");
  }

  /* Skip any leading whitespace. */
  auto it = std::find_if_not(v.begin(), v.end(), [](char c) { return c == ' '; });
  if (it == v.end()) {
    return def;
  }

  /* Convert to unsigned integer.  */
  T result = 0;
  it = std::find_if_not(it, v.end(), [&result](char c) {
    if (c >= '0' && c <= '0' + Base - 1) {
      result *= Base;
      result += c - '0';
      return true;
    }
    return false;
  });

  check_no_trailing_nonspace(it, v.end());
  return result;
}

constexpr bool inline_long_names(GD::Ar::Format format)
{
  return format == GD::Ar::Format::bsd || format == GD::Ar::Format::darwin;
}

constexpr char long_name_terminator(GD::Ar::Format format)
{
  switch (format) {
  case GD::Ar::Format::bsd:
  case GD::Ar::Format::darwin:
    assert(false);
    return std::numeric_limits<char>::max();
  case GD::Ar::Format::svr4:
  case GD::Ar::Format::gnu:
  case GD::Ar::Format::gnu_thin:
    return '/';
  case GD::Ar::Format::win32:
    return '\0';
  default:
    abort();
  }
}

constexpr char short_name_terminator(GD::Ar::Format format)
{
  switch (format) {
  case GD::Ar::Format::bsd:
  case GD::Ar::Format::darwin:
    return ' ';
  case GD::Ar::Format::svr4:
  case GD::Ar::Format::gnu:
  case GD::Ar::Format::gnu_thin:
  case GD::Ar::Format::win32:
    return '/';
  default:
    abort();
  }
}
}  // namespace

std::string GD::Ar::Details::MemberHeader::read_str(std::istream& is, std::size_t len)
{
  std::string result;
  result.resize(len);
  is.read(result.data(), len);
  if (is.fail()) {
    throw std::runtime_error("Member header is too short.");
  }
  header_size_ += len;
  return result;
}

template<typename T, unsigned Base>
T GD::Ar::Details::MemberHeader::read_uint(std::istream& is, std::size_t len, T def)
{
  std::string v = read_str(is, len);
  return to_number<T, Base>(v, def);
}

void GD::Ar::Details::MemberHeader::update_name(std::istream& is, Format format,
                                                std::shared_ptr<Member const> long_names)
{
  if (inline_long_names(format)) {
    /* Long name is represented by #1/<Len> in name field with name immediately following header. */
    if (name_.substr(0, 3) == "#1/") {
      auto length = to_number<std::size_t>(name_.substr(3), std::string::npos);
      if (length == std::string::npos) {
        throw std::runtime_error("Bad string length");
      }
      name_ = read_str(is, length);
      return;
    }
  }
  else {
    /* Out of line long names: Represented by /<offset> in name field.  With name at <offset> in
     * long names field.  Terminated by the long_name_terminator character.  */
    if (name_[0] == '/') {
      auto offset = to_number<std::size_t>(name_.substr(1), std::string::npos);
      if (offset == std::string::npos) {
        throw std::runtime_error("Bad string offset into long names");
      }
      if (long_names == nullptr) {
        throw std::runtime_error("No long name archive found.");
      }
      auto data = long_names->data();
      if (data->size() < offset) {
        throw std::runtime_error("Offset out of range for long names.");
      }
      auto it = data->begin() + offset;
      auto it_end = std::find(it, data->end(), std::byte(long_name_terminator(format)));
      if (it_end == data->end()) {
        throw std::runtime_error("Unterminated string in long names table.");
      }
      std::transform(it, it_end, name_.begin(), [](std::byte b) { return char(b); });
      return;
    }
  }

  /* Short names.  */
  auto it = std::find(name_.begin(), name_.end(), short_name_terminator(format));
  if (it != name_.end()) {
    check_no_trailing_nonspace(it + 1, name_.end());
  }
  name_.erase(it, name_.end());
  return;
}

GD::Ar::Details::MemberHeader::MemberHeader(std::istream& is, Format format,
                                            std::shared_ptr<Member const> long_names)
{
  header_size_ = 0;
  name_ = read_str(is, NAME_LEN);
  mtime_ = read_uint<uint64_t>(is, MTIME_LEN);
  uid_ = read_uint<uid_t>(is, UID_LEN, -1);
  gid_ = read_uint<uid_t>(is, GID_LEN, -1);
  mode_ = read_uint<mode_t, 8>(is, MODE_LEN);
  size_ = read_uint<std::size_t>(is, SIZE_LEN);
  std::string fmag = read_str(is, FMAG_LEN);
  if (fmag[0] != 96 || fmag[1] != 10) {
    throw std::runtime_error("Bad end of header magic");
  }
  update_name(is, format, long_names);
}

std::string const& GD::Ar::Details::MemberHeader::name() const noexcept { return name_; }
uint64_t GD::Ar::Details::MemberHeader::mtime() const noexcept { return mtime_; }
uid_t GD::Ar::Details::MemberHeader::uid() const noexcept { return uid_; }
gid_t GD::Ar::Details::MemberHeader::gid() const noexcept { return gid_; }
mode_t GD::Ar::Details::MemberHeader::mode() const noexcept { return mode_; }
size_t GD::Ar::Details::MemberHeader::size() const noexcept { return size_; }
size_t GD::Ar::Details::MemberHeader::header_size() const noexcept { return header_size_; }

GD::Ar::Member::Member(Details::MemberHeader&& header, std::size_t(offset), Data data)
    : header_(std::move(header)), offset_(offset), data_(data)
{
  assert(header_.size() == data_->size());
}

std::string const& GD::Ar::Member::name() const noexcept { return header_.name(); }
uint64_t GD::Ar::Member::mtime() const noexcept { return header_.mtime(); }
uid_t GD::Ar::Member::uid() const noexcept { return header_.uid(); }
gid_t GD::Ar::Member::gid() const noexcept { return header_.gid(); }
mode_t GD::Ar::Member::mode() const noexcept { return header_.mode(); }
size_t GD::Ar::Member::offset() const noexcept { return offset_; }
size_t GD::Ar::Member::size() const noexcept { return header_.size(); }
GD::Ar::Member::Data GD::Ar::Member::data() const { return data_; }
