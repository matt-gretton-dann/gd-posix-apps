/** \file   reader.cc
 *  \brief  Implementation of GD::BC::Reader and derived classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include "bc-messages.hh"

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <string_view>

#include "bc.hh"

GD::Bc::Location::Location(std::string_view file_name) : file_name_(file_name), column_(1), line_(1)
{
}

// NOLINTNEXTLINE
GD::Bc::Location::Location(std::string_view file_name, Line line, Column column)
    : file_name_(file_name), column_(column), line_(line)
{
}

auto GD::Bc::Location::file_name() const -> std::string const& { return file_name_; }

auto GD::Bc::Location::column() const -> GD::Bc::Location::Column { return column_; }

auto GD::Bc::Location::line() const -> GD::Bc::Location::Line { return line_; }

void GD::Bc::Location::next_column()
{
  if (column_ < std::numeric_limits<Column>::max()) {
    ++column_;
  }
}

void GD::Bc::Location::next_column(std::size_t count)
{
  auto const room = static_cast<std::size_t>(std::numeric_limits<Column>::max() - column_);
  column_ += static_cast<Column>(std::min(count, room));
}

void GD::Bc::Location::next_line()
{
  if (line_ < std::numeric_limits<Line>::max()) {
    ++line_;
  }
  column_ = 1;
}

auto GD::Bc::operator<<(std::ostream& os, GD::Bc::Location const& location) -> std::ostream&
{
  os << location.file_name() << ':' << location.line() << ':' << location.column();
  return os;
}

auto GD::Bc::operator==(GD::Bc::Location const& lhs, GD::Bc::Location const& rhs) -> bool
{
  return lhs.file_name() == rhs.file_name() && lhs.line() == rhs.line() &&
         lhs.column() == rhs.column();
}

auto GD::Bc::operator!=(GD::Bc::Location const& lhs, GD::Bc::Location const& rhs) -> bool
{
  return !(lhs == rhs);
}

GD::Bc::Reader::Reader(std::string_view name) : location_(name) {}

void GD::Bc::Reader::chew()
{
  if (peek() == '\n') {
    location_.next_line();
  }
  else {
    location_.next_column();
  }

  do_chew();
}

auto GD::Bc::Reader::location() const -> GD::Bc::Location const& { return location_; }

void GD::Bc::Reader::chew_prefix(std::size_t size)
{
  for (std::size_t i = 0; i != size; ++i) {
    chew();
  }
}

void GD::Bc::Reader::advance_location(std::string_view text)
{
  for (auto newline = text.find('\n'); newline != std::string_view::npos;
       newline = text.find('\n')) {
    location_.next_line();
    text.remove_prefix(newline + 1);
  }
  location_.next_column(text.size());
}

GD::Bc::Reader::~Reader() = default;

GD::Bc::StringReader::StringReader(std::string_view s) : Reader("Library"), s_(s) {}

auto GD::Bc::StringReader::peek() -> int
{
  if (pos_ >= s_.length()) {
    return EOF;
  }

  return static_cast<unsigned char>(s_[pos_]);
}

void GD::Bc::StringReader::do_chew()
{
  if (pos_ < s_.length()) {
    ++pos_;
  }
}

auto GD::Bc::StringReader::contiguous() -> std::string_view { return s_.substr(pos_); }

void GD::Bc::StringReader::chew_prefix(std::size_t size)
{
  assert(size <= s_.size() - pos_);
  advance_location(s_.substr(pos_, size));
  pos_ += size;
}

GD::Bc::FileReader::FileReader(std::string_view f)
    : Reader(f), file_(f), buffered_(file_.is_regular_file())
{
  check_error();
}

void GD::Bc::FileReader::check_error()
{
  if (file_.error()) {
    std::cerr << error(Msg::file_error, file_.filename());
    std::exit(1);  // NOLINT(concurrency-mt-unsafe)
  }
}

auto GD::Bc::FileReader::peek() -> int
{
  if (buffered_) {
    auto const available = contiguous();
    return available.empty() ? EOF : static_cast<unsigned char>(available.front());
  }

  /* We only read from the file when we know we need to - which is indicated by c_ being EOF.  */
  if (c_ == EOF && !file_.eof()) {
    // Each named FILE is private to this sequential reader. Borrowed stdin
    // keeps ordinary locking inside the utility, including across readers.
    c_ = file_.getc_exclusive();

    check_error();
  };
  return c_;
}

void GD::Bc::FileReader::do_chew()
{
  if (buffered_) {
    if (position_ != end_) {
      ++position_;
    }
    return;
  }
  /* Ensure we have something to chew.  */
  (void)peek();

  /* And then clear it... */
  c_ = EOF;
}

auto GD::Bc::FileReader::contiguous() -> std::string_view
{
  if (!buffered_) {
    return {};
  }
  if (position_ == end_) {
    end_ = file_.read_block(buffer_.data(), buffer_.size());
    position_ = 0;
    if (end_ == 0) {
      check_error();
    }
  }
  return {buffer_.data() + position_, end_ - position_};
}

void GD::Bc::FileReader::chew_prefix(std::size_t size)
{
  assert(buffered_ && size <= end_ - position_);
  advance_location({buffer_.data() + position_, size});
  position_ += size;
}

GD::Bc::StdinReader::StdinReader() : Reader("-"), file_("-") {}

auto GD::Bc::StdinReader::peek() -> int
{
#if defined(_WIN32)
  // Retain the generic reader on Windows. Native timing found unrelated
  // comparison regressions with the status-query change in the MSVC image.
  if (c_ == EOF && !file_.eof()) {
    c_ = file_.getc();
    if (file_.error()) {
      std::cerr << error(Msg::file_error, file_.filename());
      std::exit(1);  // NOLINT(concurrency-mt-unsafe)
    }
  }
#else
  if (c_ == EOF && !at_end_) {
    c_ = file_.getc();
    // A successful read needs no separate locked stream status query.
    if (c_ == EOF) {
      if (file_.error()) {
        std::cerr << error(Msg::file_error, file_.filename());
        std::exit(1);  // NOLINT(concurrency-mt-unsafe)
      }
      at_end_ = true;
    }
  }
#endif
  return c_;
}

void GD::Bc::StdinReader::do_chew() { c_ = EOF; }
