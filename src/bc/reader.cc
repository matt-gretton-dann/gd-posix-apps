/** \file   reader.cc
 *  \brief  Implementation of GD::BC::Reader and derived classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include "bc-messages.hh"

#include <cstdint>
#include <limits>
#include <string_view>

#include "bc.hh"

GD::Bc::Location::Location(std::string_view file_name) : file_name_(file_name), column_(1), line_(1)
{
}

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

GD::Bc::Reader::~Reader() = default;

GD::Bc::StringReader::StringReader(std::string_view s) : Reader("Library"), s_(s), pos_(0) {}

auto GD::Bc::StringReader::peek() -> int
{
  if (pos_ >= s_.length()) {
    return EOF;
  }

  return s_[pos_];
}

void GD::Bc::StringReader::do_chew()
{
  if (pos_ < s_.length()) {
    ++pos_;
  }
}

GD::Bc::FileReader::FileReader(std::string_view f) : Reader(f), file_(f), c_(EOF)
{
  if (file_.error()) {
    error(Msg::file_error, file_.filename());
  }
}

auto GD::Bc::FileReader::peek() -> int
{
  /* We only read from the file when we know we need to - which is indicated by c_ being EOF.  */
  if (c_ == EOF && !file_.eof()) {
    c_ = file_.getc();

    if (file_.error()) {
      error(Msg::file_error, file_.filename());
    }
  };
  return c_;
}

void GD::Bc::FileReader::do_chew()
{
  /* Ensure we have something to chew.  */
  (void)peek();

  /* And then clear it... */
  c_ = EOF;
}
