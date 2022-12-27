/** \file   reader.cc
 *  \brief  Implementation of GD::BC::Reader and derived classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include "awk-messages.hh"

#include <cstdint>
#include <limits>
#include <string_view>

#include "awk.hh"

GD::Awk::Location::Location(std::string_view file_name)
    : file_name_(file_name), column_(1), line_(1)
{
}

// NOLINTNEXTLINE
GD::Awk::Location::Location(std::string_view file_name, Line line, Column column)
    : file_name_(file_name), column_(column), line_(line)
{
}

auto GD::Awk::Location::file_name() const -> std::string const& { return file_name_; }

auto GD::Awk::Location::column() const -> GD::Awk::Location::Column { return column_; }

auto GD::Awk::Location::line() const -> GD::Awk::Location::Line { return line_; }

void GD::Awk::Location::next_column()
{
  if (column_ < std::numeric_limits<Column>::max()) {
    ++column_;
  }
}

void GD::Awk::Location::next_line()
{
  if (line_ < std::numeric_limits<Line>::max()) {
    ++line_;
  }
  column_ = 1;
}

auto GD::Awk::operator<<(std::ostream& os, GD::Awk::Location const& location) -> std::ostream&
{
  os << location.file_name() << ':' << location.line() << ':' << location.column();
  return os;
}

auto GD::Awk::operator==(GD::Awk::Location const& lhs, GD::Awk::Location const& rhs) -> bool
{
  return lhs.file_name() == rhs.file_name() && lhs.line() == rhs.line() &&
         lhs.column() == rhs.column();
}

auto GD::Awk::operator!=(GD::Awk::Location const& lhs, GD::Awk::Location const& rhs) -> bool
{
  return !(lhs == rhs);
}

GD::Awk::Reader::Reader(std::string_view name) : location_(name) {}

void GD::Awk::Reader::chew()
{
  if (peek() == '\n') {
    location_.next_line();
  }
  else {
    location_.next_column();
  }

  do_chew();
}

auto GD::Awk::Reader::location() const -> GD::Awk::Location const& { return location_; }

GD::Awk::Reader::~Reader() = default;

GD::Awk::StringReader::StringReader(std::string_view s) : Reader("Library"), s_(s) {}

auto GD::Awk::StringReader::peek() -> int
{
  if (pos_ >= s_.length()) {
    return EOF;
  }

  return s_[pos_];
}

void GD::Awk::StringReader::do_chew()
{
  if (pos_ < s_.length()) {
    ++pos_;
  }
}

GD::Awk::FileReader::FileReader(std::string_view f) : Reader(f), file_(f)
{
  if (file_.error()) {
    error(Msg::file_error, file_.filename());
  }
}

auto GD::Awk::FileReader::peek() -> int
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

void GD::Awk::FileReader::do_chew()
{
  /* Ensure we have something to chew.  */
  (void)peek();

  /* And then clear it... */
  c_ = EOF;
}

GD::Awk::FilesReader::FilesReader(std::vector<std::string> f)
    : Reader(""), current_file_(nullptr), files_(std::move(f)), location_("")
{
  open_front_file();
}

void GD::Awk::FilesReader::open_front_file()
{
  if (files_.empty()) {
    return;
  }

  current_file_ = std::make_unique<GD::StreamInputFile>(files_.front());
  location_ = Location(files_.front());

  if (current_file_->error()) {
    error(Msg::file_error, files_.front());
  }

  files_.erase(files_.begin());
}

auto GD::Awk::FilesReader::peek() -> int
{
  /* We only read from the file when we know we need to - which is indicated by c_ being EOF.  */
  if (c_ != EOF) {
    return c_;
  }

  /* At the end of the current file - then open the next and use that.  */
  if (current_file_->eof() && !files_.empty()) {
    open_front_file();
  }

  if (!current_file_->eof()) {
    c_ = current_file_->getc();

    if (c_ == '\n') {
      location_.next_line();
    }
    else {
      location_.next_column();
    }
  }

  if (current_file_->error()) {
    error(Msg::file_error, current_file_->filename());
  }

  return c_;
}

void GD::Awk::FilesReader::do_chew()
{
  /* Ensure we have something to chew.  */
  (void)peek();

  /* And then clear it... */
  c_ = EOF;
}

auto GD::Awk::FilesReader::location() const -> GD::Awk::Location const& { return location_; }
