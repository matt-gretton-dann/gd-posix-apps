/** \file   reader.cc
 *  \brief  Implementation of GD::BC::Reader and derived classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include "bc-messages.hh"

#include <stdint.h>

#include "bc.hh"
#include <string_view>

GD::Bc::Reader::Reader(std::string_view name) : name_(name), line_(1), column_(1) {}

void GD::Bc::Reader::chew()
{
  if (peek() == '\n') {
    ++line_;
    column_ = 0;
  }

  ++column_;
  do_chew();
}

GD::Bc::Reader::~Reader() {}

GD::Bc::StringReader::StringReader(std::string_view s) : Reader("Library"), s_(s), pos_(0) {}

int GD::Bc::StringReader::peek()
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

int GD::Bc::FileReader::peek()
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
