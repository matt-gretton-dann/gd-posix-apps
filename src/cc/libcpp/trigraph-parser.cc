/** \file   libcpp/trigraph-parser.cc
 *  \brief  Trigraph Tokenizing functions and classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "token.hh"
#include "tokenizers.hh"

auto GD::CPP::Details::trigraph_replacement(Token const& t) noexcept -> char32_t
{
  if (t.type() != TokenType::character) {
    return U'\0';
  }
  switch (t.character()) {
  case U'=':
    return U'#';
  case U'(':
    return U'[';
  case U'/':
    return U'\\';
  case U')':
    return U']';
  case U'\'':
    return U'^';
  case U'<':
    return U'{';
  case U'!':
    return U'|';
  case U'>':
    return U'}';
  case U'-':
    return U'~';
  default:
    return U'\0';
  }
}
