/** \file   libcpp/include/preprocessor-tokenizer.hh
 *  \brief  Basic tokenizers
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBCPP_INCLUDE_PREPROCESS_TOKENIZER_HH_
#define LIBCPP_INCLUDE_PREPROCESS_TOKENIZER_HH_
#include <optional>

#include "error.hh"
#include "file-store.hh"
#include "token.hh"
#include "tokenizer.hh"

namespace GD::CPP {

namespace Details {
constexpr inline auto is_whitespace(char32_t c) -> bool
{
  return c == U' ' || c == U'\t' || c == U'\f' || c == U'\v';
}

}  // namespace Details

/** \brief         Preprocessor tokenizer
 *  \tparam Parent Parent class type to call to get tokens.
 *
 * Parses everything into one of the preprocessing-tokens.  Will also generate white_space tokens.
 *
 * White-space and comments are merged into one whitespace token per-sequence.
 */
template<typename Parent>
class PreprocessorTokenizer : public Tokenizer<PreprocessorTokenizer<Parent>, Parent>
{
public:
  PreprocessorTokenizer(Parent& parent, ErrorManager& em)
      : Tokenizer<PreprocessorTokenizer<Parent>, Parent>(parent), error_manager_(em)
  {
  }

private:
  friend class Tokenizer<PreprocessorTokenizer<Parent>, Parent>;

  auto parse_whitespace(Parent& parent) -> Token
  {
    Location begin = parent.peek().range().begin();
    Location end = begin;
    while (parent.peek().type() == TokenType::character &&
           Details::is_whitespace(parent.peek().character())) {
      end = parent.peek().range().end();
      parent.chew();
    }

    return {TokenType::white_space, Range{begin, end}};
  }

  auto do_peek(Parent& parent) -> std::optional<Token>
  {
    auto const& t = parent.peek();
    if (t == TokenType::character) {
      char32_t c = t.character();
      if (Details::is_whitespace(c)) {
        return parse_whitespace(parent);
      }
    }
    return std::nullopt;
  }

  ErrorManager& error_manager_;
};

}  // namespace GD::CPP

#endif  // LIBCPP_INCLUDE_PREPROCESS_TOKENIZER_HH_
