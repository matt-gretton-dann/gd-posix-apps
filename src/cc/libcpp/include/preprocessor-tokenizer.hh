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

  /** \brief         Parse a line comment (// ...)
   *  \param  begin  Begin location of first / in comment
   *  \param  parent Parent tokenizer
   *  \return        Whitespace token.  parent is updated to point at newline after end of comment
   *
   * On entry parent should be point at second / of comment start.
   */
  auto parse_line_comment(Location begin, Parent& parent) -> Token
  {
    assert_ice(parent.peek() == U'/', "parse_line_comment should point to second slash on entry.");

    Location end = parent.peek().range().end();
    while (true) {
      Token const& t = parent.peek();
      if (t == TokenType::character) {
        if (t == U'\n') {
          // New line means end of comment.  Return whitespace for the comment.
          return {TokenType::white_space, Range{begin, end}};
        }

        // Not a newline continue to chew.
        parent.chew();
      }
      else {
        error_manager_.error(ErrorCode::unterminated_line_comment, Range{begin, end});
        return {TokenType::white_space, Range{begin, end}};
      }
    }
  }

  /** \brief         Parse a multi-line comment (/ * ... * /)
   *  \param  begin  Begin location of first / in comment
   *  \param  parent Parent tokenizer
   *  \return        Whitespace token.  parent is updated to point at token after end of comment
   *
   * On entry parent should be point at second * of comment start.
   */
  auto parse_multiline_comment(Location begin, Parent& parent) -> Token
  {
    assert_ice(parent.peek() == U'*', "parse_multiline_comment should point to * on entry.");

    Location end = parent.peek().range().end();
    parent.chew();

    enum class CommentState { normal, star, end };
    CommentState state = CommentState::normal;
    while (state != CommentState::end) {
      Token const& t = parent.peek();
      end = t.range().end();
      if (t == TokenType::character) {
        if (t == U'*') {
          state = CommentState::star;
        }
        else if (t == U'/' && state == CommentState::star) {
          state = CommentState::end;
        }
        else {
          state = CommentState::normal;
        }
        parent.chew();
      }
      else {
        error_manager_.error(ErrorCode::unterminated_multiline_comment, Range{begin, end});
        state = CommentState::end;
      }
    }

    return {TokenType::white_space, Range{begin, end}};
  }

  /** \brief         Parse tokens that start with a slash.
   *  \param  parent Parent tokenizer.
   *  \return        Token.
   */
  auto parse_slash(Parent& parent) -> Token
  {
    auto pending{parent.peek()};
    assert_ice(parent.peek() == U'/', "parse_slash must point to an initial slash.");

    auto begin = pending.range().begin();
    parent.chew();
    Token const& t2 = parent.peek();
    if (t2 == U'/') {
      return parse_line_comment(begin, parent);
    }
    if (t2 == U'*') {
      return parse_multiline_comment(begin, parent);
    }

    return pending;
  }

  /* \brief         Parse tokens that start with whitespace or a slash.
   * \param  parent Parent token
   * \return        Token
   *
   * We do white-space and slashes together so that we can merge comments together.
   */
  auto parse_whitespace_and_slash(Parent& parent) -> Token
  {
    Location begin = parent.peek().range().begin();
    Location end = begin;
    bool first_token = true;
    while (true) {
      auto const& t = parent.peek();
      if (t == TokenType::character && Details::is_whitespace(t.character())) {
        end = t.range().end();
        parent.chew();
      }
      else if (t == U'/') {
        auto slash_token = parse_slash(parent);
        if (slash_token == TokenType::white_space) {
          /* Token was just white-space merge it into the white-space token we're creating.  */
          end = t.range().end();
        }
        else {
          if (first_token) {
            /* If this is the first token we're processing just return it. */
            return slash_token;
          }

          /* We have a white-space token to return, so make this token pending, and then break.  */
          pending_ = slash_token;
          break;
        }
      }
      else {
        break;
      }
      first_token = false;
    }
    return {TokenType::white_space, Range{begin, end}};
  }

  auto do_peek(Parent& parent) -> std::optional<Token>
  {
    if (pending_) {
      /* There is a pending token so we just return it and reset. */
      auto t{*pending_};
      pending_.reset();
      return t;
    }

    auto const& t = parent.peek();
    if (t == TokenType::character) {
      char32_t c = t.character();
      if (Details::is_whitespace(c) || c == U'/') {
        return parse_whitespace_and_slash(parent);
      }
    }
    return std::nullopt;
  }

  ErrorManager& error_manager_;                 ///< Error manager
  std::optional<Token> pending_{std::nullopt};  ///< Pending token
};

}  // namespace GD::CPP

#endif  // LIBCPP_INCLUDE_PREPROCESS_TOKENIZER_HH_
