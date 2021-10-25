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
#include "identifier-manager.hh"
#include "token.hh"
#include "tokenizer.hh"

namespace GD::CPP {

namespace Details {
/** \brief  Is \a c classified as a whitespace character? */
constexpr inline auto is_whitespace(char32_t c) -> bool
{
  return c == U' ' || c == U'\t' || c == U'\f' || c == U'\v';
}

/** \brief  Is this a valid Universal Character name identifier?  */
constexpr inline auto is_ucn_identifier(char32_t c) -> bool
{
  return c == U'\u00A8' || c == U'\u00AA' || c == U'\u00AD' || c == U'\u00AF' ||
         (c >= U'\u00B2' && c <= U'\u00B5') || (c >= U'\u00B7' && c <= U'\u00BA') ||
         (c >= U'\u00BC' && c <= U'\u00BE') || (c >= U'\u00C0' && c <= U'\u00D6') ||
         (c >= U'\u00D8' && c <= U'\u00F6') || (c >= U'\u00F8' && c <= U'\u00FF') ||
         (c >= U'\u0100' && c <= U'\u167F') || (c >= U'\u1681' && c <= U'\u180D') ||
         (c >= U'\u180F' && c <= U'\u1FFF') || (c >= U'\u200B' && c <= U'\u200D') ||
         (c >= U'\u202A' && c <= U'\u202E') || (c >= U'\u203F' && c <= U'\u2040') ||
         c == U'\u2054' || (c >= U'\u2060' && c <= U'\u206F') ||
         (c >= U'\u2070' && c <= U'\u218F') || (c >= U'\u2460' && c <= U'\u24FF') ||
         (c >= U'\u2776' && c <= U'\u2793') || (c >= U'\u2C00' && c <= U'\u2DFF') ||
         (c >= U'\u2E80' && c <= U'\u2FFF') || (c >= U'\u3004' && c <= U'\u3007') ||
         (c >= U'\u3021' && c <= U'\u302F') || (c >= U'\u3031' && c <= U'\u303F') ||
         (c >= U'\u3040' && c <= U'\uD7FF') || (c >= U'\uF900' && c <= U'\uFD3D') ||
         (c >= U'\uFD40' && c <= U'\uFDCF') || (c >= U'\uFDF0' && c <= U'\uFE44') ||
         (c >= U'\uFE47' && c <= U'\uFFFD') || (c >= U'\U00010000' && c <= U'\U0001FFFD') ||
         (c >= U'\U00020000' && c <= U'\U0002FFFD') || (c >= U'\U00030000' && c <= U'\U0003FFFD') ||
         (c >= U'\U00040000' && c <= U'\U0004FFFD') || (c >= U'\U00050000' && c <= U'\U0005FFFD') ||
         (c >= U'\U00060000' && c <= U'\U0006FFFD') || (c >= U'\U00070000' && c <= U'\U0007FFFD') ||
         (c >= U'\U00080000' && c <= U'\U0008FFFD') || (c >= U'\U00090000' && c <= U'\U0009FFFD') ||
         (c >= U'\U000A0000' && c <= U'\U000AFFFD') || (c >= U'\U000B0000' && c <= U'\U000BFFFD') ||
         (c >= U'\U000C0000' && c <= U'\U000CFFFD') || (c >= U'\U000D0000' && c <= U'\U000DFFFD') ||
         (c >= U'\U000E0000' && c <= U'\U000EFFFD');
}

/** \brief  Is this a valid Universal Character name initial identifier?  */
constexpr inline auto is_ucn_initial_identifier(char32_t c) -> bool
{
  bool is_id = is_ucn_identifier(c);
  bool disallowed_first = (c >= U'\u0300' && c <= U'\u036F') ||
                          (c >= U'\u1DC0' && c <= U'\u1DFF') ||
                          (c >= U'\u20D0' && c <= U'\u20FF') || (c >= U'\uFE20' && c <= U'\uFE2F');
  return is_id && !disallowed_first;
}

/** \brief Is this a non-digit?  */
constexpr inline auto is_nondigit(char32_t c) -> bool
{
  /* This range check is safe because char32_t is a Unicode encoded set where A-Z & a-z are
   * contiguous.  */
  return c == '_' || (c >= U'A' && c <= U'Z') || (c >= U'a' && c <= u'z');
}

/** \brief Is this a digit?  */
constexpr inline auto is_digit(char32_t c) -> bool
{
  /* This range check is safe because char32_t is a Unicode encoded set where A-Z & a-z are
   * contiguous.  */
  return c >= '0' && c <= '9';
}

constexpr inline auto is_hex_digit(char32_t c) -> bool
{
  return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

constexpr inline auto hex_digit_value(char32_t c) -> unsigned
{
  constexpr unsigned offset = 10;
  if (c >= '0' && c <= '9') {
    return c - '0';
  }
  if (c >= 'A' && c <= 'F') {
    return c - 'A' + offset;
  }
  if (c >= 'a' && c <= 'f') {
    return c - 'a' + offset;
  }

  return std::numeric_limits<unsigned>::max();
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
  PreprocessorTokenizer(Parent& parent, ErrorManager& em, IdentifierManager& id)
      : Tokenizer<PreprocessorTokenizer<Parent>, Parent>(parent), error_manager_(em),
        identifier_manager_(id)
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

  auto read_char32_hex_digits(Parent& parent, unsigned count) -> char32_t
  {
    assert_ice(count > 0 && count < 9, "Can only read up to 8 hex digits.");
    auto begin = parent.peek().range().begin();
    char32_t result = 0;
    for (unsigned i = 0; i < count; ++i) {
      auto const& t = parent.peek();
      if (t != TokenType::character || !Details::is_hex_digit(t.character())) {
        error_manager_.error(ErrorCode::not_enough_hex_digits, Range{begin, t.range().end()}, count,
                             i);
        return U'\ufffd';
      }

      result *= 16;  // NOLINT
      result += Details::hex_digit_value(t.character());
      parent.chew();
    }

    return result;
  }

  auto parse_identifier(Parent& parent) -> Token
  {
    std::u32string id{};
    auto begin = parent.peek().range().begin();
    auto end = begin;
    while (true) {
      auto const& t = parent.peek();
      if (t != TokenType::character) {
        break;
      }
      char32_t c = t.character();
      if (Details::is_ucn_identifier(c)) {
        error_manager_.error(ErrorCode::explicit_ucn_implementation_defined, t.range(),
                             to_string(c));
        parent.chew();
      }
      else if (c == U'\\') {
        Token back_slash{t};
        parent.chew();
        if (parent.peek() != U'U' && parent.peek() != U'u') {
          pending_ = back_slash;
          break;
        }

        constexpr unsigned Ulen = 8;
        constexpr unsigned ulen = 4;
        unsigned count = parent.peek() == U'U' ? Ulen : ulen;
        parent.chew();
        c = read_char32_hex_digits(parent, count);
        if (!Details::is_ucn_identifier(c)) {
          pending_ = Token{TokenType::character,
                           Range{back_slash.range().begin(), parent.peek().range().begin()}, c};
          break;
        }
      }
      else if (Details::is_digit(c) || Details::is_nondigit(c)) {
        parent.chew();
      }
      else {
        break;
      }

      id.push_back(c);
      end = t.range().end();
    }

    return Token{TokenType::identifier, Range{begin, end}, identifier_manager_.id(id)};
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
      if (Details::is_nondigit(c) || Details::is_ucn_initial_identifier(c)) {
        return parse_identifier(parent);
      }
    }
    return std::nullopt;
  }

  ErrorManager& error_manager_;                 ///< Error manager
  IdentifierManager& identifier_manager_;       ///< Identifier manager
  std::optional<Token> pending_{std::nullopt};  ///< Pending token
};

}  // namespace GD::CPP

#endif  // LIBCPP_INCLUDE_PREPROCESS_TOKENIZER_HH_
