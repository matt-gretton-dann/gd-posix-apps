/** \file   libcpp/include/preprocessor-tokenizer.hh
 *  \brief  Basic tokenizers
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBCPP_INCLUDE_PREPROCESS_TOKENIZER_HH_
#define LIBCPP_INCLUDE_PREPROCESS_TOKENIZER_HH_
#include <optional>

#include "character-classifiers.hh"
#include "error.hh"
#include "file-store.hh"
#include "identifier-manager.hh"
#include "token.hh"
#include "tokenizer.hh"

namespace GD::CPP {

/** \brief         Preprocessor tokenizer
 *  \tparam Parent Parent class type to call to get tokens.
 *  \param  parent Parent tokenizer
 *  \param  em     Error manager
 *  \param  id     Identifier manager
 *  \param  ppm    PPNumber manager
 *
 * Parses everything into one of the preprocessing-tokens.  Will also generate white_space tokens.
 *
 * White-space and comments are merged into one whitespace token per-sequence.
 */
template<typename Parent>
class PreprocessorTokenizer : public Tokenizer<PreprocessorTokenizer<Parent>, Parent>
{
public:
  PreprocessorTokenizer(Parent& parent, ErrorManager& em, IdentifierManager& id,
                        PPNumberManager& ppm)
      : Tokenizer<PreprocessorTokenizer<Parent>, Parent>(parent), error_manager_(em),
        identifier_manager_(id), ppnumber_manager_(ppm)
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

  /** \brief         Read hex digits returning the characters they represent.
   *  \param  parent Parent tokenizer
   *  \param  count  Number of digits to read in range [1, 8].
   *  \return        Character read.
   *
   * On entry the parent should point to first character to read.
   */
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

  /** \brief         Read hex digits returning the number they represent.
   *  \param  parent Parent tokenizer
   *  \return        Character read.
   *
   * On entry the parent should point to first character to read.
   */
  auto read_hex_escape(Parent& parent, Location begin) -> std::uint32_t
  {
    std::uint32_t result = 0;
    while (true) {
      auto const& t = parent.peek();
      if (t != TokenType::character || !Details::is_hex_digit(t.character())) {
        return result;
      }

      std::uint32_t temp = result;
      result *= 16;               // NOLINT
      if (result / 16 != temp) {  // NOLINT
        error_manager_.error(ErrorCode::overflow_in_hex_escape, Range{begin, t.range().end()});
        return temp;
      }

      result += Details::hex_digit_value(t.character());
      parent.chew();
    }

    return result;
  }

  /** \brief         Read hex digits returning the number they represent.
   *  \param  parent Parent tokenizer
   *  \return        Character read.
   *
   * On entry the parent should point to first character to read.
   */
  auto read_octal_escape(Parent& parent, Location begin) -> std::uint32_t
  {
    std::uint32_t result = 0;
    unsigned count = 0;
    while (count < 3) {
      auto const& t = parent.peek();
      if (t != TokenType::character || !Details::is_octal_digit(t.character())) {
        return result;
      }

      result *= 8;  // NOLINT
      result += t.character() - U'0';
      parent.chew();
      ++count;
    }

    if (count == 3 && parent.peek() == TokenType::character &&
        Details::is_octal_digit(parent.peek().character())) {
      error_manager_.error(ErrorCode::overlong_octal_escape,
                           Range{begin, parent.peek().range().end()});
    }

    return result;
  }

  /** \brief         Parse a PP-number.
   *  \param  parent Parent tokenizer.
   *  \param  first  First token in PPNumber.
   *  \return        Token to return.
   *
   * parent should point to one past first.
   */
  auto parse_ppnumber(Parent& parent, Token const& first) -> Token
  {
    assert_ice(first == TokenType::character, "First token must be a character");
    std::u32string id{};
    auto begin = first.range().begin();
    auto end = first.range().end();
    id.push_back(first.character());
    bool sign_allowed = false;
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
          /* This is not a UCN identifier, terminate the in-progress ID and return it.  Or if we
           * have an empty identifier return the character.
           */
          pending_.emplace(TokenType::character,
                           Range{back_slash.range().begin(), parent.peek().range().begin()}, c);
          break;
        }
      }
      else if (Details::is_digit(c) || Details::is_nondigit(c) || c == U'.' ||
               (sign_allowed && Details::is_sign(c))) {
        parent.chew();
      }
      else {
        break;
      }

      sign_allowed = c == U'E' || c == U'P' || c == U'e' || c == U'p';
      id.push_back(c);
      end = t.range().end();
    }

    return Token{TokenType::ppnumber, Range{begin, end}, ppnumber_manager_.id(id)};
  }

  /** \brief         Parse an identifier.
   *  \param  parent Parent tokenizer.
   *  \param  id     Already parsed part of identifier
   *  \param  begin  Location of start of token
   *  \return        Token to return
   *
   * If the first character being parsed is a '\' may return a character token if it doesn't become
   * a proper UCN.
   *
   * Maybe called when we've had to disambiguate a token from being an identifier or something else
   * (for example: Usa).
   */
  auto parse_identifier(Parent& parent, std::u32string id, Location begin) -> Token
  {
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
          if (id.empty()) {
            return back_slash;
          }

          pending_ = back_slash;
          break;
        }

        constexpr unsigned Ulen = 8;
        constexpr unsigned ulen = 4;
        unsigned count = parent.peek() == U'U' ? Ulen : ulen;
        parent.chew();
        c = read_char32_hex_digits(parent, count);
        bool valid =
          id.empty() ? Details::is_ucn_initial_identifier(c) : Details::is_ucn_identifier(c);
        if (!valid) {
          /* This is not a UCN identifier, terminate the in-progress ID and return it.  Or if we
           * have an empty identifier return the character.
           */
          auto result = Token{TokenType::character,
                              Range{back_slash.range().begin(), parent.peek().range().begin()}, c};
          if (id.empty()) {
            return result;
          }

          pending_ = result;
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

  auto parse_escape_sequence(Parent& parent) -> std::uint32_t
  {
    assert_ice(parent.peek() == U'\\', "Escape sequence parsing should point to opening \\.");
    auto begin = parent.peek().range().begin();
    parent.chew(TokenType::character);

    auto const& t = parent.peek();
    auto end = t.range().end();

    if (t != TokenType::character) {
      error_manager_.error(ErrorCode::unrecognised_escape_sequence, Range{begin, end}, t);
      return U'\\';
    }

    auto c = t.character();

    switch (c) {
    case U'\'':
      parent.chew();
      return static_cast<unsigned char>('\'');
    case U'\"':
      parent.chew();
      return static_cast<unsigned char>('\"');
    case U'\?':
      parent.chew();
      return static_cast<unsigned char>('\?');
    case U'\\':
      parent.chew();
      return static_cast<unsigned char>('\\');
    case U'a':
      parent.chew();
      return static_cast<unsigned char>('\a');
    case U'b':
      parent.chew();
      return static_cast<unsigned char>('\b');
    case U'f':
      parent.chew();
      return static_cast<unsigned char>('\f');
    case U'n':
      parent.chew();
      return static_cast<unsigned char>('\n');
    case U'r':
      parent.chew();
      return static_cast<unsigned char>('\r');
    case U't':
      parent.chew();
      return static_cast<unsigned char>('\t');
    case U'u':
      parent.chew();
      return static_cast<std::uint32_t>(read_char32_hex_digits(parent, 4));
    case U'U':
      parent.chew();
      return static_cast<std::uint32_t>(read_char32_hex_digits(parent, 8));  // NOLINT
    case U'v':
      parent.chew();
      return static_cast<unsigned char>('\v');
    case U'x':
      parent.chew();
      return read_hex_escape(parent, begin);
    case U'0':
    case U'1':
    case U'2':
    case U'3':
    case U'4':
    case U'5':
    case U'6':
    case U'7':
      return read_octal_escape(parent, begin);
    default:
      error_manager_.error(ErrorCode::unrecognised_escape_sequence, Range{begin, end}, t);
      parent.chew();
      return static_cast<unsigned char>(c);
    }
  }

  auto parse_char_literal(Parent& parent, TokenType type, Location begin) -> Token
  {
    assert_ice(parent.peek() == U'\'', "Char literal parsing should point to opening \'.");
    parent.chew(TokenType::character);

    std::uint32_t result = 0;
    Location end = parent.peek().range().end();
    auto const& t = parent.peek();
    if (t == U'\\') {
      result = parse_escape_sequence(parent);
    }
    else if (t == U'\'') {
      auto end = t.range().end();
      parent.chew();
      if (parent.peek() == U'\'') {
        result = static_cast<unsigned char>('\'');
        error_manager_.error(ErrorCode::literal_quote_not_valid_in_character_literal,
                             Range{begin, parent.peek().range().end()});
      }
      else {
        result = 0;
        error_manager_.error(ErrorCode::empty_character_literal_not_valid,
                             Range{begin, parent.peek().range().end()});
        /* Don't want to do any of the other tidy up - just return.  */
        return {type, Range{begin, end}, std::uint32_t{0}};
      }
    }
    else if (t == U'\n') {
      result = static_cast<unsigned char>('\n');
      error_manager_.error(ErrorCode::newline_not_valid_in_character_literal,
                           Range{begin, parent.peek().range().end()});
      parent.chew();
    }
    else if (t == TokenType::character) {
      result = static_cast<unsigned char>(t.character());
      parent.chew();
      if (parent.peek() != U'\'') {
        error_manager_.error(ErrorCode::too_many_characters_in_character_literal,
                             Range{begin, parent.peek().range().end()});
      }
    }

    /* Find the closing '.  This may not be the most immediate token if we're in an error
     * condition.
     */
    while (parent.peek() != U'\'' && parent.peek() != TokenType::end_of_include &&
           parent.peek() != TokenType::end_of_source) {
      parent.chew();
    }

    if (parent.peek() == TokenType::end_of_include || parent.peek() == TokenType::end_of_source) {
      error_manager_.error(ErrorCode::unterminated_character_literal,
                           Range{begin, parent.peek().range().end()});
    }
    else {
      /* Chew the closing quote. */
      parent.chew();
    }

    std::uint32_t max{0};
    switch (type) {
    case TokenType::char_literal:
      max = std::numeric_limits<unsigned char>::max();
      break;
    case TokenType::char16_literal:
      max = std::numeric_limits<char16_t>::max();
      break;
    case TokenType::char32_literal:
      max = std::numeric_limits<char32_t>::max();
      break;
    case TokenType::wchar_literal:
      assert_ice(false, "TARGET NOT YET IMPLEMENTED");
      max = std::numeric_limits<wchar_t>::max();
      break;
    default:
      max = 0;
      break;
    }

    if (result > max) {
      error_manager_.error(ErrorCode::character_literal_out_of_range, Range{begin, end}, result,
                           max);
    }

    return {type, Range{begin, end}, result};
  }

  /** \brief  Parse tokens that begin with a U.  */
  auto parse_L(Parent& parent) -> Token
  {
    assert_ice(parent.peek() == U'L', "L parsing needs to start by pointing at L.");
    auto begin{parent.peek().range().begin()};
    parent.chew();

    if (parent.peek() == '\'') {
      return parse_char_literal(parent, TokenType::wchar_literal, begin);
    }

    return parse_identifier(parent, U"L", begin);
  }

  /** \brief  Parse tokens that begin with a U.  */
  auto parse_U(Parent& parent) -> Token
  {
    assert_ice(parent.peek() == U'U', "U parsing needs to start by pointing at U.");
    auto begin{parent.peek().range().begin()};
    parent.chew();

    if (parent.peek() == '\'') {
      return parse_char_literal(parent, TokenType::char32_literal, begin);
    }

    return parse_identifier(parent, U"U", begin);
  }

  /** \brief  Parse tokens that begin with a u.  */
  auto parse_u(Parent& parent) -> Token
  {
    assert_ice(parent.peek() == U'u', "u parsing needs to start by pointing at u.");
    auto begin{parent.peek().range().begin()};
    parent.chew();

    if (parent.peek() == '\'') {
      return parse_char_literal(parent, TokenType::char16_literal, begin);
    }

    return parse_identifier(parent, U"u", begin);
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

      if (Details::is_digit(c)) {
        auto first{t};
        parent.chew();
        return parse_ppnumber(parent, first);
      }

      if (c == U'.') {
        auto first{t};
        parent.chew();
        if (parent.peek() == TokenType::character && Details::is_digit(parent.peek().character())) {
          return parse_ppnumber(parent, first);
        }

        return first;
      }

      if (c == U'\'') {
        return parse_char_literal(parent, TokenType::char_literal, t.range().begin());
      }

      if (c == U'L') {
        return parse_L(parent);
      }

      if (c == U'U') {
        return parse_U(parent);
      }

      if (c == U'u') {
        return parse_u(parent);
      }

      if (c == U'\\' || Details::is_nondigit(c) || Details::is_ucn_initial_identifier(c)) {
        return parse_identifier(parent, U"", parent.peek().range().begin());
      }
    }
    return std::nullopt;
  }

  ErrorManager& error_manager_;                 ///< Error manager
  IdentifierManager& identifier_manager_;       ///< Identifier manager
  PPNumberManager& ppnumber_manager_;           ///< PPNumber manager
  std::optional<Token> pending_{std::nullopt};  ///< Pending token
};

}  // namespace GD::CPP

#endif  // LIBCPP_INCLUDE_PREPROCESS_TOKENIZER_HH_
