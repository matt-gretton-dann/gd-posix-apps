/** \file   awk.hh
 *  \brief  Header file for awk
 *  \author Copyright 2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef SRC_AWK_AWK_HH_INCLUDED
#define SRC_AWK_AWK_HH_INCLUDED

#include "gd/nl_types.h"

#include "gd/format.hh"

#include "util/file.hh"
#include "util/utils.hh"

#include "awk-messages.hh"

#include <array>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <sstream>
#include <stack>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

namespace GD::Awk {

/** \brief  Token type.
 *
 * Very basic token type stores type along with any extra info needed.
 */
struct Token
{
  /** \brief  Enum listing token types.  */
  enum class Type {
    error,
    eof,
    newline,
    string,
    letter,
    number,
    power,
    multiply,
    divide,
    modulo,
    add,
    subtract,
    assign,
    add_assign,
    subtract_assign,
    multiply_assign,
    divide_assign,
    modulo_assign,
    power_assign,
    equals,
    less_than_equals,
    greater_than_equals,
    not_equals,
    less_than,
    greater_than,
    increment,
    decrement,
    define,
    break_,
    halt,
    quit,
    length,
    return_,
    for_,
    if_,
    while_,
    abs,
    sqrt,
    scale,
    ibase,
    obase,
    auto_,
    lparens,
    rparens,
    lsquare,
    rsquare,
    lbrace,
    rbrace,
    comma,
    semicolon,
  };

  /** \brief      Construct a token of basic type
   *  \param type Token type (shouldn't be Type::letter, Type::number or Type::string)
   */
  explicit Token(Type type);

  /** \brief      Construct a token of type Type::number, Type::string, or Type::error.
   *  \param type Token type
   *  \param s    Value of number or string
   *
   * Numbers are stored as strings as we can't interpret their value until we know the input base
   * until execution time.
   */
  Token(Type type, std::string const& s);

  /** \brief Get token type.  */
  [[nodiscard]] auto type() const -> Type;

  /** \brief  Get string stored in token. */
  [[nodiscard]] auto string() const -> std::string const&;

  /** \brief  Get number stored in token. */
  [[nodiscard]] auto number() const -> std::string const&;

  /** \brief  Get the error message.  */
  [[nodiscard]] auto error() const -> std::string const&;

  /** \brief  Output debug form of token. */
  void debug(std::ostream& os) const;

  /** \brief  Is this an assignment op? */
  [[nodiscard]] auto is_assign_op() const -> bool;

  /** \brief  Is this an increment/decrement op?  */
  [[nodiscard]] auto is_incr_decr_op() const -> bool;

  /** \brief  Is this a multiplication style op?  */
  [[nodiscard]] auto is_mul_op() const -> bool;

  /** \brief  Is this an addition style op?  */
  [[nodiscard]] auto is_add_op() const -> bool;

  /** \brief  Is this a relation op? */
  [[nodiscard]] auto is_rel_op() const -> bool;

private:
  /** Internal type to hold a number and diferentiate it from string and error.  */
  using NumInt = TypeWrapper<std::string, struct NumIntType>;

  /** Internal type to hold an error string and differentiate it from string and number.  */
  using ErrorInt = TypeWrapper<std::string, struct ErrorIntType>;

  std::variant<Type, std::string, NumInt, ErrorInt> value_;
};

/** \brief Output operator for token type.  */
auto operator<<(std::ostream& os, Token::Type t) -> std::ostream&;
auto operator<<(std::ostream& os, Token const& token) -> std::ostream&;

/* Comparison operators.  */
auto operator==(Token const& token, Token::Type type) -> bool;
auto operator==(Token::Type type, Token const& token) -> bool;
auto operator!=(Token const& token, Token::Type type) -> bool;
auto operator!=(Token::Type type, Token const& token) -> bool;

/** \brief  A source location.  */
class Location  // NOLINT(bugprone-exception-escape)
{
public:
  using Column = unsigned;
  using Line = unsigned;

  /** \brief  Construct a source location at the first column of the first line of a file.
   *  \param file_name Name of file being processed.
   */
  explicit Location(std::string_view file_name);

  /** \brief Location constructor.  */
  Location(std::string_view file_name, Line line, Column column);

  /** Get file name. */
  [[nodiscard]] auto file_name() const -> std::string const&;

  /** Get column.  */
  [[nodiscard]] auto column() const -> Column;

  /** Get line.  */
  [[nodiscard]] auto line() const -> Line;

  /** \brief Move to next column.
   *
   * We silently saturate to the maximum value of Column.
   */
  void next_column();

  /** \brief Move to next line, and reset column to 1.
   *
   * We silently saturate to the maximum value of Line.
   */
  void next_line();

private:
  std::string file_name_;  ///< File name
  Column column_;          ///< Column number
  Line line_;              ///< Line number
};

auto operator<<(std::ostream& os, Location const& location) -> std::ostream&;

auto operator==(Location const& lhs, Location const& rhs) -> bool;
auto operator!=(Location const& lhs, Location const& rhs) -> bool;

/** \brief  Reader base class.
 *
 * Base class that allows us to read characters from an input stream (either string or file).
 */
class Reader
{
public:
  /** \brief      Constructor a parser
   *  \param name Name to use for file in error messages.
   */
  explicit Reader(std::string_view name);

  /** \brief Abstract destructor.  */
  virtual ~Reader() = 0;

  Reader(Reader const&) = default;
  Reader(Reader&&) noexcept = default;
  auto operator=(Reader const&) -> Reader& = default;
  auto operator=(Reader&&) noexcept -> Reader& = default;

  /** \brief  Peek at the current character.
   *  \return Current character or EOF on end-of-file.
   *
   * Implementations should be lazy - that is not do the peeking until called, hence this method
   * not being `const`.
   */
  virtual auto peek() -> int = 0;

  /** \brief  Chew the current character. */
  void chew();

  /** \brief  Get the current source location.  */
  [[nodiscard]] virtual auto location() const -> Location const&;

  /** \brief  Report an error giving source location. */
  template<typename... Ts>
  auto error(Msg msg, Ts... args) -> std::string
  {
    std::ostringstream os;
    os << Messages::get().get(Set::awk, Msg::error_label) << ":" << location_ << ": "
       << Messages::get().format(Set::awk, msg, args...) << '\n';
    return os.str();
  }

private:
  /** \brief  Do the actual chew of a character.  */
  virtual void do_chew() = 0;

  Location location_;  ///< Current source location.
};

/** \brief Parse a string. */
struct StringReader : public Reader
{
public:
  /** \brief Constructor
   *  \param s String to parse - it must stay valid for length of life of object.
   */
  explicit StringReader(std::string_view s);

  auto peek() -> int final;

private:
  void do_chew() final;

  std::string_view s_;                  ///< String
  std::string_view::size_type pos_{0};  ///< Current position in string.
};

/** \brief File parser. */
class FileReader : public Reader
{
public:
  /** \brief Constructor.
   *  \param f File name.
   */
  explicit FileReader(std::string_view f);

  auto peek() -> int final;

private:
  void do_chew() final;

  GD::StreamInputFile file_;  ///< File
  int c_{EOF};                ///< Current character (EOF means needs peeking or EOF)
};

/** \brief Read from multiple files as if they were one. */
class FilesReader : public Reader
{
public:
  /** \brief   Constructor.
   *  \param f Vector of files.
   */
  explicit FilesReader(std::vector<std::string> f);

  auto peek() -> int final;
  [[nodiscard]] auto location() const -> Location const& override;

private:
  void do_chew() final;

  /** \brief  Open the file at the top of files_.  */
  void open_front_file();

  std::unique_ptr<GD::StreamInputFile> current_file_;  ///< File
  std::vector<std::string> files_;                     ///< List of files we're reading from.
  Location location_;                                  ///< Current location
  int c_{EOF};  ///< Current character (EOF means needs peeking or EOF)
};

/** Lexer.  */
class Lexer
{
public:
  /** \brief   Constructor.
   *  \param r Reader to read from.
   */
  explicit Lexer(std::unique_ptr<Reader>&& r);

  /** \brief   Peek the current token.
   *  \return  Peeked token.
   *
   * Not const as we peek lazily.
   */
  auto peek() -> Token const&;

  /** \brief  Chew the current token. */
  void chew();

  /** \brief  Get the current location. */
  [[nodiscard]] auto location() const -> Location const&;

  /** \brief  Generate an error message with current location information added.  */
  template<typename... Ts>
  auto error(Msg msg, Ts... args) -> std::string
  {
    return r_->error(msg, args...);
  }

private:
  /** Lexer.
   *
   * All lexing routines end up by calling t_.emplace() to construct an appropriate token. */
  void lex();

  void lex_string();
  void lex_number();
  void lex_letter_or_keyword();
  void lex_assign_or_equals();
  void lex_not_equals();
  void lex_comment();

  /**              Lex a symbol
   *  \param plain Token type to use if the next character isn't matched
   *  \param next1 Next character to try and match
   *  \param tok1  Token type to use if next1 matches
   */
  void lex_symbol(Token::Type plain, char next1, Token::Type tok1);

  /**              Lex a symbol
   *  \param plain Token type to use if the next character isn't matched
   *  \param next1 Next character to try and match
   *  \param tok1  Token type to use if next1 matches
   *  \param next2 Next character to try and match
   *  \param tok2  Token type to use if next2 matches
   */
  void lex_symbol(Token::Type plain, char next1, Token::Type tok1, char next2, Token::Type tok2);

  std::unique_ptr<Reader> r_;   ///< Reader
  std::optional<Token> t_;      ///< Pending token.
  bool first_character_{true};  ///< Have we lexed anything yet?
};

/** \brief Parsing class.  */
class Parser
{
public:
  using Offset = std::size_t;  ///< Offset into instructions list.
  using Index = std::size_t;   ///< Index into instructions list.

  /** \brief             Constructor
   *  \param lexer       Lexer.
   *  \param interactive Do we need to execute after every top-level input?
   */
  explicit Parser(std::unique_ptr<Lexer>&& lexer);

  /** \brief  Do the next stage of a parse.
   */
  auto parse();  //-> std::shared_ptr<Instructions>;

private:
  /** \brief       Insert error message into stream.
   *  \param  msg  Message ID
   *  \param  args Arguments for the message.
   *  \return      Offset of last instruction of error.
   */
  template<typename... Ts>
  auto insert_error(Msg msg, Ts... args)
  {
    error_ = true;
    /* If the lexer holds an error token we report that rather than the error message we've been
     * asked to report.
     */
    bool const lexer_error = lexer_->peek() == Token::Type::error;
    auto s = insert_string(lexer_error ? lexer_->peek().error() : lexer_->error(msg, args...));
    if (lexer_error) {
      lexer_->chew();
    }
    // insert_print(s, Instruction::Stream::error);
    // return insert_quit(1);
  }

  std::unique_ptr<Lexer> lexer_;  ///< Lexer
  bool error_{false};             ///< Has there been an error?
  bool in_function_{false};       ///< Are we in a function?
  bool seen_quit_{false};         ///< Have we seen a quit token?
};

namespace Details {
/* Forward declaration of class that holds VMState.  */
struct VMState;
}  // namespace Details

}  // namespace GD::Awk

template<>
struct fmt::formatter<GD::Awk::Token>
{
  static constexpr auto parse(format_parse_context& ctx)
  {
    if (ctx.begin() != ctx.end() && *ctx.begin() != '}') {
      throw format_error("invalid format");
    }
    return ctx.begin();
  }

  template<typename FormatContext>
  auto format(GD::Awk::Token const& token, FormatContext& ctx)
  {
    std::ostringstream os;
    os << token;
    // Work around Win32 STL bug:
    return fmt::vformat_to(ctx.out(), "{0}", fmt::make_format_args(os.str()));
  }
};

#endif  // SRC_AWK_AWK_HH_INCLUDED