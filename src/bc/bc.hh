/** \file   bc.hh
 *  \brief  Header file for bc
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_BC_BC_HH_INCLUDED
#define _SRC_BC_BC_HH_INCLUDED

#include "gd/format.hh"

#include "util/file.hh"
#include "util/utils.hh"

#include "bc-messages.hh"

#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <variant>

#include <type_traits>

namespace GD::Bc {

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
    quit,
    length,
    return_,
    for_,
    if_,
    while_,
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

  /** \brief      Construct a token of type Type::letter
   *  \param type Type::letter
   *  \param l    Letter
   */
  Token(Type type, char l);

  /** \brief Get token type.  */
  Type type() const;

  /** \brief  Get string stored in token. */
  std::string const& string() const;

  /** \brief  Get number stored in token. */
  std::string const& number() const;

  /** \brief Get letter stored in token. */
  char letter() const;

  /** \brief  Get the error message.  */
  std::string const& error() const;

  /** \brief  Output debug form of token. */
  void debug(std::ostream& os) const;

private:
  /** Internal type to hold a number and diferentiate it from string and error.  */
  struct NumInt
  {
    std::string num_;
  };

  /** Internal type to hold an error string and differentiate it from string and number.  */
  struct ErrorInt
  {
    std::string error_;
  };

  std::variant<Type, std::string, NumInt, ErrorInt, char> value_;
};

/** \brief Output operator for token type.  */
std::ostream& operator<<(std::ostream& os, Token::Type t);
std::ostream& operator<<(std::ostream& os, Token const& token);

/* Comparison operators.  */
bool operator==(Token const& token, Token::Type type);
bool operator==(Token::Type type, Token const& token);
bool operator!=(Token const& token, Token::Type type);
bool operator!=(Token::Type type, Token const& token);

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
  Reader(std::string_view name);

  /** \brief Abstract destructor.  */
  virtual ~Reader() = 0;

  /** \brief  Peek at the current character.
   *  \return Current character or EOF on end-of-file.
   *
   * Implementations should be lazy - that is not do the peeking until called, hence this method
   * not being `const`.
   */
  virtual int peek() = 0;

  /** \brief  Chew the current character. */
  void chew();

  /** \brief  Report an error giving source location. */
  template<typename... Ts>
  std::string error(Msg msg, Ts... args)
  {
    std::ostringstream os;
    os << GD::Bc::Messages::get().get(GD::Bc::Set::bc, Msg::error_label) << ":" << name_ << ':'
       << line_ << ':' << column_ << ": "
       << GD::Bc::Messages::get().format(GD::Bc::Set::bc, msg, args...) << '\n';
    return os.str();
  }

private:
  /** \brief  Do the actual chew of a character.  */
  virtual void do_chew() = 0;

  std::string name_;  ///< Name of thing we're parsing.
  ::size_t line_;     ///< Current line number.
  ::size_t column_;   ///< Current column number
};

/** \brief Parse a string. */
struct StringReader : public Reader
{
public:
  /** \brief Constructor
   *  \param s String to parse - it must stay valid for length of life of object.
   */
  StringReader(std::string_view s);

  int peek() override final;

private:
  void do_chew() override final;

  std::string_view s_;               ///< String
  std::string_view::size_type pos_;  ///< Current position in string.
};

/** \brief File parser. */
class FileReader : public Reader
{
public:
  /** \brief Constructor.
   *  \param f File name.
   */
  FileReader(std::string_view f);

  int peek() override final;

private:
  void do_chew() override final;

  GD::InputFile file_;  ///< File
  int c_;               ///< Current character (EOF means needs peeking or EOF)
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
  Token const& peek();

  /** \brief  Chew the current token. */
  void chew();

  /** \brief  Generate an error message with current location information added.  */
  template<typename... Ts>
  std::string error(Msg msg, Ts... args)
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

  std::unique_ptr<Reader> r_;  ///< Reader
  std::optional<Token> t_;     ///< Pending token.
};

/** \brief  An Instruction.
 *
 * Instructions contrain an opcode and up to two operands.
 *
 * References to other instructions are always PC relative - so that we can extract function
 * definitions easily.
 *
 * | Opcode  |  Operand 1  |  Operand 2  |  Description                                            |
 * | :------ | :---------- | :---------- | :------------------------------------------------------ |
 * | eof     |             |             | end of file.                                            |
 * | print   | Offset      | Stream      | Print the value at Offset to Stream.                    |
 * | quit    | unsigned    |             | Quit - using exit code `unsigned`.                      |
 * | string  | String      |             | A string value                                          |
 */
class Instruction
{
public:
  /** Opcodes. */
  enum class Opcode {
    eof,     ///< Reached the end of the current file.
    print,   ///< Print.  op1: Index of thing to print, op2: Stream
    quit,    ///< Quit. op1: Exit code
    string,  ///< String. op1: String to print.
  };

  /** Stream identifiers.  */
  enum class Stream {
    stdout,  ///< Standard output
    stderr   ///< Standard error.
  };

  /** Type representing an index into the list of instructions.  */
  using Index = std::vector<Instruction>::size_type;

  /** Type representing an offset of to an instruction. */
  using Offset = std::make_signed_t<Index>;

  /** Valid operand types.  */
  using Operand = std::variant<std::string, Stream, Offset, unsigned>;

  /** \brief        Constructor
   *  \param opcode Opcode
   */
  explicit Instruction(Opcode opcode);

  /** \brief        Constructor
   *  \param opcode Opcode
   *  \param op1    First operand
   */
  Instruction(Opcode opcode, Operand const& op1);

  /** \brief        Constructor
   *  \param opcode Opcode
   *  \param op1    First  operand
   *  \param op2    Second operand
   */
  Instruction(Opcode opcode, Operand const& op1, Operand const& op2);

  /** Get opcode */
  Opcode opcode() const;

  /** Get operand 1.  */
  Operand const& op1() const;

  /** Get operand 2.  */
  Operand const& op2() const;

  /** Do we have op1? */
  bool has_op1() const;

  /** Do we have op2? */
  bool has_op2() const;

  /** Does \a opcode have op1? */
  static bool has_op1(Opcode opcode);

  /** Does \a opcode have op2? */
  static bool has_op2(Opcode opcode);

private:
  Opcode opcode_;               ///< Opcode
  std::optional<Operand> op1_;  ///< Operand 1
  std::optional<Operand> op2_;  ///< Operand 2
};

/** Vector of instructions.  */
using Instructions = std::vector<Instruction>;

std::ostream& operator<<(std::ostream& os, Instruction::Stream s);
std::ostream& operator<<(std::ostream& os, Instruction::Opcode opcode);
std::ostream& operator<<(std::ostream& os, Instruction::Operand operand);
std::ostream& operator<<(std::ostream& os, Instruction const& instruction);
std::ostream& operator<<(std::ostream& os, Instructions const& instruction);

/** \brief Parsing class.  */
class Parser
{
public:
  /** \brief             Constructor
   *  \param lexer       Lexer.
   *  \param interactive Do we need to execute after every top-level input?
   */
  Parser(std::unique_ptr<Lexer>&& lexer, bool interactive);

  /** \brief  Do the next stage of a parse.
   */
  std::shared_ptr<Instructions> parse();

private:
  using Offset = Instruction::Offset;  ///< Offset into instructions list.
  using Index = Instruction::Index;    ///< Index into instructions list.

  /** Parse the main program production. */
  void parse_program();

  /** \brief  Parse input_item production
   *  \return \a true If we need to stop after this production (to execute the code).
   */
  bool parse_input_item();

  /** Parse semicolon_list production. */
  void parse_semicolon_list();

  /** Parse statement_list production.  */
  void parse_statement_list();

  /** \brief  Parse optional statement
   *  \return \a true if production was non-empty.
   */
  bool parse_opt_statement();

  /** Parse statement production.  */
  void parse_statement();

  /** Parse function production. */
  void parse_function();

  /** \brief  Parse optional parameter list
   *  \return \a true if production was non-empty
   */
  bool parse_opt_parameter_list();

  /** Parse a parameter list.  */
  void parse_parameter_list();

  /** \brief  Parse optional auto define list
   *  \return \a true if production was non-empty
   */
  bool parse_opt_auto_define_list();

  /** Parse define list.  */
  void parse_define_list();

  /** \brief  Parse optional argument list
   *  \return \a true if production was non-empty
   */
  bool parse_opt_argument_list();

  /** Parse argument list.  */
  void parse_argument_list();

  /** Parse relational expression.  */
  void parse_relational_expression();

  /** Parse return expression.  */
  void parse_return_expression();

  /** Parse expression.  */
  void parse_expression();

  /** Parse a named expression.  */
  void parse_named_expression();

  /** \brief       Insert error message into stream.
   *  \param  msg  Message ID
   *  \param  args Arguments for the message.
   *  \return      Offset of last instruction of error.
   */
  template<typename... Ts>
  Index insert_error(Msg msg, Ts... args)
  {
    error_ = true;
    auto s = 0;
    /* If the lexer holds an error token we report that rather than the error message we've been
     * asked to report.
     */
    if (lexer_->peek() == Token::Type::error) {
      s = insert_string(lexer_->peek().error());
    }
    else {
      s = insert_string(lexer_->error(msg, args...));
    }
    insert_print(s, Instruction::Stream::stderr);
    return insert_quit(1);
  }

  /** \brief    Insert end of file instruction
   *  \return   Index of inserted string
   */
  Index insert_eof();

  /** \brief    Insert string at end of instruction stream.
   *  \param  s String to insert
   *  \return   Index of inserted string
   */
  Index insert_string(std::string const& s);

  /** \brief         Insert print at end of instruction stream.
   *  \param  index  Index of thing to print.
   *  \param  stream Stream to print to.
   *  \return        Index of inserted print
   */
  Index insert_print(Index index, Instruction::Stream stream);

  /** \brief         Insert quit at end of instruction stream.
   *  \param  code   Exit code
   *  \return        Index of inserted print
   */
  Index insert_quit(unsigned code);

  std::unique_ptr<Lexer> lexer_;                ///< Lexer
  std::shared_ptr<Instructions> instructions_;  ///< Current set of instructions
  bool interactive_;                            ///< Are we interactive?
  bool error_;                                  ///< Has there been an error?
};

}  // namespace GD::Bc

template<>
struct fmt::formatter<GD::Bc::Token>
{
  constexpr auto parse(format_parse_context& ctx)
  {
    if (ctx.begin() != ctx.end() && *ctx.begin() != '}') {
      throw format_error("invalid format");
    }
    return ctx.begin();
  }

  template<typename FormatContext>
  auto format(GD::Bc::Token const& token, FormatContext& ctx)
  {
    std::ostringstream os;
    os << token;
    return format_to(ctx.out(), "{0}", os.str());
  }
};
#endif  //  _SRC_BC_BC_HH_INCLUDED
