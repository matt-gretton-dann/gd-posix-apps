/** \file   bc.hh
 *  \brief  Header file for bc
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef SRC_BC_BC_HH_INCLUDED
#define SRC_BC_BC_HH_INCLUDED

#include "gd/nl_types.h"

#include "gd/format.hh"

#include "util/file.hh"
#include "util/utils.hh"

#include "bc-messages.hh"

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
#include <variant>

namespace GD::Bc {
// Needed for number.hh
inline constexpr auto extensions_enabled() -> bool
{
#if ENABLE_EXTENSIONS
  return true;
#else
  return false;
#endif  // ENABLE_EXTENSIONS
}

}  // namespace GD::Bc

#include <type_traits>
#include <unordered_map>

#include "number.hh"

#ifndef ENABLE_EXTENSIONS
/** Are the non-POSIX extensions enabled? */
#  define ENABLE_EXTENSIONS 0  // NOLINT(cppcoreguidelines-macro-usage)
#endif

namespace GD::Bc {

/** \brief A letter
 *
 * Used for variable, function, and array names.
 */
class Letter
{
public:
  static constexpr unsigned count_ = 26;

  /** \brief  Construct letter from char. */
  explicit Letter(char l);

  /** \brief Convert to unsigned.  */
  explicit operator unsigned() const;

  /* Comparison operators.  */
  auto operator==(Letter l) const -> bool;
  auto operator==(char l) const -> bool;

private:
  static auto encode(char l) -> unsigned;
  unsigned letter_;  ///< The letter.
};

auto operator<<(std::ostream& /*os*/, Letter l) -> std::ostream&;

auto operator!=(Letter lhs, Letter rhs) -> bool;
auto operator!=(Letter lhs, char rhs) -> bool;
auto operator!=(char lhs, Letter rhs) -> bool;

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

  /** \brief      Construct a token of type Type::letter
   *  \param type Type::letter
   *  \param l    Letter
   */
  Token(Type type, Letter l);

  /** \brief Get token type.  */
  [[nodiscard]] auto type() const -> Type;

  /** \brief  Get string stored in token. */
  [[nodiscard]] auto string() const -> std::string const&;

  /** \brief  Get number stored in token. */
  [[nodiscard]] auto number() const -> std::string const&;

  /** \brief Get letter stored in token. */
  [[nodiscard]] auto letter() const -> Letter;

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

  std::variant<Type, std::string, NumInt, ErrorInt, Letter> value_;
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
  [[nodiscard]] auto location() const -> Location const&;

  /** \brief  Report an error giving source location. */
  template<typename... Ts>
  auto error(Msg msg, Ts... args) -> std::string
  {
    std::ostringstream os;
    os << Messages::get().get(Set::bc, Msg::error_label) << ":" << location_ << ": "
       << Messages::get().format(Set::bc, msg, args...) << '\n';
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
  bool seen_quit_{false};       ///< Have we seen a quit token?
  bool first_character_{true};  ///< Have we lexed anything yet?
};

/** Wrapper around Letter representing a Variable */
using Variable = TypeWrapper<Letter, struct VariableTag>;

/** Wrapper around Array representing a Variable */
using Array = TypeWrapper<Letter, struct ArrayTag>;
using ArrayIndex = Number::NumType;

/** An array element - the array and index.  */
using ArrayElement = std::pair<Array, ArrayIndex>;

/** Array values. */
using ArrayValues = std::shared_ptr<std::map<ArrayIndex, Number>>;

template<typename T>
auto operator==(TypeWrapper<Letter, T> lhs, TypeWrapper<Letter, T> rhs) -> bool
{
  return lhs.get() == rhs.get();
}

template<typename T>
auto operator<<(std::ostream& os, TypeWrapper<Letter, T> l) -> std::ostream&
{
  return (os << l.get());
}

/** \brief  A mask of variables and arrays.  Used for indicating the local variables of a function.
 */
class VariableMask
{
public:
  VariableMask();

  /** \brief COnstructor (mainly for testing purposes). */
  VariableMask(std::string_view vars, std::string_view arrays);

  /** \brief  Add variable \a letter to the mask.  */
  void add(Variable v);

  /** \brief  Add array \a letter to the mask.  */
  void add(Array a);

  /** \brief  Does the mask contain variable \a letter?  */
  [[nodiscard]] auto contains(Variable v) const -> bool;

  /** \brief  Does the mask contain array \a letter?  */
  [[nodiscard]] auto contains(Array a) const -> bool;

  /** Equality operator. */
  auto operator==(VariableMask rhs) const -> bool;

  /** \brief  Call \a f for each variable in the mask.  */
  template<typename Fn>
  void for_each_variable(Fn f) const
  {
    for_each_internal(f, variable_mask_);
  }

  /** \brief  Call \a f for each array in the mask.  */
  template<typename Fn>
  void for_each_array(Fn f) const
  {
    for_each_internal(f, array_mask_);
  }

private:
  /* Internal implementation of for_each_*. */
  template<typename Fn>
  static void for_each_internal(Fn fn, ::uint32_t mask)
  {
    /* Can't assume letters are contiguous code-points.  */
    std::string_view const letters = "abcdefghijklmnopqrstuvwxyz";
    std::string_view::const_iterator letter = letters.begin();
    while (letter != letters.end() && mask != 0) {
      if (mask & 1) {
        fn(Letter(*letter));
      }
      mask >>= 1;
      ++letter;
    }
  }

  ::uint32_t variable_mask_ = 0;  ///< Mask of variables
  ::uint32_t array_mask_ = 0;     ///< Mask of arrays.
};

auto operator<<(std::ostream& os, VariableMask mask) -> std::ostream&;

auto operator!=(VariableMask lhs, VariableMask rhs) -> bool;

/** Tag class for an ibase.  */
enum class Ibase : int;

/** Tag class for an obase.  */
enum class Obase : int;

/** Tag class for an scale.  */
enum class Scale : int;

/** \brief  An Instruction.
 *
 * Instructions contrain an opcode, up to two operands.
 *
 * References to other instructions are always PC relative - so that we can extract function
 * definitions easily.
 *
 * | Opcode              |  Operand 1  |  Operand 2  |  Description                               |
 * | :------------------ | :---------- | :---------- | :----------------------------------------- |
 * | eof                 |             |             | end of file.                               |
 * | print               | Offset      | Stream      | Print the value at op1 to stream op2.      |
 * | quit                | unsigned    |             | Quit - using exit code Op1.                |
 * | string              | String      |             | A string value                             |
 * | number              | String      |             | A number value (as yet uninterpreted).     |
 * | variable            | Variable    |             | A variable                                 |
 * | array               | Array       |             | Array op1                                  |
 * | array_element       | Array       | Offset      | Element op2 of Array op1                   |
 * | scale               |             |             | Scale variable                             |
 * | ibase               |             |             | ibase variable                             |
 * | obase               |             |             | obase variable                             |
 * | add                 | Offset      | Offset      | Op1 + Op2                                  |
 * | subtact             | Offset      | Offset      | Op1 - Op2                                  |
 * | negate              | Offset      |             | -Op1                                       |
 * | multiply            | Offset      | Offset      | Op1 * Op2                                  |
 * | divide              | Offset      | Offset      | Op1 / Op2                                  |
 * | modulo              | Offset      | Offset      | Op1 % Op2                                  |
 * | power               | Offset      | Offset      | Op1 ^ Op2                                  |
 * | load                | Offset      |             | Load value stored in named-expr op1.       |
 * | store               | Offset      | Offset      | Store value Op2 into named expression Op1. |
 * | scale_expr          | Offset      |             | Calculate scale(op1)                       |
 * | sqrt                | Offset      |             | Calculate sqrt(op1)                        |
 * | abs                 | Offset      |             | Calculate abs(op1)                         |
 * | length              | Offset      |             | Calculate length(op1)                      |
 * | equals              | Offset      | Offset      | 1 if op1 == op2, 0 otherwise               |
 * | less_than_equals    | Offset      | Offset      | 1 if op1 <= op2, 0 otherwise               |
 * | not_equals          | Offset      | Offset      | 1 if op1 != op2, 0 otherwise               |
 * | less_than           | Offset      | Offset      | 1 if op1 < op2, 0 otherwise                |
 * | branch              | Offset      |             | Unconditional branch to op1                |
 * | branch_zero         | Offset      | Offset      | Branch to op2 if op1 is 0.                 |
 * | return_             | Offset      |             | return from function with value Op1        |
 * | call                | Letter      | Location    | Call fn op1, op2 is source loc of call     |
 * | push_param_mark     |             |             | Push fn separator marker onto param stack  |
 * | pop_param_mark      |             |             | Pop fn separator marker from param stack   |
 * | push_param          | Offset      |             | Push parameter onto param stack            |
 * | pop_param           |             |             | Pop scalar parameter from param stack      |
 * | pop_param_array     |             |             | Pop array parameter from param stack       |
 * | function_begin      | SavedVars   | Location    | Start of function definition               |
 * | function_end        | Letter      | Offset      | End of function definition                 |
 *
 * function_begin contains mask of local variables in op1, and location of definition in op2.
 * function_end contains name of function in op1 and offset to function_begin in op2.
 *
 */
class Instruction
{
public:
  /** Opcodes. */
  enum class Opcode {
    eof,               ///< Reached the end of the current file.
    print,             ///< Print.  op1: Index of thing to print, op2: Stream
    quit,              ///< Quit. op1: Exit code
    string,            ///< String. op1: String to print.
    number,            ///< Number. op1: Stringified version of number
    variable,          ///< Variable. op1: Var
    array_element,     ///< Element of array: op1[op2]
    array,             ///< Array
    scale,             ///< Scale variable.
    ibase,             ///< ibase variable.
    obase,             ///< Obase variable.
    add,               ///< Add. op1 + op2
    subtract,          ///< Subtract. op1 - op2
    negate,            ///< Negate. -op1
    multiply,          ///< Multiply. op1 * op2
    divide,            ///< Divide. op1 / op2
    modulo,            ///< Modulo. op1 % op2
    power,             ///< Power. op1 ^ op2
    load,              ///< Load. Load from op1
    store,             ///< Store.  Store op2 into op1.
    scale_expr,        ///< scale(expr).
    sqrt,              ///< sqrt(expr).
    abs,               ///< abs(expr).
    length,            ///< length(expr).
    equals,            ///< op1 == op2.
    less_than_equals,  ///< op1 <= op2.
    not_equals,        ///< op1 != op2.
    less_than,         ///< op1 < op2.
    branch,            ///< Branch to op1
    branch_zero,       ///< Branch to op1 if op2 is zero.
    return_,           ///< return(op1)
    call,              ///< call function op1, source location of call in Op2.
    push_param_mark,   ///< Push a "function separator" marker onto the parameter stack
    pop_param_mark,    ///< Pop a "function separator" marker onto the parameter stack
    push_param,        ///< Push op1 onto the parameter stack
    pop_param,         ///< Pop op1 off the parameter stack.
    pop_param_array,   ///< Pop op1 off the parameter stack.
    function_begin,    ///< Start of function definition.  Op2: Definition location, Op1: local vars
    function_end,      ///< End of function definition, Op1: function name.
  };

  /** Stream identifiers.  */
  enum class Stream {
    output,  ///< Use normal output stream
    error    ///< Use error stream.
  };

  /** Type representing an index into the list of instructions.  */
  using Index = std::vector<Instruction>::size_type;

  /** Type representing an offset of to an instruction. */
  using Offset = std::make_signed_t<Index>;

  /** Valid operand types.  */
  using Operand = std::variant<std::string, Stream, Offset, Location, VariableMask, unsigned,
                               Letter, Variable, Array>;

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
  [[nodiscard]] auto opcode() const -> Opcode;

  /** Do we have op1? */
  [[nodiscard]] auto has_op1() const -> bool;

  /** Get operand 1.  */
  [[nodiscard]] auto op1() const -> Operand const&;

  /** Update operand 1.  */
  void op1(Operand const& operand);

  /** Do we have op2? */
  [[nodiscard]] auto has_op2() const -> bool;

  /** Get operand 2.  */
  [[nodiscard]] auto op2() const -> Operand const&;

  /** Update operand 2.  */
  void op2(Operand const& operand);

  /** Do we have result? */
  [[nodiscard]] auto has_result() const -> bool;

  /** Does \a opcode have op1? */
  static auto has_op1(Opcode opcode) -> bool;

  /** Does \a opcode have op2? */
  static auto has_op2(Opcode opcode) -> bool;

private:
  /** \brief  How many operands does \a opcode take? */
  static auto op_count(Opcode opcode) -> unsigned;

  /** \brief  Validate the operands.  */
  void validate_operands() const;

  Opcode opcode_;               ///< Opcode
  std::optional<Operand> op1_;  ///< Operand 1
  std::optional<Operand> op2_;  ///< Operand 2
};

/** Vector of instructions.  */
using Instructions = std::vector<Instruction>;

auto operator<<(std::ostream& os, Instruction::Stream s) -> std::ostream&;
auto operator<<(std::ostream& os, Instruction::Opcode opcode) -> std::ostream&;
auto operator<<(std::ostream& os, Instruction::Operand const& operand) -> std::ostream&;
auto operator<<(std::ostream& os, Instruction const& instruction) -> std::ostream&;
auto operator<<(std::ostream& os, Instructions const& instruction) -> std::ostream&;

/** \brief Parsing class.  */
class Parser
{
public:
  using Offset = Instruction::Offset;  ///< Offset into instructions list.
  using Index = Instruction::Index;    ///< Index into instructions list.

  /** Flags for parse_opt_primary_expression.  */
  enum class POPEFlags {
    parse_primary = 0x0,      ///< Primary and named expressions.
    parse_array_slices = 0x1  ///< Array slices, primary and named expressions.
  };

  /* Root expression type returned by parse_opt_epxression().  */
  enum class ExprType {
    missing,      ///< No expression
    assignment,   ///< Assignment expression
    named,        ///< A named expression
    primary,      ///< A primary expression.
    array_slice,  ///< Array slice.
    other         ///< Some other type
  };

  /** \brief  Index/Type of an expression pair. */
  class ExprIndex
  {
  public:
    /** \brief  Construct an Expression index
     *  \param index Index
     *  \param type  Type of expression
     */
    constexpr ExprIndex(Index index, ExprType type) : index_(index), type_(type) {}

    /** \brief       Construct an Expression index of type other.
     *  \param index Index
     */
    constexpr explicit ExprIndex(Index index) : index_(index), type_(ExprType::other) {}

    /** Get the index.  */
    [[nodiscard]] constexpr auto index() const -> Index { return index_; }

    /** Get the type.  */
    [[nodiscard]] constexpr auto type() const -> ExprType { return type_; }

    /** \brief Get a canonical "missing" expression index.  */
    static auto missing() -> ExprIndex const&
    {
      static ExprIndex const missing(0, ExprType::missing);
      return missing;
    }

  private:
    Index index_;
    ExprType type_;
  };

  /** \brief             Constructor
   *  \param lexer       Lexer.
   *  \param interactive Do we need to execute after every top-level input?
   */
  Parser(std::unique_ptr<Lexer>&& lexer, bool interactive);

  /** \brief  Do the next stage of a parse.
   */
  auto parse() -> std::shared_ptr<Instructions>;

  /** \brief  Have we seen a `quit` statement.  */
  [[nodiscard]] auto seen_quit() const noexcept -> bool;

private:
  /** Parse the main program production. */
  void parse_program();

  /** \brief  Parse input_item production
   *  \return \a true If we need to stop after this production (to execute the code).
   */
  auto parse_input_item() -> bool;

  /** Parse semicolon_list production. */
  void parse_semicolon_list();

  /** Parse statement_list production.  */
  void parse_statement_list();

  /** \brief  Parse optional statement
   *  \return \a true if production was non-empty.
   */
  auto parse_opt_statement() -> bool;

  /** Parse statement production.  */
  void parse_statement();

  /** Parse a break statement production.  */
  void parse_break_statement();

  /** Parse a return statement production.  */
  void parse_return_statement();

  /** Parse a for statement production.  */
  void parse_for_statement();

  /** Parse an if statement production.  */
  void parse_if_statement();

  /** Parse a while statement production.  */
  void parse_while_statement();

  /** Parse function production. */
  void parse_function();

  /** \brief                Parse optional parameter list
   *  \param function_begin index of function begin instruction
   *  \return               Expression of last entry in list
   */
  auto parse_opt_parameter_list(ExprIndex function_begin) -> ExprIndex;

  /** \brief                Parse optional auto define list
   *  \param function_begin index of function begin instruction
   *  \return               Expression of last entry in list
   */
  auto parse_opt_auto_define_list(ExprIndex function_begin) -> ExprIndex;

  /** \brief                  Parse an optional define list.
   *  \param function_begin   index of function begin instruction
   *  \param allow_duplicates Do we allow multiple entries of the same variable/array?
   *  \return                 Expression of last entry in list
   */
  auto parse_opt_define_list(ExprIndex function_begin, bool allow_duplicates) -> ExprIndex;

  /** \brief                  Parse a define list.
   *  \param function_begin   index of function begin instruction
   *  \param allow_duplicates Do we allow multiple entries of the same variable/array?
   *  \return                 Expression of last entry in list
   */
  auto parse_define_list(ExprIndex function_begin, bool allow_duplicates) -> ExprIndex;

  /** \brief                  Parse an optional define element.
   *  \param function_begin   index of function begin instruction
   *  \param allow_duplicates Do we allow multiple entries of the same variable/array?
   *  \return                 Expression of last entry in list
   */
  auto parse_opt_define_element(ExprIndex function_begin, bool allow_duplicates) -> ExprIndex;

  /** \brief                  Parse a define element.
   *  \param function_begin   index of function begin instruction
   *  \param allow_duplicates Do we allow multiple entries of the same variable/array?
   *  \return                 Expression of last entry in list
   */
  auto parse_define_element(ExprIndex function_begin, bool allow_duplicates) -> ExprIndex;

  /** \brief  Parse optional argument list
   *  \return \a true if production was non-empty
   */
  auto parse_opt_argument_list() -> ExprIndex;

  /** Parse argument list.  */
  auto parse_argument_list() -> ExprIndex;

  /** Parse relational expression.  */
  auto parse_relational_expression() -> ExprIndex;

  /** Parse return expression.  */
  auto parse_return_expression() -> ExprIndex;

  /** \brief        Parse an optional expression
   *  \param  flags Flags of type of expressions to accept, default primary.
   *  \return       Index of expression (if one given) and type of expression parsed).
   */
  auto parse_opt_expression(POPEFlags flags = POPEFlags::parse_primary) -> ExprIndex;

  /** \brief        Parse expression.
   *  \param  flags Flags of type of expressions to accept, default primary.
   *  \return       Index of expression's value.
   */
  auto parse_expression(POPEFlags flags = POPEFlags::parse_primary) -> ExprIndex;

  /** \brief        Parse an optional assignment expression
   *  \param  flags Flags of type of expressions to accept, default primary.
   *  \return       Index of expression, and type of expression.
   */
  auto parse_opt_assign_expression(POPEFlags flags = POPEFlags::parse_primary) -> ExprIndex;

  /** \brief        Parse an optional assignment expression
   *  \param  flags Flags of type of expressions to accept, default primary.
   *  \return       Index of expression, and type of expression.
   */
  auto parse_assign_expression(POPEFlags flags = POPEFlags::parse_primary) -> ExprIndex;

  /** \brief  Parse a unary minus expression.
   *  \return  Index of expression, and type of expression.
   */
  auto parse_unary_minus_expression() -> ExprIndex;

  /** \brief  Parse an inrement/decrement expression.
   *  \return  Index of expression, and type of expression.
   */
  auto parse_incr_decr_expression() -> ExprIndex;

  /** \brief      Parse an add expression
   *  \return     Index of result
   */
  auto parse_add_expression() -> ExprIndex;

  /** \brief      Parse a mul expression
   *  \return     Index of result
   */
  auto parse_mul_expression() -> ExprIndex;

  /** \brief      Parse a power expression
   *  \return     Index of result, may be \a lhs if this isn't a power expression.
   */
  auto parse_power_expression() -> ExprIndex;

  /** \brief      Parse an add expression
   *  \param  lhs Left hand side of expression
   *  \return     Index of result, may be \a lhs if this isn't an add expression.
   */
  auto parse_add_expression(ExprIndex lhs) -> ExprIndex;

  /** \brief      Parse a mul expression
   *  \param  lhs Left hand side of expression
   *  \return     Index of result, may be \a lhs if this isn't a mul expression.
   */
  auto parse_mul_expression(ExprIndex lhs) -> ExprIndex;

  /** \brief      Parse a power expression
   *  \param  lhs Left hand side of expression
   *  \return     Index of result, may be \a lhs if this isn't a power expression.
   */
  auto parse_power_expression(ExprIndex lhs) -> ExprIndex;

  /** \brief        Parse primary and named expressions
   *  \param  flags Bit mask of flags - default parse primary and named expressions
   *  \return       Expression index.
   *
   * This function will always parse named expression.  The flags will add primary expressions and
   * array slices to the parsing.
   */
  auto parse_opt_primary_expression(POPEFlags flags = POPEFlags::parse_primary) -> ExprIndex;

  /** \brief        Parse primary, named and optionally array_slice expressions
   *  \param  flags Flags - match array slices as well as primary & named expressions?
   *  \return       Expression index.
   *
   * This is the most complicated of the parsing functions.  Everywhere the grammar accepts a
   * named_expression it also accepts a primary_expression, and the recognisers are intertwined.
   * Therefore we recognise both in one function and return the type in the result.
   *
   * Array_slices are only used in function calls as the top-level expression for a parameter, but
   * it is easiest to include recognising them in this function.
   */
  auto parse_primary_expression(POPEFlags flags = POPEFlags::parse_primary) -> ExprIndex;

  /** \brief      Ensure that the value of the expression pointed to by \a idx has been loaded.
   *  \param  idx Index to check the loadedness of
   *  \return     Index of loaded value (may be same as \a idx).
   *
   * Named expressions point to names and not values.  This function ensures that we have loaded
   * the value of the name.
   */
  auto ensure_expr_loaded(ExprIndex idx) -> ExprIndex;

  /** \brief       Insert error message into stream.
   *  \param  msg  Message ID
   *  \param  args Arguments for the message.
   *  \return      Offset of last instruction of error.
   */
  template<typename... Ts>
  auto insert_error(Msg msg, Ts... args) -> ExprIndex
  {
    if (seen_quit_) {
      return ExprIndex::missing();
    }

    error_ = true;
    /* If the lexer holds an error token we report that rather than the error message we've been
     * asked to report.
     */
    bool const lexer_error = lexer_->peek() == Token::Type::error;
    auto s = insert_string(lexer_error ? lexer_->peek().error() : lexer_->error(msg, args...));
    if (lexer_error) {
      lexer_->chew();
    }
    insert_print(s, Instruction::Stream::error);
    return insert_quit(1);
  }

  /** \brief    Insert end of file instruction
   *  \return   Index of inserted string
   */
  auto insert_eof() -> ExprIndex;

  /** \brief    Insert string at end of instruction stream.
   *  \param  s String to insert
   *  \return   Index of inserted string
   */
  auto insert_string(std::string const& s) -> ExprIndex;

  /** \brief         Insert print at end of instruction stream.
   *  \param  idx    Index of thing to print.
   *  \param  stream Stream to print to.
   *  \return        Index of inserted print
   */
  auto insert_print(ExprIndex idx, Instruction::Stream stream) -> ExprIndex;

  /** \brief         Insert quit at end of instruction stream.
   *  \param  code   Exit code
   *  \return        Index of inserted print
   */
  auto insert_quit(unsigned code) -> ExprIndex;

  /** \brief        Insert a load.
   *  \param  idx   Named expression to load
   *  \return       Index to value loaded.
   */
  auto insert_load(ExprIndex idx) -> ExprIndex;

  /** \brief        Insert a store.
   *  \param  named Named expression to store to
   *  \param  value Value to store.
   *  \return       Index to value stored, but with expression type of assignment.
   */
  auto insert_store(ExprIndex var, ExprIndex value) -> ExprIndex;  // Type == assignment

  /** \brief         Insert an arithmetic operation.
   *  \param  opcode Arithmetic opcode to use
   *  \param  lhs    Left-hand side expression
   *  \param  rhs    Right-hand side expression
   *  \return        Index of result value.
   */
  auto insert_arith(Instruction::Opcode opcode, ExprIndex lhs, ExprIndex rhs) -> ExprIndex;

  /** \brief         Insert a negation op
   *  \param  expr   Expression to negate
   *  \return        Index of result value.
   */
  auto insert_negate(ExprIndex expr) -> ExprIndex;

  /** \brief         Insert a number
   *  \param  number Number to store
   *  \return        Index of number.
   */
  auto insert_number(std::string const& number) -> ExprIndex;

  /** \brief         Insert an array element
   *  \param v       Array
   *  \param element Index of expr to calculate element
   *  \return         Index of inserted instruction
   */
  auto insert_array_element(Array v, ExprIndex element) -> ExprIndex;  // Type = named

  /** \brief         Insert an array
   *  \param v       Array
   *  \return         Index of inserted instruction
   */
  auto insert_array_slice(Array v) -> ExprIndex;

  /** \brief          Insert a variable
   *  \param  v       Variable
   *  \return         Index of inserted instruction
   */
  auto insert_variable(Variable v) -> ExprIndex;  // Type = named

  /** \brief  Insert scale variable - not scale()
   *  \return Index of inserted instruction
   */
  auto insert_scale() -> ExprIndex;  // Type = named

  /** \brief  Insert obase variable
   *  \return Index of inserted instruction
   */
  auto insert_obase() -> ExprIndex;  // Type = named

  /** \brief  Insert ibase variable
   *  \return Index of inserted instruction
   */
  auto insert_ibase() -> ExprIndex;  // Type = named

  /** \brief       Insert scale() call
   *  \param  expr Expression to pass
   *  \return      Index of inserted instruction
   */
  auto insert_scale_expr(ExprIndex expr) -> ExprIndex;

  /** \brief       Insert sqrt() call
   *  \param  expr Expression to pass
   *  \return      Index of inserted instruction
   */
  auto insert_sqrt(ExprIndex expr) -> ExprIndex;

  /** \brief       Insert abs() call
   *  \param  expr Expression to pass
   *  \return      Index of inserted instruction
   */
  auto insert_abs(ExprIndex expr) -> ExprIndex;

  /** \brief       Insert length() call
   *  \param  expr Expression to pass
   *  \return      Index of inserted instruction
   */
  auto insert_length(ExprIndex expr) -> ExprIndex;

  /** \brief      Insert branch if zero
   *  \param dest Destination
   *  \param cmp  Comparison index
   */
  auto insert_branch_zero(ExprIndex dest, ExprIndex cmp) -> ExprIndex;

  /** \brief      Insert branch
   *  \param dest Destination
   */
  auto insert_branch(ExprIndex dest) -> ExprIndex;

  /** \brief      Insert return
   *  \param expr Result of function
   */
  auto insert_return(ExprIndex expr) -> ExprIndex;

  /** \brief       Insert call() call
   *  \param  v    Function to call
   *  \param  loc  Source location of call (for error reporting purposes).
   *  \return      Index of inserted instruction
   */
  auto insert_call(Letter v, Location const& loc) -> ExprIndex;

  /** \brief   Insert push_param_mark instruction.
   */
  auto insert_push_param_mark() -> ExprIndex;

  /** \brief   Insert pop_param_mark instruction.
   */
  auto insert_pop_param_mark() -> ExprIndex;

  /** \brief   Insert push_param instruction.
   *  \param expr Expression to push
   */
  auto insert_push_param(ExprIndex expr) -> ExprIndex;

  /** \brief   Insert pop_param instruction.
   */
  auto insert_pop_param() -> ExprIndex;

  /** \brief   Insert pop_param_array instruction.
   */
  auto insert_pop_param_array() -> ExprIndex;

  /** \brief      Insert function begin
   *  \param mask Mask of local variables to save
   *  \param loc  Location of function definition
   */
  auto insert_function_begin(VariableMask mask, Location const& loc) -> ExprIndex;

  /** \brief        Insert function end
   *  \param letter Function we're finishing
   *  \param begin  Index of beginning of function.
   */
  auto insert_function_end(Letter letter, ExprIndex begin) -> ExprIndex;

  /** \brief Push a new loop state.  */
  void push_loop();

  /** \brief     Add an loop break/exit instruction to the list for the current loop.
   *  \param idx Index of loop exit instruction.
   */
  void add_loop_exit(Index idx);

  /** \brief      Update the break/exit instructions for the current loop to point to idx.
   *  \param dest Index of first instruction after the end of the loop.
   */
  void update_loop_exits(Index dest);

  /** \brief  Pop the loop state.  */
  void pop_loop();

  /** \brief  Are we currently in a loop? */
  [[nodiscard]] auto in_loop() const -> bool;

  std::unique_ptr<Lexer> lexer_;                ///< Lexer
  std::shared_ptr<Instructions> instructions_;  ///< Current set of instructions
  std::stack<std::list<Index>> loop_breaks_;    ///< Stack of loop breaks.
  bool interactive_;                            ///< Are we interactive?
  bool error_{false};                           ///< Has there been an error?
  bool in_function_{false};                     ///< Are we in a function?
  bool seen_quit_{false};                       ///< Have we seen a quit token?
};

auto operator==(Parser::ExprIndex const& lhs, Parser::ExprIndex const& rhs) -> bool;
auto operator==(Parser::ExprIndex const& lhs, Parser::Index rhs) -> bool;
auto operator==(Parser::ExprIndex const& lhs, Parser::ExprType rhs) -> bool;
auto operator!=(Parser::ExprIndex const& lhs, Parser::ExprIndex const& rhs) -> bool;
auto operator!=(Parser::ExprIndex const& lhs, Parser::Index rhs) -> bool;
auto operator!=(Parser::ExprIndex const& lhs, Parser::ExprType rhs) -> bool;
auto operator-(Parser::ExprIndex const& lhs, Parser::ExprIndex const& rhs) -> Parser::Offset;

namespace Details {
/* Forward declaration of class that holds VMState.  */
struct VMState;
}  // namespace Details

/** \brief  Virtual Machine
 *
 * This is the object that executes instructions.
 */
class VM
{
public:
  /** \brief               Constructor
   *  \param out           Stream to use for standard output.
   *  \param err           Stream to use for standard error.
   *  \param save_specials Should we save ibase, obase, scale on entry to a function?
   */
  VM(std::ostream& out, std::ostream& err, bool save_specials);

  /** \brief              Execute instructions.
   *  \param instructions Instructions to execute
   *  \return             \c true if we should continue, or \c false if we have reached EOF.
   */
  auto execute(Instructions& instructions) -> bool;

private:
  Details::VMState* state_;
};

}  // namespace GD::Bc

template<>
struct fmt::formatter<GD::Bc::Token>
{
  static constexpr auto parse(format_parse_context& ctx)
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
    // Work around Win32 STL bug:
    return fmt::vformat_to(ctx.out(), "{0}", fmt::make_format_args(os.str()));
  }
};

template<>
struct fmt::formatter<GD::Bc::Letter>
{
  static constexpr auto parse(format_parse_context& ctx)
  {
    if (ctx.begin() != ctx.end() && *ctx.begin() != '}') {
      throw format_error("invalid format");
    }
    return ctx.begin();
  }

  template<typename FormatContext>
  auto format(GD::Bc::Letter letter, FormatContext& ctx)
  {
    static std::string_view const letters = "abcdefghijklmnopqrstuvwxyz";
    // Work around Win32 STL bug:
    return fmt::vformat_to(ctx.out(), "{0}",
                           fmt::make_format_args(letters[static_cast<unsigned>(letter)]));
  }
};
#endif  // SRC_BC_BC_HH_INCLUDED
