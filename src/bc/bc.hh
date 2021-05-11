/** \file   bc.hh
 *  \brief  Header file for bc
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_BC_BC_HH_INCLUDED
#define _SRC_BC_BC_HH_INCLUDED

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

#include "number.hh"
#include <type_traits>
#include <unordered_map>

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
  bool operator==(Letter l) const;
  bool operator==(char l) const;

private:
  static unsigned encode(char l);
  unsigned letter_;  ///< The letter.
};

std::ostream& operator<<(std::ostream&, Letter l);

bool operator!=(Letter lhs, Letter rhs);
bool operator!=(Letter lhs, char rhs);
bool operator!=(char lhs, Letter rhs);

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
  Token(Type type, Letter l);

  /** \brief Get token type.  */
  Type type() const;

  /** \brief  Get string stored in token. */
  std::string const& string() const;

  /** \brief  Get number stored in token. */
  std::string const& number() const;

  /** \brief Get letter stored in token. */
  Letter letter() const;

  /** \brief  Get the error message.  */
  std::string const& error() const;

  /** \brief  Output debug form of token. */
  void debug(std::ostream& os) const;

  /** \brief  Is this an assignment op? */
  bool is_assign_op() const;

  /** \brief  Is this an increment/decrement op?  */
  bool is_incr_decr_op() const;

  /** \brief  Is this a multiplication style op?  */
  bool is_mul_op() const;

  /** \brief  Is this an addition style op?  */
  bool is_add_op() const;

  /** \brief  Is this a relation op? */
  bool is_rel_op() const;

private:
  /** Internal type to hold a number and diferentiate it from string and error.  */
  using NumInt = TypeWrapper<std::string, struct NumIntType>;

  /** Internal type to hold an error string and differentiate it from string and number.  */
  using ErrorInt = TypeWrapper<std::string, struct ErrorIntType>;

  std::variant<Type, std::string, NumInt, ErrorInt, Letter> value_;
};

/** \brief Output operator for token type.  */
std::ostream& operator<<(std::ostream& os, Token::Type t);
std::ostream& operator<<(std::ostream& os, Token const& token);

/* Comparison operators.  */
bool operator==(Token const& token, Token::Type type);
bool operator==(Token::Type type, Token const& token);
bool operator!=(Token const& token, Token::Type type);
bool operator!=(Token::Type type, Token const& token);

/** \brief  A source location.  */
class Location
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
  std::string const& file_name() const;

  /** Get column.  */
  Column column() const;

  /** Get line.  */
  Line line() const;

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

std::ostream& operator<<(std::ostream& os, Location const& location);

bool operator==(Location const& lhs, Location const& rhs);
bool operator!=(Location const& lhs, Location const& rhs);

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

  /** \brief  Get the current source location.  */
  Location const& location() const;

  /** \brief  Report an error giving source location. */
  template<typename... Ts>
  std::string error(Msg msg, Ts... args)
  {
    std::ostringstream os;
    os << GD::Bc::Messages::get().get(GD::Bc::Set::bc, Msg::error_label) << ":" << location_ << ": "
       << GD::Bc::Messages::get().format(GD::Bc::Set::bc, msg, args...) << '\n';
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

  /** \brief  Get the current location. */
  Location const& location() const;

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

/** Wrapper around Letter representing a Variable */
using Variable = TypeWrapper<Letter, struct VariableTag>;

/** Wrapper around Array representing a Variable */
using Array = TypeWrapper<Letter, struct ArrayTag>;
using ArrayIndex = Number::NumType;

/** An array element - the array and index.  */
using ArrayElement = std::pair<Array, ArrayIndex>;

/** Array values. */
using ArrayValues = std::shared_ptr<std::unordered_map<ArrayIndex, Number>>;

template<typename T>
bool operator==(TypeWrapper<Letter, T> lhs, TypeWrapper<Letter, T> rhs)
{
  return lhs.get() == rhs.get();
}

template<typename T>
std::ostream& operator<<(std::ostream& os, TypeWrapper<Letter, T> l)
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
  bool contains(Variable v) const;

  /** \brief  Does the mask contain array \a letter?  */
  bool contains(Array a) const;

  /** Equality operator. */
  bool operator==(VariableMask rhs) const;

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
    char const* letters = "abcdefghijklmnopqrstuvwxyz";
    char const* letter = letters;
    while (*letter != '\0' && mask != 0) {
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

std::ostream& operator<<(std::ostream& os, VariableMask mask);

bool operator!=(VariableMask lhs, VariableMask rhs);

/** Tag class for an ibase.  */
enum class Ibase : int;

/** Tag class for an obase.  */
enum class Obase : int;

/** Tag class for an scale.  */
enum class Scale : int;

/** \brief  An Instruction.
 *
 * Instructions contrain an opcode, up to two operands, and an optional result (once the instruction
 * has been executed.
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
    stdout,  ///< Standard output
    stderr   ///< Standard error.
  };

  /** Type representing an index into the list of instructions.  */
  using Index = std::vector<Instruction>::size_type;

  /** Type representing an offset of to an instruction. */
  using Offset = std::make_signed_t<Index>;

  /** Valid operand types.  */
  using Operand = std::variant<std::string, Stream, Offset, Location, VariableMask, unsigned,
                               Letter, Variable, Array>;

  /** Valid result types for an instruction:
   *
   * Number: A number
   * string_view: a string, legal as it should only point to the string in op1().
   * Variable: Variable name
   * Array: Array name
   * ArrayElement: Array element.
   */
  using Result = std::variant<Number, ArrayValues, std::string_view, Variable, Array, ArrayElement,
                              Ibase, Obase, Scale>;

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

  /** Do we have op1? */
  bool has_op1() const;

  /** Get operand 1.  */
  Operand const& op1() const;

  /** Update operand 1.  */
  void op1(Operand const& operand);

  /** Do we have op2? */
  bool has_op2() const;

  /** Get operand 2.  */
  Operand const& op2() const;

  /** Update operand 2.  */
  void op2(Operand const& operand);

  /** Do we have result? */
  bool has_result() const;

  /** Get the result.  */
  Result const& result() const;

  /** Clear the result value.  */
  void clear_result();

  /** Set the result.  */
  void result(Result const& result);

  /** Does \a opcode have op1? */
  static bool has_op1(Opcode opcode);

  /** Does \a opcode have op2? */
  static bool has_op2(Opcode opcode);

private:
  /** \brief  How many operands does \a opcode take? */
  static unsigned op_count(Opcode opcode);

  /** \brief  Validate the operands.  */
  void validate_operands() const;

  /** \brief  Validate the result.  */
  void validate_result() const;

  Opcode opcode_;                 ///< Opcode
  std::optional<Operand> op1_;    ///< Operand 1
  std::optional<Operand> op2_;    ///< Operand 2
  std::optional<Result> result_;  ///< The result.
};

/** Vector of instructions.  */
using Instructions = std::vector<Instruction>;

std::ostream& operator<<(std::ostream& os, Instruction::Stream s);
std::ostream& operator<<(std::ostream& os, Instruction::Opcode opcode);
std::ostream& operator<<(std::ostream& os, Instruction::Operand const& operand);
std::ostream& operator<<(std::ostream& os, Instruction::Result const& result);
std::ostream& operator<<(std::ostream& os, Instruction const& instruction);
std::ostream& operator<<(std::ostream& os, Instructions const& instruction);

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
    constexpr Index index() const { return index_; }

    /** Get the type.  */
    constexpr ExprType type() const { return type_; }

    /** \brief Get a canonical "missing" expression index.  */
    static ExprIndex const& missing()
    {
      static ExprIndex missing(0, ExprType::missing);
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
  std::shared_ptr<Instructions> parse();

private:
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
  ExprIndex parse_opt_parameter_list(ExprIndex function_begin);

  /** \brief                Parse optional auto define list
   *  \param function_begin index of function begin instruction
   *  \return               Expression of last entry in list
   */
  ExprIndex parse_opt_auto_define_list(ExprIndex function_begin);

  /** \brief                  Parse an optional define list.
   *  \param function_begin   index of function begin instruction
   *  \param allow_duplicates Do we allow multiple entries of the same variable/array?
   *  \return                 Expression of last entry in list
   */
  ExprIndex parse_opt_define_list(ExprIndex function_begin, bool allow_duplicates);

  /** \brief                  Parse a define list.
   *  \param function_begin   index of function begin instruction
   *  \param allow_duplicates Do we allow multiple entries of the same variable/array?
   *  \return                 Expression of last entry in list
   */
  ExprIndex parse_define_list(ExprIndex function_begin, bool allow_duplicates);

  /** \brief                  Parse an optional define element.
   *  \param function_begin   index of function begin instruction
   *  \param allow_duplicates Do we allow multiple entries of the same variable/array?
   *  \return                 Expression of last entry in list
   */
  ExprIndex parse_opt_define_element(ExprIndex function_begin, bool allow_duplicates);

  /** \brief                  Parse a define element.
   *  \param function_begin   index of function begin instruction
   *  \param allow_duplicates Do we allow multiple entries of the same variable/array?
   *  \return                 Expression of last entry in list
   */
  ExprIndex parse_define_element(ExprIndex function_begin, bool allow_duplicates);

  /** \brief  Parse optional argument list
   *  \return \a true if production was non-empty
   */
  ExprIndex parse_opt_argument_list();

  /** Parse argument list.  */
  ExprIndex parse_argument_list();

  /** Parse relational expression.  */
  ExprIndex parse_relational_expression();

  /** Parse return expression.  */
  ExprIndex parse_return_expression();

  /** \brief        Parse an optional expression
   *  \param  flags Flags of type of expressions to accept, default primary.
   *  \return       Index of expression (if one given) and type of expression parsed).
   */
  ExprIndex parse_opt_expression(POPEFlags flags = POPEFlags::parse_primary);

  /** \brief        Parse expression.
   *  \param  flags Flags of type of expressions to accept, default primary.
   *  \return       Index of expression's value.
   */
  ExprIndex parse_expression(POPEFlags flags = POPEFlags::parse_primary);

  /** \brief        Parse an optional assignment expression
   *  \param  flags Flags of type of expressions to accept, default primary.
   *  \return       Index of expression, and type of expression.
   */
  ExprIndex parse_opt_assign_expression(POPEFlags flags = POPEFlags::parse_primary);

  /** \brief        Parse an optional assignment expression
   *  \param  flags Flags of type of expressions to accept, default primary.
   *  \return       Index of expression, and type of expression.
   */
  ExprIndex parse_assign_expression(POPEFlags flags = POPEFlags::parse_primary);

  /** \brief  Parse a unary minus expression.
   *  \return  Index of expression, and type of expression.
   */
  ExprIndex parse_unary_minus_expression();

  /** \brief  Parse an inrement/decrement expression.
   *  \return  Index of expression, and type of expression.
   */
  ExprIndex parse_incr_decr_expression();

  /** \brief      Parse an add expression
   *  \return     Index of result
   */
  ExprIndex parse_add_expression();

  /** \brief      Parse a mul expression
   *  \return     Index of result
   */
  ExprIndex parse_mul_expression();

  /** \brief      Parse a power expression
   *  \return     Index of result, may be \a lhs if this isn't a power expression.
   */
  ExprIndex parse_power_expression();

  /** \brief      Parse an add expression
   *  \param  lhs Left hand side of expression
   *  \return     Index of result, may be \a lhs if this isn't an add expression.
   */
  ExprIndex parse_add_expression(ExprIndex lhs);

  /** \brief      Parse a mul expression
   *  \param  lhs Left hand side of expression
   *  \return     Index of result, may be \a lhs if this isn't a mul expression.
   */
  ExprIndex parse_mul_expression(ExprIndex lhs);

  /** \brief      Parse a power expression
   *  \param  lhs Left hand side of expression
   *  \return     Index of result, may be \a lhs if this isn't a power expression.
   */
  ExprIndex parse_power_expression(ExprIndex lhs);

  /** \brief        Parse primary and named expressions
   *  \param  flags Bit mask of flags - default parse primary and named expressions
   *  \return       Expression index.
   *
   * This function will always parse named expression.  The flags will add primary expressions and
   * array slices to the parsing.
   */
  ExprIndex parse_opt_primary_expression(POPEFlags flags = POPEFlags::parse_primary);

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
  ExprIndex parse_primary_expression(POPEFlags flags = POPEFlags::parse_primary);

  /** \brief      Ensure that the value of the expression pointed to by \a idx has been loaded.
   *  \param  idx Index to check the loadedness of
   *  \return     Index of loaded value (may be same as \a idx).
   *
   * Named expressions point to names and not values.  This function ensures that we have loaded
   * the value of the name.
   */
  ExprIndex ensure_expr_loaded(ExprIndex idx);

  /** \brief       Insert error message into stream.
   *  \param  msg  Message ID
   *  \param  args Arguments for the message.
   *  \return      Offset of last instruction of error.
   */
  template<typename... Ts>
  ExprIndex insert_error(Msg msg, Ts... args)
  {
    error_ = true;
    /* If the lexer holds an error token we report that rather than the error message we've been
     * asked to report.
     */
    bool lexer_error = lexer_->peek() == Token::Type::error;
    auto s = insert_string(lexer_error ? lexer_->peek().error() : lexer_->error(msg, args...));
    if (lexer_error) {
      lexer_->chew();
    }
    insert_print(s, Instruction::Stream::stderr);
    return insert_quit(1);
  }

  /** \brief    Insert end of file instruction
   *  \return   Index of inserted string
   */
  ExprIndex insert_eof();

  /** \brief    Insert string at end of instruction stream.
   *  \param  s String to insert
   *  \return   Index of inserted string
   */
  ExprIndex insert_string(std::string const& s);

  /** \brief         Insert print at end of instruction stream.
   *  \param  index  Index of thing to print.
   *  \param  stream Stream to print to.
   *  \return        Index of inserted print
   */
  ExprIndex insert_print(ExprIndex index, Instruction::Stream stream);

  /** \brief         Insert quit at end of instruction stream.
   *  \param  code   Exit code
   *  \return        Index of inserted print
   */
  ExprIndex insert_quit(unsigned code);

  /** \brief        Insert a load.
   *  \param  named Named expression to load
   *  \return       Index to value loaded.
   */
  ExprIndex insert_load(ExprIndex named);

  /** \brief        Insert a store.
   *  \param  named Named expression to store to
   *  \param  value Value to store.
   *  \return       Index to value stored, but with expression type of assignment.
   */
  ExprIndex insert_store(ExprIndex var, ExprIndex value);  // Type == assignment

  /** \brief         Insert an arithmetic operation.
   *  \param  opcode Arithmetic opcode to use
   *  \param  lhs    Left-hand side expression
   *  \param  rhs    Right-hand side expression
   *  \return        Index of result value.
   */
  ExprIndex insert_arith(Instruction::Opcode opcode, ExprIndex lhs, ExprIndex rhs);

  /** \brief         Insert a negation op
   *  \param  expr   Expression to negate
   *  \return        Index of result value.
   */
  ExprIndex insert_negate(ExprIndex expr);

  /** \brief         Insert a number
   *  \param  number Number to store
   *  \return        Index of number.
   */
  ExprIndex insert_number(std::string const& number);

  /** \brief         Insert an array element
   *  \param v       Array
   *  \param element Index of expr to calculate element
   *  \return         Index of inserted instruction
   */
  ExprIndex insert_array_element(Array v, ExprIndex element);  // Type = named

  /** \brief         Insert an array
   *  \param v       Array
   *  \return         Index of inserted instruction
   */
  ExprIndex insert_array_slice(Array v);

  /** \brief          Insert a variable
   *  \param  v       Variable
   *  \return         Index of inserted instruction
   */
  ExprIndex insert_variable(Variable v);  // Type = named

  /** \brief  Insert scale variable - not scale()
   *  \return Index of inserted instruction
   */
  ExprIndex insert_scale();  // Type = named

  /** \brief  Insert obase variable
   *  \return Index of inserted instruction
   */
  ExprIndex insert_obase();  // Type = named

  /** \brief  Insert ibase variable
   *  \return Index of inserted instruction
   */
  ExprIndex insert_ibase();  // Type = named

  /** \brief       Insert scale() call
   *  \param  expr Expression to pass
   *  \return      Index of inserted instruction
   */
  ExprIndex insert_scale_expr(ExprIndex expr);

  /** \brief       Insert sqrt() call
   *  \param  expr Expression to pass
   *  \return      Index of inserted instruction
   */
  ExprIndex insert_sqrt(ExprIndex expr);

  /** \brief       Insert length() call
   *  \param  expr Expression to pass
   *  \return      Index of inserted instruction
   */
  ExprIndex insert_length(ExprIndex expr);

  /** \brief      Insert branch if zero
   *  \param dest Destination
   *  \param cmp  Comparison index
   */
  ExprIndex insert_branch_zero(ExprIndex dest, ExprIndex cmp);

  /** \brief      Insert branch
   *  \param dest Destination
   */
  ExprIndex insert_branch(ExprIndex dest);

  /** \brief      Insert return
   *  \param expr Result of function
   */
  ExprIndex insert_return(ExprIndex expr);

  /** \brief       Insert call() call
   *  \param  v    Function to call
   *  \param  loc  Source location of call (for error reporting purposes).
   *  \return      Index of inserted instruction
   */
  ExprIndex insert_call(Letter v, Location const& loc);

  /** \brief   Insert push_param_mark instruction.
   */
  ExprIndex insert_push_param_mark();

  /** \brief   Insert pop_param_mark instruction.
   */
  ExprIndex insert_pop_param_mark();

  /** \brief   Insert push_param instruction.
   *  \param expr Expression to push
   */
  ExprIndex insert_push_param(ExprIndex expr);

  /** \brief   Insert pop_param instruction.
   */
  ExprIndex insert_pop_param();

  /** \brief   Insert pop_param_array instruction.
   */
  ExprIndex insert_pop_param_array();

  /** \brief      Insert function begin
   *  \param mask Mask of local variables to save
   *  \param loc  Location of function definition
   */
  ExprIndex insert_function_begin(VariableMask mask, Location const& loc);

  /** \brief        Insert function end
   *  \param letter Function we're finishing
   *  \param begin  Index of beginning of function.
   */
  ExprIndex insert_function_end(Letter letter, ExprIndex begin);

  /** \brief Push a new loop state.  */
  void push_loop();

  /** \brief     Add an loop break/exit instruction to the list for the current loop.
   *  \param idx Index of loop exit instruction.
   */
  void add_loop_exit(Index idx);

  /** \brief     Update the break/exit instructions for the current loop to point to idx.
   *  \param idx Index of first instruction after the end of the loop.
   */
  void update_loop_exits(Index idx);

  /** \brief  Pop the loop state.  */
  void pop_loop();

  /** \brief  Are we currently in a loop? */
  bool in_loop() const;

  std::unique_ptr<Lexer> lexer_;                ///< Lexer
  std::shared_ptr<Instructions> instructions_;  ///< Current set of instructions
  std::stack<std::list<Index>> loop_breaks_;    ///< Stack of loop breaks.
  bool interactive_;                            ///< Are we interactive?
  bool error_;                                  ///< Has there been an error?
  bool in_function_;                            ///< Are we in a function?
};

bool operator==(Parser::ExprIndex const& lhs, Parser::ExprIndex const& rhs);
bool operator==(Parser::ExprIndex const& lhs, Parser::Index rhs);
bool operator==(Parser::ExprIndex const& lhs, Parser::ExprType rhs);
bool operator!=(Parser::ExprIndex const& lhs, Parser::ExprIndex const& rhs);
bool operator!=(Parser::ExprIndex const& lhs, Parser::Index rhs);
bool operator!=(Parser::ExprIndex const& lhs, Parser::ExprType rhs);
Parser::Offset operator-(Parser::ExprIndex const& lhs, Parser::ExprIndex const& rhs);

/** \brief  Virtual Machine
 *
 * This is the object that executes instructions.
 */
class VM
{
public:
  /** \brief Constructor
   *  \param stdout Stream to use for standard output.
   *  \param stderr Stream to use for standard error.
   */
  VM(std::ostream& stdout, std::ostream& stderr);

  /** \brief              Execute instructions.
   *  \param instructions Instructions to execute
   *  \return             \c true if we should continue, or \c false if we have reached EOF.
   */
  std::pair<Number, bool> execute(Instructions& instructions);

private:
  using Param = std::variant<ArrayValues, Number>;
  using Params = std::list<Param>;
  using ParamStack = std::list<Params>;
  using Index = Instruction::Index;
  using Offset = Instruction::Offset;
  using NumType = Number::NumType;
  using FunctionDefinition = std::tuple<Instructions, VariableMask, Location>;

  void execute_string(Instructions& instructions, Index i);
  void execute_number(Instructions& instructions, Index i);
  void execute_variable(Instructions& instructions, Index i);
  void execute_array(Instructions& instructions, Index i);
  void execute_array_element(Instructions& instructions, Index i);
  void execute_ibase(Instructions& instructions, Index i);
  void execute_obase(Instructions& instructions, Index i);
  void execute_scale(Instructions& instructions, Index i);
  void execute_print(Instructions& instructions, Index i);
  void execute_quit(Instructions& instructions, Index i);
  void execute_load(Instructions& instructions, Index i);
  void execute_store(Instructions& instructions, Index i);
  void execute_negate(Instructions& instructions, Index i);
  void execute_add(Instructions& instructions, Index i);
  void execute_sub(Instructions& instructions, Index i);
  void execute_less_than(Instructions& instructions, Index i);
  void execute_less_than_equals(Instructions& instructions, Index i);
  void execute_equals(Instructions& instructions, Index i);
  void execute_not_equals(Instructions& instructions, Index i);
  Index execute_branch(Instructions& instructions, Index i);
  Index execute_branch_zero(Instructions& instructions, Index i);
  Index execute_function_begin(Instructions& instructions, Index i);
  void execute_call(Instructions& instructions, Index i);
  Number execute_return(Instructions& instructions, Index i);
  void execute_push_param_mark(Instructions& instructions, Index i);
  void execute_pop_param_mark(Instructions& instructions, Index i);

  Number get_op1_expr(Instructions& instructions, Index i);
  Number get_op2_expr(Instructions& instructions, Index i);

  void set_ibase(Number num);
  void set_obase(Number num);
  void set_scale(Number num);

  Number get(ArrayElement const& ae) const;
  void set(ArrayElement const& ae, Number num);

  std::ostream& stdout_;
  std::ostream& stderr_;
  std::array<ArrayValues, Letter::count_> arrays_;
  std::array<std::optional<FunctionDefinition>, Letter::count_> functions_;
  std::array<Number, Letter::count_> variables_;
  ParamStack param_stack_;  ///< Stack of parameters
  ParamStack local_stack_;  ///< Stack of locals.

  NumType ibase_ = 10;
  NumType obase_ = 10;
  NumType scale_ = 0;
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

template<>
struct fmt::formatter<GD::Bc::Letter>
{
  constexpr auto parse(format_parse_context& ctx)
  {
    if (ctx.begin() != ctx.end() && *ctx.begin() != '}') {
      throw format_error("invalid format");
    }
    return ctx.begin();
  }

  template<typename FormatContext>
  auto format(GD::Bc::Letter letter, FormatContext& ctx)
  {
    static std::string_view letters = "abcdefghijklmnopqrstuvwxyz";
    return format_to(ctx.out(), "{0}", letters[static_cast<unsigned>(letter)]);
  }
};
#endif  //  _SRC_BC_BC_HH_INCLUDED
