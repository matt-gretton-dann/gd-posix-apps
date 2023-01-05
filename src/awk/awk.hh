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
#include <regex>
#include <sstream>
#include <stack>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

namespace GD::Awk {

namespace Details {
class ParseState;
}  // namespace Details

/** Internal type to differentiate a name.  */
using Name = TypeWrapper<std::string, struct NameTag>;

/** Internal type to differentiate an ERE.  */
using ERE = TypeWrapper<std::string, struct ERETag>;

/** Internal type to differentiate a function name.  */
using FuncName = TypeWrapper<std::string, struct FuncNameTag>;

/** \brief A variable name.  Just a tagged string. */
using VariableName = Name;

/** What we use to represent an integer
 *
 * POSIX says that we should treat 'integers' as signed longs.  Even though this means different
 * behaviours on LP64 and LLP64 systems.
 */
using Integer = TypeWrapper<signed long, struct IntegerTag>;  // NOLINT(google-runtime-int)

/** What we use to represent a floating point number: */
using Floating = double;

/** Type to hold a file descriptor.  */
using FileDescriptor = TypeWrapper<int, struct FDTag>;

/** Type representing an index into a list of instructions or results.  */
using Index = std::size_t;

/** An illegal index.  */
static constexpr auto illegal_index{std::numeric_limits<Index>::max()};

/** \brief  Token type.
 *
 * Very basic token type stores type along with any extra info needed.
 */
struct Token
{
  /** \brief  Enum listing builtin functions. */
  enum class BuiltinFunc {
    atan2,
    close,
    cos,
    exp,
    gsub,
    index,
    int_,
    length,
    log,
    match,
    rand,
    sin,
    split,
    sprintf,
    sqrt,
    srand,
    sub,
    substr,
    system,
    tolower,
    toupper,
  };

  /** \brief  Enum listing token types.  */
  enum class Type {
    error,
    eof,
    newline,
    name,
    func_name,
    builtin_func_name,
    string,
    floating,
    integer,
    ere,
    begin,
    break_,
    continue_,
    delete_,
    do_,
    else_,
    end,
    exit,
    for_,
    function,
    getline,
    if_,
    in,
    next,
    print,
    printf,
    return_,
    while_,
    add_assign,
    sub_assign,
    mul_assign,
    div_assign,
    mod_assign,
    pow_assign,
    or_,
    and_,
    no_match,
    eq,
    le,
    ge,
    ne,
    incr,
    decr,
    append,
    lbrace,
    rbrace,
    lparens,
    rparens,
    lsquare,
    rsquare,
    comma,
    semicolon,
    add,
    subtract,
    multiply,
    divide,
    modulo,
    power,
    not_,
    greater_than,
    less_than,
    pipe,
    query,
    colon,
    tilde,
    dollar,
    assign,
  };

  /** \brief      Construct a token of basic type
   *  \param type Token type
   */
  explicit Token(Type type);

  /** \brief      Construct a token of type Type::name, Type::func_name,
   *              Type::ere, Type::string, or Type::error.
   *  \param type Token type
   *  \param s    Value of number or string
   *
   * Numbers are stored as strings as we can't interpret their value until we know the input base
   * until execution time.
   */
  Token(Type type, std::string const& s);

  /** \brief  Construct a token of Type::builtin_func.
   *  \param type Token type (Builtin_func)
   *  \param func Builtin function
   */
  Token(Type type, BuiltinFunc func);

  /** @brief         Construct a token of Type::integer
   *  @param type    Type::integer
   *  @param integer Integer to store
   */
  Token(Type type, Integer integer);

  /** @brief          Construct a token of Type::floating
   *  @param type     Type::floating
   *  @param floating Floating point number to store
   */
  Token(Type type, Floating floating);

  /** \brief Get token type.  */
  [[nodiscard]] auto type() const -> Type;

  /** \brief  Get name stored in token. */
  [[nodiscard]] auto name() const -> std::string const&;

  /** \brief  Get the function name stored in token. */
  [[nodiscard]] auto func_name() const -> std::string const&;

  /** \brief  Get the ERE name stored in token. */
  [[nodiscard]] auto ere() const -> std::string const&;

  /** \brief  Get integer number stored in token. */
  [[nodiscard]] auto integer() const -> Integer;

  /** \brief  Get floating-point number stored in token. */
  [[nodiscard]] auto floating() const -> Floating;

  /** \brief  Get string stored in token. */
  [[nodiscard]] auto string() const -> std::string const&;

  /** \brief  Get the error message.  */
  [[nodiscard]] auto error() const -> std::string const&;

  /** \brief  Get the builtin function type.  */
  [[nodiscard]] auto builtin_func_name() const -> BuiltinFunc;

  /** \brief  Output debug form of token. */
  void debug(std::ostream& os) const;

private:
  /** Internal type to hold an error string and differentiate it from string and number.  */
  using Error = TypeWrapper<std::string, struct ErrorIntType>;

  /** The value of thos token.  */
  std::variant<Type, BuiltinFunc, std::string, Name, ERE, FuncName, Integer, Floating, Error>
    value_;
};

/** \brief Output operator for token type.  */
auto operator<<(std::ostream& os, Token::Type t) -> std::ostream&;
auto operator<<(std::ostream& os, Token::BuiltinFunc bf) -> std::ostream&;
auto operator<<(std::ostream& os, Token const& token) -> std::ostream&;

/* Comparison operators.  */
auto operator==(Token const& token, Token::Type type) -> bool;
auto operator==(Token const& token, Token::BuiltinFunc builtin_func) -> bool;
auto operator==(Token::Type type, Token const& token) -> bool;
auto operator!=(Token const& token, Token::Type type) -> bool;
auto operator!=(Token const& token, Token::BuiltinFunc builtin_func) -> bool;
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

  /** \brief                Peek the current token.
   *  \param  enable_divide Enable the divide tokens.
   *  \return               Peeked token.
   *
   * Not const as we peek lazily.
   */
  auto peek(bool enable_divide) -> Token const&;

  /** \brief                Chew the current token.
   *  \param  enable_divide Enable the divide tokens.
   */
  void chew(bool enable_divide);

  /** @brief  Lex a string from the command line.
   *
   * This basically treats the rest of the characters to be tokenized as a string which is between
   * two quotes.  This is used to lex the second half of the VAR=NAME command line options and
   * operands.
   *
   * Peeked token will have type Token::Type::String.
   */
  auto peek_cmdline_string() -> Token const&;

  /** \brief  Get the current location. */
  [[nodiscard]] auto location() const -> Location const&;

  /** \brief  Generate an error message with current location information added.  */
  template<typename... Ts>
  auto error(Msg msg, Ts... args) -> std::string
  {
    return r_->error(msg, args...);
  }

private:
  /** @brief              Lexer.  Main entry point
   *  @param allow_divide are / and /= allowed?
   *
   * All lexing routines end up by calling t_.emplace() to construct an appropriate token.
   */
  void lex(bool allow_divide);

  /** Lex the &&. */
  void lex_and();

  /** Lex a comment.
   *
   * On entry t_->peek() should point to the # at the start of the comment.
   */
  void lex_comment();

  /** Lex a number.
   *
   * On entry t_->peek() should point to the first digit of the number.
   */
  void lex_number();

  /**         Lex an octal-escape.
   *  @return Character encoded in octal sequence.
   *
   * On entry t_->peek() should point to the first digit of the escape sequence.
   */
  auto lex_octal_escape() -> char;

  /** Lex a string or regular expression.
   * @param from_cmdline true if this is a string from the command line.
   *
   * On entry, t_->peek() should point to the opening " or / of the escape sequence.
   */
  void lex_string_or_ere(bool from_cmdline);

  /** Lex a word (name, func_name, or builtin_func_name). */
  void lex_word();

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
  std::optional<Token> t2_;    ///< Second pending token.
};

/** \brief  An Instruction.
 *
 * Instructions contain an opcode, and up to two, statically typed, operands.
 *
 * When executing instructions we assume we have a side-array that contains the result for the last
 * time each instruction was executed.  For example om how this works if we have:
 *
 *   0: load_variable a
 *   1: load_variable b
 *   2: add 0, 1
 *
 *  Then the contents of the array after execution will be:
 *   0: numeric value of variable a
 *   1: numeric value of variable b
 *   2: value of a + b
 *
 * Key for opcode table:
 *  * B: castable to Boolean
 *  * BFN: Builtin function name
 *  * I: castable to Integer
 *  * Ix(t): Index to the result of another instruction, t is the type of result expected.
 *  * F: castable Floating
 *  * FD: File descriptor
 *  * FL: Field ID
 *  * FN: Function name
 *  * PP: Parameter pack
 *  * O(r): Offset to the result of a different instruction. r is type of result.
 *  * R: Regex
 *  * S: castable to String
 *  * VN: Variable name
 *
 * | Opcode                |  Operand 1  |  Operand 2  |  Description                             |
 * | :-----------------    | :---------- | :---------- | :--------------------------------------- |
 * | field                 | Ix(I)       |             | Calculate the field ID from <op1>        |
 * | variable              | VN          |             | Name of a variable                       |
 * | load_literal          | I/F/S/R/FD  |             | Result is the literal in <op1>           |
 * | load_lvalue           | Ix(VN/FL)   |             | Load the lvalue identified by <op1>      |
 * | load_svalue           | Ix(VN/FL)   | Ix(I/F/S)   | Store <op2> into <op1>                   |
 * | print                 | Ix(I/F/S)   | Ix(FD)      | Print the value <op1> to stream <op2>    |
 * | printf                | Ix(I/F/S)   | Ix(FD)      | printf the params <op1> to stream <op2>  |
 * | open_param_pack       |             |             | Open a parameter pack                    |
 * | push_param            | Ix(PP)      | Ix(I/F/S)   | Push expr <op2> onto the front of <op1>  |
 * | close_param_pack      | Ix(PP)      |             | Close the parameter pack <op1>           |
 * | add                   | Ix(I/F)     | Ix(I/F)     | <op1> + <op2>                            |
 * | sub                   | Ix(I/F)     | Ix(I/F)     | <op1> - <op2>                            |
 * | power                 | Ix(I/F)     | Ix(I/F)     | <op1> ^ <op2>                            |
 * | to_number             | Ix(I/F)     |             | cast op1 to a number                     |
 * | to_bool               | Ix(B)       |             | cast op1 to a bool                       |
 * | negate                | Ix(I/F)     |             | -<op1>                                   |
 * | logical_not           | Ix(B)       |             | !<op1>                                   |
 * | multiply              | Ix(I/F)     | Ix(I/F)     | <op1> * <op2>                            |
 * | divide                | Ix(I/F)     | Ix(I/F)     | <op1> / <op2>                            |
 * | modulo                | Ix(I/F)     | Ix(I/F)     | <op1> % <op2>                            |
 * | concat                | Ix(S)       | Ix(S)       | concatenate <op1> and <op2>              |
 * | is_equal              | Ix(I/F/S)   | Ix(I/F/S)   | <op1> == <op2>                           |
 * | is_not_equal          | Ix(I/F/S)   | Ix(I/F/S)   | <op1> != <op2>                           |
 * | is_less_than          | Ix(I/F/S)   | Ix(I/F/S)   | <op1> <  <op2>                           |
 * | is_less_than_equal    | Ix(I/F/S)   | Ix(I/F/S)   | <op1> <= <op2>                           |
 * | is_greater_than       | Ix(I/F/S)   | Ix(I/F/S)   | <op1> >  <op2>                           |
 * | is_greater_than_equal | Ix(I/F/S)   | Ix(I/F/S)   | <op1> >= <op2>                           |
 * | branch_if_false       | Ix(B)       | Ix          | Branch to <op2> if <op1> is false        |
 * | re_match              | Ix(S)       | Ix(R)       | <op1> ~ <op2>                            |
 * | logical_or            | Ix(B)       | Ix(B)       | <op1> && <op2>                           |
 * | logical_and           | Ix(B)       | Ix(B)       | <op1> || <op2>                           |
 * | reserve_reg           | Ix          |             | Reserve space for <op1> result registers |
 * | open                  | Ix(S)       | I           | Open <op1>, If I is 1 append             |
 * | popen                 | Ix(S)       |             | Open <op1> as a process                  |
 *
 * Parameter packs are identified by the index of the instruction corresponding to the
 * 'open_param_pack'.
 */
class Instruction
{
public:
  /** Opcodes. */
  enum class Opcode {
    field,                  ///< A field lvalue
    variable,               ///< A variable lvalue
    load_literal,           ///< Load a literal value
    load_lvalue,            ///< Load an lvalue
    store_lvalue,           ///< Store an lvalue
    print,                  ///< Print the value referenced by <op1> to stream <op2>.
    printf,                 ///< Printf the parameter pack <op1> to stream <op2>.
    open_param_pack,        ///< Open a parameter pack.  Result is parameter pack ID.
    push_param,             ///< Push parameter <op2> onto the front of parameter pack <op1>.
    close_param_pack,       ///< Close parameter pack <op1>.
    add,                    ///< Numeric addition
    sub,                    ///< Numeric subtraction
    power,                  ///< Exponentation
    to_number,              ///< Cast to number
    to_bool,                ///< Cast to bool
    negate,                 ///< negation
    logical_not,            ///< Logical not
    multiply,               ///< Multiplication
    divide,                 ///< Division
    modulo,                 ///< Modulus
    concat,                 /// String concatenation
    is_equal,               ///< equality comparison
    is_not_equal,           ///< inequality comparison
    is_less_than,           ///< less than comparison
    is_less_than_equal,     ///< <= comparison
    is_greater_than,        ///< > comparison
    is_greater_than_equal,  ///< >= comparison
    branch_if_false,        ///< Branch to op2 if op1 is false
    re_match,               ///< Regular expression match
    logical_and,            ///< Logical and
    logical_or,             ///< Logical or
    reserve_regs,           ///< Reserve space for <op1> result registers
    open,                   ///< Open a file
    popen,                  ///< Open a process
  };

  /** Type representing an offset of to an instruction. */
  using Offset = std::make_signed_t<Index>;

  /** Valid operand types.  */
  using Operand =
    std::variant<std::string, VariableName, Integer, Floating, std::regex, FileDescriptor, Index>;

  /** \brief        Constructor
   *  \param opcode Opcode
   */
  explicit Instruction(Opcode opcode);

  /** \brief        Constructor
   *  \param opcode Opcode
   *  \param reg    Register for the result to go in
   */
  Instruction(Opcode opcode, Index reg);

  /** \brief        Constructor
   *  \param opcode Opcode
   *  \param op1    First operand
   */
  Instruction(Opcode opcode, Operand const& op1);

  /** \brief        Constructor
   *  \param opcode Opcode
   *  \param reg    Register for the result to go in
   *  \param op1    First operand
   */
  Instruction(Opcode opcode, Index reg, Operand const& op1);

  /** \brief        Constructor
   *  \param opcode Opcode
   *  \param op1    First  operand
   *  \param op2    Second operand
   */
  Instruction(Opcode opcode, Operand const& op1, Operand const& op2);

  /** \brief        Constructor
   *  \param opcode Opcode
   *  \param reg    Register for the result to go in
   *  \param op1    First  operand
   *  \param op2    Second operand
   */
  Instruction(Opcode opcode, Index reg, Operand const& op1, Operand const& op2);

  /** Get opcode */
  [[nodiscard]] auto opcode() const noexcept -> Opcode;

  /** Do we have a reult register? */
  [[nodiscard]] auto has_reg() const noexcept -> bool;

  /** Get the result register.  */
  [[nodiscard]] auto reg() const noexcept -> Index;

  /** Do we have op1? */
  [[nodiscard]] auto has_op1() const noexcept -> bool;

  /** Get operand 1.  */
  [[nodiscard]] auto op1() const -> Operand const&;

  /** Update operand 1.  */
  void op1(Operand const& operand);

  /** Do we have op2? */
  [[nodiscard]] auto has_op2() const noexcept -> bool;

  /** Get operand 2.  */
  [[nodiscard]] auto op2() const -> Operand const&;

  /** Update operand 2.  */
  void op2(Operand const& operand);

private:
  /** Do we have result? */
  [[nodiscard]] static auto has_result(Opcode opcode) noexcept -> bool;

  /** Does \a opcode have op1? */
  [[nodiscard]] static auto has_op1(Opcode opcode) noexcept -> bool;

  /** Does \a opcode have op2? */
  [[nodiscard]] static auto has_op2(Opcode opcode) noexcept -> bool;

  /** \brief  How many operands does \a opcode take? */
  [[nodiscard]] static auto op_count(Opcode opcode) noexcept -> unsigned;

  /** \brief  Validate the operands.  */
  void validate_operands() const;

  Opcode opcode_;               ///< Opcode
  Index reg_{illegal_index};    ///< Result location
  std::optional<Operand> op1_;  ///< Operand 1
  std::optional<Operand> op2_;  ///< Operand 2
};

/** Vector of instructions.  */
using Instructions = std::vector<Instruction>;

auto operator<<(std::ostream& os, Instruction::Opcode opcode) -> std::ostream&;
auto operator<<(std::ostream& os, Instruction::Operand const& operand) -> std::ostream&;
auto operator<<(std::ostream& os, Instruction const& instruction) -> std::ostream&;
auto operator<<(std::ostream& os, Instructions const& instruction) -> std::ostream&;

/** \brief A parsed program.
 *
 * A parsed program contains instruction sequences for:
 *    * The BEGIN block
 *    * The END block
 *    * The pattern matching block - executed for each record.
 *    * Any function definitions.
 */
class ParsedProgram
{
public:
  ParsedProgram() noexcept = default;
  ~ParsedProgram() noexcept = default;

  ParsedProgram(ParsedProgram const&) = delete;
  ParsedProgram(ParsedProgram&&) noexcept = default;
  auto operator=(ParsedProgram const&) -> ParsedProgram& = delete;
  auto operator=(ParsedProgram&&) noexcept -> ParsedProgram& = default;

  using InstructionIterators =
    std::pair<Instructions::const_iterator, Instructions::const_iterator>;

  /** \brief  Get the begin and end iterators for the BEGIN block instructions.
   *  \return pair of begin, end iterators.
   */
  [[nodiscard]] auto begin_instructions() const noexcept -> InstructionIterators;

  /** \brief  Get the begin and end iterators for the instructions to run over each record.
   *  \return pair of begin, end iterators.
   */
  [[nodiscard]] auto per_record_instructions() const noexcept -> InstructionIterators;

  /** \brief  Get the begin and end iterators for the END block instructions.
   *  \return pair of begin, end iterators.
   */
  [[nodiscard]] auto end_instructions() const noexcept -> InstructionIterators;

  /** \brief       Get the begin and end iterators for the given function.
   *  \param  name Function name.
   *  \return      pair of begin, end iterators.
   */
  [[nodiscard]] auto function_instructions(std::string const& name) const noexcept
    -> InstructionIterators;

private:
  friend class Details::ParseState;

  /** \brief Get BEGIN instructions for editing. */
  [[nodiscard]] auto begin() noexcept -> Instructions&;

  /** \brief Get BEGIN instructions for reference. */
  [[nodiscard]] auto begin() const noexcept -> Instructions const&;

  /** \brief Get per-record instructions for editing. */
  [[nodiscard]] auto per_record() noexcept -> Instructions&;

  /** \brief Get per-record instructions for reference. */
  [[nodiscard]] auto per_record() const noexcept -> Instructions const&;

  /** \brief Get END instructions for editing. */
  [[nodiscard]] auto end() noexcept -> Instructions&;

  /** \brief Get END instructions for reference. */
  [[nodiscard]] auto end() const noexcept -> Instructions const&;

  /** \brief       Get the instructions for the named function.  Creating it if it doesn't exist.
   *  \param  name Function name.
   *  \return      Instructions.
   */
  [[nodiscard]] auto function(std::string const& name) noexcept -> Instructions&;

  /** \brief       Get the instructions for the named function.
   *  \param  name Function name.
   *  \return      Instructions.
   */
  [[nodiscard]] auto function(std::string const& name) const -> Instructions const&;

  Instructions begin_;                             ///< BEGIN block instructions.
  Instructions per_record_;                        ///< Per record instructions.
  Instructions end_;                               ///< END block instructions.
  std::map<std::string, Instructions> functions_;  ///< Instructions for functions.
};

/** \brief        Parse a program.
 *  \param  lexer The Lexer to use to read the code.
 *  \return       The parsed program.
 */
auto parse(std::unique_ptr<Lexer>&& lexer) -> ParsedProgram;

/** @brief Execute a program
 *
 * @param program      The program to execute
 * @param initial_vars Initial variables string (in form 'VAR=value')
 * @param cmd_line     Command line files & variables.
 */
void execute(ParsedProgram const& program, std::vector<std::string> const& initial_vars,
             std::vector<std::string> const& cmd_line);
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
