/** \file   libcpp/include/file-store.hh
 *  \brief  File Management
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_FILE_STORE_HH_INCLUDED_
#define CC_LIBCPP_FILE_STORE_HH_INCLUDED_

#include "gd/format.hh"

#include <cstdint>
#include <fstream>
#include <iosfwd>
#include <map>
#include <stack>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "error.hh"
#include "location.hh"
#include "token.hh"

namespace GD::CPP {

/** \brief  Manages files and reading from them.
 *
 * The file store gets you a tokenizer which produces character tokens, and manages the stack of
 * files being read.  It only reads each file/stream once, assuming that the contents will not
 * change during a compilation step.
 *
 * File stores can not be copied, but they can be moved.
 *
 * It provides peek()/chew() methods to read characters from the streams.
 */
class FileStore
{
public:
  /** \brief               Construct a file store
   *  \param error_manager Error Manager to use for error generation.
   */
  FileStore(ErrorManager& error_manager);

  FileStore() = delete;
  FileStore(FileStore const&) = delete;
  FileStore(FileStore&&) noexcept = default;
  auto operator=(FileStore const&) -> FileStore& = delete;
  auto operator=(FileStore&&) noexcept -> FileStore& = default;
  ~FileStore() = default;

  /** \brief  Peek the next token from the token stream.
   *  \return Next token in the stream.
   */
  auto peek() -> Token const&;

  /** \brief  Chew the current token, accepting any.
   */
  void chew();

  /** \brief        Chew the current token, it must have the given token type.
   *  \param  type_ Expected token type.
   */
  void chew(TokenType type_);

  /** \brief             Chew the current token, it must have one of the given token types.
   *  \param type_begin_ Begin iterator for types we accept
   *  \param type_end_   End iterator for types we accept.
   */
  template<typename It>
  void chew(It type_begin, It type_end)
  {
    if (!token_) {
      do_peek();
    }
    if (std::none_of(type_begin, type_end, token_->type())) {
      assert_ice(std::any_of(type_begin, type_end, token_->type()),
                 "Token must be one of specified types.");
    }
    token_.reset();
  }

  /** \brief       Push a stream.
   *  \param  name Name to give the stream.
   *  \param  is   Input stream.
   *
   * This is mostly used by testing - but may have a use if you want to parse a generated text
   * string of data.
   */
  void push_stream(std::string const& name, std::istream& is);

  /** \brief        Push a file into the tokenizer stack
   *  \param  fname File name.
   */
  void push_file(std::string const& fname);

  /** \brief  Push the standard input onto the tokenizer stack.
   *  \return Tokenizer for the stream.
   */
  void push_standard_input();

  /** \brief          State the logical location of the next line
   *  \param filename File name
   *  \param line     Line number for next line
   */
  void map_next_logical_location(std::string const& filename, Line line);

  /** \brief  Get the location that identifies the command line. */
  static auto cmd_line_location() noexcept -> Location;

  /** \brief      Get the physical source code line for the given location.
   *  \param  loc Location
   *  \return     Source code line.
   *
   * Empty string is returned if there is no source code for a given location.
   */
  auto line(Location loc) const -> std::string const&;

  /** \brief        Get the physical souce code line that contains the range.
   *  \param  range Range
   *  \return       Source code line.
   *
   * An empty string is returned if there is no source code for the range.
   */
  auto line(Range range) const -> std::string const&;

  /** \brief        Get a 'caret' string that highlights a range of code.
   *  \param  range Range to highlight
   *  \return       Caret string.
   *
   * \subsection CaretUsage Usage
   *
   * The following code sequence will display a line of source code with a following line
   * highlighting the range.
   *
   * \code
   * std::cout << line(range) << caret(range);
   * \endcode
   */
  auto caret(Range range) const -> std::string;

  /** \brief        Get an iterator to the first character in a range.
   *  \param  range Range to query
   *  \return       Iterator to beginning of range.
   */
  auto range_begin(Range range) const -> std::string::const_iterator;

  /** \brief        Get an iterator to one past the end of a character in a range.
   *  \param  range Range to query
   *  \return       Iterator to beginning of range.
   */
  auto range_end(Range range) const -> std::string::const_iterator;

  /** \brief      Is a location in the top-level?
   *  \param  loc Location
   *  \return     True iff \a loc is a location.
   */
  auto is_top_level(Location loc) const noexcept -> bool;

  /** \brief      Get the parent location of a given location.
   *  \param  loc Location
   *  \return     Paremt location.  Top-level location returns itself.
   */
  auto parent_location(Location loc) const noexcept -> Location;

  /** \brief      Get the logical filename for a given location.
   *  \param  loc Location
   *  \return     Filename, or empty.
   */
  auto logical_filename(Location loc) const noexcept -> std::string const&;

  /** \brief      Get the logical line number for a given location.
   *  \param  loc Location
   *  \return     1-based Line number, 0 if location is illegal.
   */
  auto logical_line(Location loc) const noexcept -> Line;

  /** \brief      Get the logical column for a given location.
   *  \param  loc Location
   *  \return     Column number, starting at 1 for the first column. 0 if illegal location.
   */
  auto logical_column(Location loc) const noexcept -> Column;

  /** \brief      Get the physical filename for a given location.
   *  \param  loc Location
   *  \return     Filename, or empty.
   */
  auto physical_filename(Location loc) const noexcept -> std::string const&;

  /** \brief      Get the logical line number for a given location.
   *  \param  loc Location
   *  \return     1-based Line number, 0 if location is illegal.
   */
  auto physical_line(Location loc) const noexcept -> Line;

  /** \brief      Get the logical column for a given location.
   *  \param  loc Location
   *  \return     Column number, starting at 1 for the first column. 0 if illegal location.
   */
  auto physical_column(Location loc) const noexcept -> Column;

private:
  /** \brief  Information about a particular line of code.
   *
   * This stores details about a line of code - including the logical (flile, line, column) triple
   * of the first character, and the location ID of the first character.
   *
   * The physical file is stored in the parent details, the physical line number is 1 + the index
   * of this entry in LocationDetails::lines_.
   */
  struct LineDetails
  {
    Location begin_;            ///< Location of first character in the line.
    std::size_t logical_file_;  ///< ID of the logical file.
    Line logical_line_;         ///< Logical line number
    Column logical_column_;     ///< Logical column number of first character
  };

  /** \brief Details for each line in an included file.
   *
   * Each instance of this refers to an included file - so the same file may have multiple
   * LocationDetails objects, once for every time it has been included.
   */
  struct LocationDetails
  {
    LocationDetails() = default;

    /** \brief Destructor.  */
    ~LocationDetails();

    /** \brief      Find the line number associated with a given location.
     *  \param  loc Location to query.
     *  \return     Line number associated with the location, or \c illegal_line if location isn't
     *              in the range for this object.
     */
    auto find_line(Location loc) const -> Line;

    /** \brief      Find the line details associated with a given location.
     *  \param  loc Location to query.
     *  \return     Line details associated with the location, or \c nullptr if location isn't
     *              in the range for this object.
     */
    auto find_line_details(Location loc) const -> LineDetails const*;

    Location begin_;                          ///< Location of the first character in the file
    Location end_;                            ///< One past end location of this file.
    std::size_t physical_file_;               ///< Physical File ID.
    std::vector<LineDetails> lines_;          ///< Details for each line.
    std::vector<LocationDetails*> children_;  ///< Details for each child.
  };

  /** \brief Location stack for includes.  */
  struct LocationStack
  {
    /** \brief  Construct stack entry.
     *  \param loc_details Location details for current included file
     */
    LocationStack(LocationDetails& loc_details);

    LocationDetails& loc_details_;  ///< Location details of include file
    Line physical_line_;            ///< Next physical line
    std::size_t logical_file_;      ///< Logical File of the next line
    Line logical_line_;             ///< Logical line number of the next line.
  };

  using FileLines = std::pair<std::istream&, std::vector<std::string>>;
  using Files = std::map<std::size_t, FileLines>;

  /** \brief           Get the filename ID for a filename.
   *  \param  filename File name to get ID of.
   *  \return          ID.
   *
   * Will always succeed, adding a new ID if necessary.
   */
  auto find_filename_id(std::string const& filename) -> std::size_t;

  /** \brief      Find the location details object for a given location.
   *  \param  loc Location to look up.
   *  \return     Location details.
   */
  auto find_loc_details(Location loc) const -> LocationDetails const&;

  /** \brief  Peek the next token.  */
  void do_peek();

  /** \brief  Update token_ with the next character.
   *
   * Assumes that the current line has more bytes to go through.
   */
  void peek_character();

  /** \brief  Read the next line.  */
  void peek_next_line();

  /** \brief Pop a file.  */
  void pop_file();

  static constexpr auto illegal_line = Line{std::numeric_limits<std::size_t>::max()};
  static const std::string empty_;  ///< An empty string.

  ErrorManager& error_manager_;               ///< Error manager
  Files physical_files_;                      ///< Map of read files
  std::vector<std::string> file_names_;       ///< Array of file names
  LocationDetails cmd_line_location_;         ///< Location details for the command line
  std::stack<LocationStack> location_stack_;  ///< Stack of include locations
  std::vector<std::fstream> streams_;         ///< The streams we own.
  Location next_;                             ///< Location for the next token.
  std::string::const_iterator next_begin_;    ///< Pointer to the first character of the next token.
  std::string::const_iterator line_end_;      ///< End of current line being parsed
  std::optional<Token> token_;                ///< Current token (if we have one)
};
}  // namespace GD::CPP

#endif  // CC_LIBCPP_FILE_STORE_HH_INCLUDED_
