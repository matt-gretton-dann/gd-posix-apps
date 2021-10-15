/** \file   libcpp/include/file-store.hh
 *  \brief  File storage class
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "file-store.hh"

#include <cassert>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <numeric>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace {
/** \brief  Count the number of characters in a string.
 *
 * This treats the characters as UTF-8 code-points.  So multiple bytes may turn into a fewer
 * number of characters.
 */
auto count_chars(std::string::const_iterator begin, std::string::const_iterator end) noexcept
  -> GD::CPP::Column
{
  return static_cast<GD::CPP::Column>(std::count_if(begin, end, [](char c) {
    constexpr auto mask = static_cast<char>(0xe0);
    constexpr auto reject = static_cast<char>(0x80);
    return (c & mask) != reject;
  }));
}
}  // namespace

const std::string GD::CPP::FileStore::empty_{};

auto GD::CPP::FileStore::peek() -> Token const&
{
  if (!token_) {
    do_peek();
  }
  assert(token_.has_value());  // NOLINT
  return *token_;
}

void GD::CPP::FileStore::chew()
{
  if (!token_) {
    do_peek();
  }
  token_.reset();
}

void GD::CPP::FileStore::chew(TokenType type_)
{
  if (!token_) {
    do_peek();
  }
  if (token_->type() != type_) {
    token_.emplace(TokenType::error, Range{next_},
                   error_manager_.error(ErrorCode::token_chew_error, token_->type()));
    return;
  }
  token_.reset();
}

void GD::CPP::FileStore::do_peek()
{
  assert(!token_.has_value());  // NOLINT
  if (error_.has_value()) {
    token_.emplace(TokenType::error, Range(next_), error_.value());
    error_.reset();
    return;
  }

  if (next_begin_ == line_end_) {
    peek_next_line();
  }

  if (!token_) {
    peek_character();
  }
}

void GD::CPP::FileStore::peek_character()
{
  assert(next_begin_ != line_end_);  // NOLINT
  auto end = next_begin_;
  constexpr auto top_bit = static_cast<char>(0x80);
  constexpr auto cont_mask = static_cast<char>(0xe0);

  if ((*end & top_bit) == 0) {
    ++end;  // NOLINT
  }
  else {
    do {
      ++end;  // NOLINT
    } while (end != line_end_ && (*end & cont_mask) == top_bit);
  }

  auto len = end - next_begin_;
  Range range(next_, len);
  token_.emplace(TokenType::character, range);

  next_ = next_ + static_cast<Column>(len);
  next_begin_ = end;
}

GD::CPP::FileStore::FileStore(ErrorManager& error_manager) : error_manager_(error_manager)
{
  file_names_.emplace_back("(command line)");
  next_ = Location{1};

  cmd_line_location_.begin_ = Location(0);
  cmd_line_location_.end_ = Location(std::numeric_limits<std::underlying_type_t<Location>>::max());
  cmd_line_location_.physical_file_ = 0;

  location_stack_.emplace(cmd_line_location_);
}

void GD::CPP::FileStore::push_stream(std::string const& name, std::istream& is)
{
  std::size_t index = find_filename_id(name);

  // Ensure we have somewhere to put the lines we read.
  auto it2 = physical_files_.find(index);
  if (it2 == physical_files_.end()) {
    // Ensure we turn exceptions on for the stream.
    is.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    auto [it3, success] =
      physical_files_.insert(std::make_pair(index, FileLines{is, std::vector<std::string>{}}));
    it2 = it3;
  }

  // Add the location to the location stack.
  auto& current = location_stack_.top();
  if (current.loc_details_.begin_ == next_) {
    error_.emplace(error_manager_.error(ErrorCode::bad_location_push));
  }
  else {
    auto* new_file = new LocationDetails();
    new_file->begin_ = next_;
    new_file->end_ = Location(std::numeric_limits<std::underlying_type_t<Location>>::max());
    new_file->physical_file_ = index;

    current.loc_details_.children_.push_back(new_file);
    location_stack_.emplace(*new_file);
  }
}

void GD::CPP::FileStore::push_file(std::string const& fname)
{
  // Look the file name up to see if we've already opened it - and if so
  auto it = std::find(file_names_.begin(), file_names_.end(), fname);
  std::size_t index = it - file_names_.end();
  if (index != file_names_.size()) {
    auto it2 = physical_files_.find(index);
    if (it2 != physical_files_.end()) {
      push_stream(fname, it2->second.first);
      return;
    }
  }

  // Create the stream and put it somewhere that will live long enough.
  streams_.emplace_back();
  streams_.back().exceptions(std::ifstream::failbit | std::ifstream::badbit);
  try {
    streams_.back().open(fname);
  }
  catch (std::exception const& e) {
    error_.emplace(error_manager_.error(ErrorCode::file_error, e.what()));
  }
  push_stream(fname, streams_.back());
}

void GD::CPP::FileStore::push_standard_input() { push_stream("(standard input)", std::cin); }

void GD::CPP::FileStore::pop_file()
{
  auto& current = location_stack_.top();
  current.loc_details_.end_ = next_;
  location_stack_.pop();
}

void GD::CPP::FileStore::map_next_logical_location(std::string const& filename, Line line)
{
  location_stack_.top().logical_file_ = find_filename_id(filename);
  location_stack_.top().logical_line_ = line;
}

auto GD::CPP::FileStore::cmd_line_location() noexcept -> Location { return Location{0}; }

auto GD::CPP::FileStore::line(Location loc) const -> std::string const&
{
  static std::string empty{};
  auto const& loc_details = find_loc_details(loc);
  auto const& file = physical_files_.at(loc_details.physical_file_);
  auto line = loc_details.find_line(loc);
  return line == illegal_line ? empty : file.second.at(static_cast<std::size_t>(line));
}

auto GD::CPP::FileStore::line(Range range) const -> std::string const&
{
  return line(range.begin());
}

auto GD::CPP::FileStore::caret(Range range) const -> std::string
{
  auto loc = range.begin();
  auto const& loc_details = find_loc_details(loc);
  auto const& file = physical_files_.at(loc_details.physical_file_);
  auto const* line_details = loc_details.find_line_details(loc);
  if (line_details == nullptr) {
    return empty_;
  }
  auto line = loc_details.find_line(loc);
  assert(line != illegal_line);  // NOLINT
  auto const& str = file.second.at(static_cast<std::size_t>(line));
  auto column_begin =
    str.begin() + static_cast<std::ptrdiff_t>(static_cast<std::size_t>(loc) -
                                              static_cast<std::size_t>(line_details->begin_));
  auto column_end = column_begin + static_cast<std::ptrdiff_t>(range.size());
  std::string s =
    std::accumulate(str.begin(), column_begin, std::string{}, [](std::string const& s, char c) {
      if (std::isspace(c) == 0) {
        c = ' ';
      }
      return s + c;
    });
  s += '^';
  auto len =
    std::max(std::size_t{1}, static_cast<std::size_t>(count_chars(column_begin, column_end))) - 1;
  return s + std::string(len, '=');
}

auto GD::CPP::FileStore::range_begin(Range range) const -> std::string::const_iterator
{
  auto loc = range.begin();
  auto const& loc_details = find_loc_details(loc);
  auto const& file = physical_files_.at(loc_details.physical_file_);
  auto const* line_details = loc_details.find_line_details(loc);
  if (line_details == nullptr) {
    return empty_.end();
  }
  auto line = loc_details.find_line(loc);
  assert(line != illegal_line);  // NOLINT
  auto const& str = file.second.at(static_cast<std::size_t>(line));
  auto column = static_cast<std::size_t>(loc) - static_cast<std::size_t>(line_details->begin_);
  return str.begin() + static_cast<std::string::difference_type>(column);
}

auto GD::CPP::FileStore::range_end(Range range) const -> std::string::const_iterator
{
  auto loc = range.end();
  auto const& loc_details = find_loc_details(loc);
  auto const& file = physical_files_.at(loc_details.physical_file_);
  auto const* line_details = loc_details.find_line_details(loc);
  if (line_details == nullptr) {
    return empty_.end();
  }
  auto line = loc_details.find_line(loc);
  assert(line != illegal_line);  // NOLINT
  auto const& str = file.second.at(static_cast<std::size_t>(line));
  auto column = static_cast<std::size_t>(loc) - static_cast<std::size_t>(line_details->begin_);
  return str.begin() + static_cast<std::string::difference_type>(column);
}

auto GD::CPP::FileStore::is_top_level(Location loc) const noexcept -> bool
{
  return std::addressof(find_loc_details(loc)) == std::addressof(cmd_line_location_);
}

auto GD::CPP::FileStore::parent_location(Location loc) const noexcept -> Location
{
  /* The parent location of a given location is 1 less than the beginning of the first location in
   * the file.  This works because we insist that you can't push multiple files at the same
   * location.
   */
  auto const& loc_details = find_loc_details(loc);
  Location begin = loc_details.begin_;
  return begin == cmd_line_location()
           ? begin
           : static_cast<Location>(static_cast<std::uint64_t>(begin) - 1);
}

auto GD::CPP::FileStore::logical_filename(Location loc) const noexcept -> std::string const&
{
  auto const* line_details = find_loc_details(loc).find_line_details(loc);
  return line_details == nullptr ? empty_ : file_names_.at(line_details->logical_file_);
}

auto GD::CPP::FileStore::logical_line(Location loc) const noexcept -> Line
{
  auto const* line_details = find_loc_details(loc).find_line_details(loc);
  return line_details == nullptr ? Line{0} : line_details->logical_line_;
}

auto GD::CPP::FileStore::logical_column(Location loc) const noexcept -> Column
{
  auto const& loc_details = find_loc_details(loc);
  auto const& file = physical_files_.at(loc_details.physical_file_);
  auto const* line_details = loc_details.find_line_details(loc);
  if (line_details == nullptr) {
    return Column{0};
  }
  auto line = loc_details.find_line(loc);
  assert(line != illegal_line);  // NOLINT
  auto const& str = file.second.at(static_cast<std::size_t>(line));
  auto column = static_cast<std::size_t>(loc) - static_cast<std::size_t>(line_details->begin_);
  auto physical_column =
    count_chars(str.begin(), str.begin() + static_cast<std::string::difference_type>(column));

  return static_cast<Column>(1 + static_cast<std::size_t>(line_details->logical_column_) +
                             static_cast<std::size_t>(physical_column));
}

auto GD::CPP::FileStore::physical_filename(Location loc) const noexcept -> std::string const&
{
  auto const& loc_details = find_loc_details(loc);
  return file_names_.at(loc_details.physical_file_);
}

auto GD::CPP::FileStore::physical_line(Location loc) const noexcept -> Line
{
  auto line = find_loc_details(loc).find_line(loc);
  return Line{static_cast<std::size_t>(line) + 1};
}

auto GD::CPP::FileStore::physical_column(Location loc) const noexcept -> Column
{
  auto const& loc_details = find_loc_details(loc);
  auto const& file = physical_files_.at(loc_details.physical_file_);
  auto const* line_details = loc_details.find_line_details(loc);
  if (line_details == nullptr) {
    return Column{0};
  }
  auto line = loc_details.find_line(loc);
  assert(line != illegal_line);  // NOLINT
  auto const& str = file.second.at(static_cast<std::size_t>(line));
  auto column = static_cast<std::size_t>(loc) - static_cast<std::size_t>(line_details->begin_);
  auto physical_column =
    count_chars(str.begin(), str.begin() + static_cast<std::string::difference_type>(column));
  return Column{1 + static_cast<std::size_t>(physical_column)};
}

auto GD::CPP::FileStore::find_loc_details(Location loc) const -> LocationDetails const&
{
  LocationDetails const* loc_details = &cmd_line_location_;
  while (!loc_details->children_.empty()) {
    /* Find first child where child.begin_ > loc_.  The location *may* be in the child *before* the
     * found one. */
    auto it =
      std::upper_bound(loc_details->children_.begin(), loc_details->children_.end(), loc,
                       [](Location loc, auto const& details) { return loc < details->begin_; });

    /* If we found at the beginning then the current loc_details are the ones we want.  */
    if (it == loc_details->children_.begin()) {
      break;
    }

    /* Now check to see if the location we are searching for is actually in the child.  */
    --it;
    if ((*it)->end_ < loc) {
      /* No - so break.  */
      break;
    }

    /* Recurse into the child. */
    loc_details = *it;
  }

  return *loc_details;
}

void GD::CPP::FileStore::peek_next_line()
{
  auto& current = location_stack_.top();

  if (std::addressof(current.loc_details_) == &cmd_line_location_) {
    token_.emplace(TokenType::end_of_source, Range{next_, 0});
    return;
  }

  auto& physical_file = physical_files_.at(current.loc_details_.physical_file_);
  auto& current_line = current.physical_line_;
  std::istream& is = physical_file.first;

  if (current_line == physical_file.second.size()) {
    /* Get the next line from the stream.  */
    if (is.eof()) {
      token_.emplace(TokenType::end_of_include, Range{next_, 0});
      pop_file();
      return;
    }

    std::string s;

    try {
      std::getline(is, s);
    }
    catch (std::exception const& e) {
      if (!is.eof()) {
        token_.emplace(TokenType::error, Range{next_, 0},
                       error_manager_.error(ErrorCode::file_error, e.what()));
      }
    }

    if (!is.eof()) {
      s.push_back('\n');
    }
    else if (s.empty()) {
      token_.emplace(TokenType::end_of_include, Range{next_, 0});
      return;
    }

    physical_file.second.push_back(s);
  }

  auto const& str = physical_file.second.at(static_cast<std::size_t>(current_line));
  LineDetails line = {next_, current.logical_file_, current.logical_line_, Column{0}};

  using UT = std::underlying_type_t<Line>;
  current.physical_line_ = static_cast<Line>(static_cast<UT>(current_line) + 1);
  current.logical_line_ = static_cast<Line>(static_cast<UT>(current.logical_line_) + 1);
  current.loc_details_.lines_.push_back(line);

  next_begin_ = str.begin();
  line_end_ = str.end();
}

auto GD::CPP::FileStore::find_filename_id(std::string const& filename) -> std::size_t
{
  auto it = std::find(file_names_.begin(), file_names_.end(), filename);
  std::size_t index = it - file_names_.end();
  if (index == file_names_.size()) {
    file_names_.emplace_back(filename);
  }

  return index;
}

GD::CPP::FileStore::LocationDetails::~LocationDetails()
{
  std::for_each(children_.begin(), children_.end(), [](LocationDetails* loc) { delete loc; });
}

auto GD::CPP::FileStore::LocationDetails::find_line(Location loc) const -> Line
{
  auto it = std::upper_bound(lines_.begin(), lines_.end(), loc,
                             [](Location loc, LineDetails const& rhs) { return loc < rhs.begin_; });
  if (it == lines_.begin()) {
    return illegal_line;
  }
  return static_cast<Line>(it - lines_.begin() - 1);
}

auto GD::CPP::FileStore::LocationDetails::find_line_details(Location loc) const
  -> LineDetails const*
{
  auto line = find_line(loc);
  // NOLINTNEXTLINE
  return line == illegal_line ? nullptr : lines_.data() + static_cast<std::size_t>(line);
}

GD::CPP::FileStore::LocationStack::LocationStack(LocationDetails& loc_details)
    : loc_details_(loc_details), physical_line_(Line{0}), logical_file_(loc_details.physical_file_),
      logical_line_(Line{0})
{
}
