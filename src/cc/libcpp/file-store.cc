
#include "file-store.hh"

#include <cassert>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

GD::CPP::FileStore::FileStore(ErrorManager& error_manager) : error_manager_(error_manager)
{
  file_names_.emplace_back("(command line)");
  next_ = Location{1};

  cmd_line_location_.begin_ = Location(0);
  cmd_line_location_.end_ = Location(std::numeric_limits<std::underlying_type_t<Location>>::max());
  cmd_line_location_.physical_file_ = 0;

  location_stack_.push(std::make_pair(Line{0}, &cmd_line_location_));
}

auto GD::CPP::FileStore::push_stream(std::string const& name, std::istream& is) -> FileTokenizer
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
  if (current.second->begin_ == next_) {
    error_message_ = "Can't push twice at the same location.";
  }
  else {
    auto* new_file = new LocationDetails();
    new_file->begin_ = next_;
    new_file->end_ = Location(std::numeric_limits<std::underlying_type_t<Location>>::max());
    new_file->physical_file_ = index;

    current.second->children_.push_back(new_file);
    location_stack_.push(std::make_pair(Line{0}, new_file));
  }

  return FileTokenizer{*this};
}

auto GD::CPP::FileStore::push_file(std::string const& fname) -> FileTokenizer
{
  // Look the file name up to see if we've already opened it - and if so
  auto it = std::find(file_names_.begin(), file_names_.end(), fname);
  std::size_t index = it - file_names_.end();
  if (index != file_names_.size()) {
    auto it2 = physical_files_.find(index);
    if (it2 != physical_files_.end()) {
      return push_stream(fname, it2->second.first);
    }
  }

  // Create the stream and put it somewhere that will live long enough.
  streams_.emplace_back();
  streams_.back().exceptions(std::ifstream::failbit | std::ifstream::badbit);
  try {
    streams_.back().open(fname);
  }
  catch (std::exception const& e) {
    error_message_ = e.what();
  }
  return push_stream(fname, streams_.back());
}

auto GD::CPP::FileStore::push_standard_input() -> FileTokenizer
{
  return push_stream("(standard input)", std::cin);
}

void GD::CPP::FileStore::pop_file()
{
  auto& current = location_stack_.top();
  current.second->end_ = next_;
  location_stack_.pop();
}

void GD::CPP::FileStore::map_next_logical_location(std::string const& filename, Line line)
{
  location_stack_.top().second->logical_file_ = find_filename_id(filename);
  location_stack_.top().second->logical_line_ = line;
}

auto GD::CPP::FileStore::cmd_line_location() noexcept -> Location { return Location{0}; }

auto GD::CPP::FileStore::line(Location loc) const -> std::string const&
{
  static std::string empty{};
  auto const* loc_details = find_loc_details(loc);
  auto const& file = physical_files_.at(loc_details->physical_file_);
  auto line = find_line(loc_details, loc);
  return line == illegal_line ? empty : file.second.at(static_cast<std::size_t>(line));
}

auto GD::CPP::FileStore::line(Range range) const -> std::string const&
{
  return line(range.begin());
}

auto GD::CPP::FileStore::caret(Range range) const -> std::string { abort(); }

auto GD::CPP::FileStore::range_begin(Range range) const -> std::string::const_iterator { abort(); }
auto GD::CPP::FileStore::range_end(Range range) const -> std::string::const_iterator { abort(); }

auto GD::CPP::FileStore::is_top_level(Location loc) const noexcept -> bool
{
  return find_loc_details(loc) == &cmd_line_location_;
}

auto GD::CPP::FileStore::parent_location(Location loc) const noexcept -> Location
{
  /* The parent location of a given location is 1 less than the beginning of the first location in
   * the file.  This works because we insist that you can't push multiple files at the same
   * location.
   */
  auto const* loc_details = find_loc_details(loc);
  Location begin = loc_details->begin_;
  return begin == cmd_line_location()
           ? begin
           : static_cast<Location>(static_cast<std::uint64_t>(begin) - 1);
}

auto GD::CPP::FileStore::logical_filename(Location loc) const noexcept -> std::string const&
{
  static std::string empty{};
  auto const* loc_details = find_loc_details(loc);
  auto const* line_details = find_line_details(loc_details, loc);
  return line_details == nullptr ? empty : file_names_.at(line_details->logical_file_);
}

auto GD::CPP::FileStore::logical_line(Location loc) const noexcept -> Line
{
  auto const* loc_details = find_loc_details(loc);
  auto const* line_details = find_line_details(loc_details, loc);
  return line_details == nullptr ? Line{0} : line_details->logical_line_;
}

auto GD::CPP::FileStore::logical_column(Location loc) const noexcept -> Column
{
  auto const* loc_details = find_loc_details(loc);
  auto const* line_details = find_line_details(loc_details, loc);
  if (line_details == nullptr) {
    return Column{0};
  }

  auto physical_column =
    static_cast<std::size_t>(loc) - static_cast<std::size_t>(line_details->begin_);

  return static_cast<Column>(1 + static_cast<std::size_t>(line_details->logical_column_) +
                             physical_column);
}

auto GD::CPP::FileStore::physical_filename(Location loc) const noexcept -> std::string const&
{
  auto const* loc_details = find_loc_details(loc);
  return file_names_.at(loc_details->physical_file_);
}

auto GD::CPP::FileStore::physical_line(Location loc) const noexcept -> Line
{
  auto const* loc_details = find_loc_details(loc);
  auto line = find_line(loc_details, loc);
  return Line{static_cast<std::size_t>(line) + 1};
}

auto GD::CPP::FileStore::physical_column(Location loc) const noexcept -> Column
{
  auto const* loc_details = find_loc_details(loc);
  auto const* line_details = find_line_details(loc_details, loc);
  if (line_details == nullptr) {
    return Column{0};
  }

  auto physical_column =
    1 + static_cast<std::size_t>(loc) - static_cast<std::size_t>(line_details->begin_);
  return Column{physical_column};
}

auto GD::CPP::FileStore::find_loc_details(Location loc) const -> LocationDetails const*
{
  auto const* loc_details = &cmd_line_location_;
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

  return loc_details;
}

auto GD::CPP::FileStore::eof() const -> bool
{
  auto const& current = location_stack_.top();
  auto const& physical_file = physical_files_.at(current.second->physical_file_);
  auto current_line = current.first;

  /* Have we reached the end of the file we're reading?  */
  if (!physical_file.first.eof()) {
    return false;
  }
  return (current_line >= static_cast<Line>(physical_file.second.size()));
}

auto GD::CPP::FileStore::error() const -> std::optional<std::pair<Location, Error>>
{
  if (error_message_.empty()) {
    return std::nullopt;
  }
  return std::make_pair(next_, error_manager_.error(ErrorCode::file_error, error_message_));
}

auto GD::CPP::FileStore::next_line() -> std::pair<Location, char const*>
{
  auto& current = location_stack_.top();
  auto& physical_file = physical_files_.at(current.second->physical_file_);
  auto& current_line = current.first;
  std::istream& is = physical_file.first;

  if (current_line == physical_file.second.size()) {
    /* Get the next line from the stream.  */
    if (!error_message_.empty() || is.eof()) {
      return {next_, nullptr};
    }
    std::string s;

    try {
      std::getline(is, s);
    }
    catch (std::exception const& e) {
      error_message_ = e.what();
      return {next_, nullptr};
    }

    if (!is.eof()) {
      s.push_back('\n');
    }

    physical_file.second.push_back(s);
  }

  auto const& str = physical_file.second.at(static_cast<std::size_t>(current_line));
  auto const* line_start = str.data();
  Location loc = next_;
  LineDetails line = {loc, current.second->logical_file_, current.second->logical_line_, Column{0}};

  using UT = std::underlying_type_t<Line>;
  current_line = static_cast<Line>(static_cast<UT>(current_line) + 1);
  current.second->logical_line_ =
    static_cast<Line>(static_cast<UT>(current.second->logical_line_) + 1);
  next_ = next_ + Column{str.size()};
  current.second->lines_.push_back(line);

  return {loc, line_start};
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

auto GD::CPP::FileStore::find_line(LocationDetails const* location_details, Location loc) -> Line
{
  auto it = std::upper_bound(location_details->lines_.begin(), location_details->lines_.end(), loc,
                             [](Location loc, LineDetails const& rhs) { return loc < rhs.begin_; });
  if (it == location_details->lines_.begin()) {
    return illegal_line;
  }
  return static_cast<Line>(it - location_details->lines_.begin() - 1);
}

auto GD::CPP::FileStore::find_line_details(LocationDetails const* location_details, Location loc)
  -> LineDetails const*
{
  auto line = find_line(location_details, loc);
  return line == illegal_line ? nullptr  // NOLINTNEXTLINE
                              : location_details->lines_.data() + static_cast<std::size_t>(line);
}
