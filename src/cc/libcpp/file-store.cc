
#include "file-store.hh"

#include <cassert>
#include <cstdint>
#include <limits>
#include <map>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

GD::CPP::FileStore::FileStore()
{
  file_names_.emplace_back("(command line)");
  next_ = Location{1};

  cmd_line_location_.begin_ = Location(0);
  cmd_line_location_.end_ = Location(std::numeric_limits<std::underlying_type_t<Location>>::max());
  cmd_line_location_.physical_file_ = 0;

  location_stack_.push(&cmd_line_location_);
}

auto GD::CPP::FileStore::push_file(std::string const& fname) -> FileTokenizer
{
  auto it = std::find(file_names_.begin(), file_names_.end(), fname);
  if (it == file_names_.end()) {
    it = file_names_.insert(file_names_.end(), fname);
  }
  auto index = it - file_names_.begin();
  auto& current = location_stack_.top();

  auto* new_file = new LocationDetails();
  new_file->begin_ = next_;
  new_file->end_ = Location(std::numeric_limits<std::underlying_type_t<Location>>::max());
  new_file->physical_file_ = index;

  current->children_.push_back(new_file);
  location_stack_.push(new_file);

  return FileTokenizer{*this};
}

auto GD::CPP::FileStore::push_standard_input() -> FileTokenizer { abort(); }

void GD::CPP::FileStore::pop_file()
{
  auto& current = location_stack_.top();
  current->end_ = next_;
  location_stack_.pop();
}

void GD::CPP::FileStore::map_next_logical_location(std::string const& filename, Line line)
{
  abort();
}

auto GD::CPP::FileStore::cmd_line_location() const noexcept -> Location { return Location{0}; }

auto GD::CPP::FileStore::line(Location loc) const -> std::string const&
{
  static std::string empty{""};
  auto const* loc_details = find_loc_details(loc);
  auto const& file = physical_files_.at(loc_details->physical_file_);
  auto it = std::upper_bound(loc_details->lines_.begin(), loc_details->lines_.end(), loc);

  if (it == loc_details->lines_.begin()) {
    return empty;
  }

  if (it == loc_details->lines_.end()) {
    return file.second.back();
  }

  auto dist = it - loc_details->lines_.begin() - 1;
  return file.second.at(dist);
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

auto GD::CPP::FileStore::parent_location(Location loc) const noexcept -> Location { abort(); }

auto GD::CPP::FileStore::logical_filename(Location loc) const noexcept -> std::string const&
{
  abort();
}
auto GD::CPP::FileStore::logical_line(Location loc) const noexcept -> Line { abort(); }
auto GD::CPP::FileStore::logical_column(Location loc) const noexcept -> Column { abort(); }

auto GD::CPP::FileStore::physical_filename(Location loc) const noexcept -> std::string const&
{
  abort();
}

auto GD::CPP::FileStore::physical_line(Location loc) const noexcept -> Line { abort(); }
auto GD::CPP::FileStore::physical_column(Location loc) const noexcept -> Column { abort(); }

auto GD::CPP::FileStore::find_loc_details(Location) const -> LocationDetails const* { abort(); }
