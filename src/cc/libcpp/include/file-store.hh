#ifndef CC_LIBCPP_FILE_STORE_H_INCLUDED_
#define CC_LIBCPP_FILE_STORE_H_INCLUDED_

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

class FileStore;

class FileTokenizer
{
public:
  FileTokenizer(FileStore& fs);
  auto peek() -> Token const&;

  auto chew();
  auto chew(TokenType type_);
  template<typename It>
  auto chew(It type_begin_, It type_end_);

private:
  void do_peek();
  void peek_character();
  void peek_special_conditions();

  FileStore& fs_;
  Location location_;
  char const* line_;
  std::optional<Token> token_;
};

class FileStore
{
public:
  FileStore(ErrorManager& error_manager);
  FileStore() = delete;
  FileStore(FileStore const&) = delete;
  FileStore(FileStore&&) noexcept = delete;
  auto operator=(FileStore const&) -> FileStore& = delete;
  auto operator=(FileStore&&) noexcept -> FileStore& = delete;
  ~FileStore() = default;

  auto push_stream(std::string const& name, std::istream& is) -> FileTokenizer;
  auto push_file(std::string const& fname) -> FileTokenizer;
  auto push_standard_input() -> FileTokenizer;
  void pop_file();

  void map_next_logical_location(std::string const& filename, Line line);

  auto cmd_line_location() const noexcept -> Location;

  auto line(Location loc) const -> std::string const&;
  auto line(Range range) const -> std::string const&;
  auto caret(Range range) const -> std::string;

  auto range_begin(Range range) const -> std::string::const_iterator;
  auto range_end(Range range) const -> std::string::const_iterator;

  auto is_top_level(Location loc) const noexcept -> bool;
  auto parent_location(Location loc) const noexcept -> Location;

  auto logical_filename(Location loc) const noexcept -> std::string const&;
  auto logical_line(Location loc) const noexcept -> Line;
  auto logical_column(Location loc) const noexcept -> Column;

  auto physical_filename(Location loc) const noexcept -> std::string const&;
  auto physical_line(Location loc) const noexcept -> Line;
  auto physical_column(Location loc) const noexcept -> Column;

private:
  struct LineDetails
  {
    Location begin_;
    std::size_t logical_file_;
    std::size_t logical_line_;
    std::size_t logical_column_;
  };

  struct LocationDetails
  {
    LocationDetails() = default;
    ~LocationDetails() = default;

    Location begin_;
    Location end_;
    std::size_t physical_file_;
    std::vector<LineDetails> lines_;
    std::vector<LocationDetails*> children_;
  };

  friend class FileTokenizer;

  using FileLines = std::pair<std::istream&, std::vector<std::string>>;
  using Files = std::map<std::size_t, FileLines>;
  using StackStatus = std::pair<std::size_t, LocationDetails*>;

  auto find_loc_details(Location) const -> LocationDetails const*;

  auto eof() const -> bool;
  auto error() const -> std::optional<std::pair<Location, Error>>;
  auto next_line() -> std::pair<Location, char const*>;

  ErrorManager& error_manager_;
  Files physical_files_;
  Location next_;
  std::vector<std::string> file_names_;
  LocationDetails cmd_line_location_;
  std::stack<StackStatus> location_stack_;
  std::vector<std::fstream> streams_;
  std::string error_message_;
};

}  // namespace GD::CPP

#endif  // CC_LIBCPP_FILETOKENIZER_H_INCLUDED_
