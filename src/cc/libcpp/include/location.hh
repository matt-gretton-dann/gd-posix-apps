/** \file   libcpp/includelocation.hh
 *  \brief  Location handling classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_LOCATION_HH_INCLUDED_
#define CC_LIBCPP_LOCATION_HH_INCLUDED_

#include <cstdint>
#include <type_traits>

namespace GD::CPP {

/** \brief  Represents a line number.  */
enum class Line : std::size_t {};

/** \brief  Represents a column number.  */
enum class Column : std::size_t {};

/** \brief Represents a location
 *
 * A location represents:
 *  * A physical (file, line, column) triple,
 *  * A logical (file, line, column) triple.
 *  * Stack of inclusions.
 *
 * The physical triple refers to the file/stream/pipe that data was read from.  The logical triple
 * refers to the value as given by #line directives.
 *
 * Location values are managed by GD::CPP::FileStore objects.
 */
enum class Location : std::uint64_t {};

constexpr inline auto operator==(Line lhs, std::size_t rhs) noexcept -> bool
{
  return static_cast<std::size_t>(lhs) == rhs;
}

constexpr inline auto operator==(std::size_t rhs, Line line) noexcept -> bool
{
  return static_cast<std::size_t>(line) == rhs;
}

/** \brief  Add a column number to a location.  */
constexpr inline auto operator+(Location loc, Column col) noexcept -> Location
{
  using UT = std::underlying_type_t<Location>;
  return static_cast<Location>(static_cast<UT>(loc) + static_cast<UT>(col));
}

/** \brief  Add a column number to a location.  */
constexpr auto operator+(Column col, Location loc) noexcept -> Location
{
  using UT = std::underlying_type_t<Location>;
  return static_cast<Location>(static_cast<UT>(loc) + static_cast<UT>(col));
}

/** \brief  A range
 *
 * Consists of a [begin, end) location.  We assume ranges are always on the same line.
 */
class Range
{
public:
  /** \brief     Create a range of 1 character.
   *  \param loc Location
   */
  explicit constexpr Range(Location loc) noexcept : begin_(loc), len_(1) {}

  /** \brief       Create a range.
   *  \param begin First location in range
   *  \param len   Length of range.
   */
  constexpr Range(Location begin, std::size_t len) noexcept : begin_(begin), len_(len) {}

  /** \brief  Create a range
   *  \param begin  First location in range
   *  \param end    Location one past end of range.
   */
  constexpr Range(Location begin, Location end) noexcept
      : begin_(begin), len_(static_cast<std::size_t>(end) - static_cast<std::size_t>(begin))
  {
  }

  /** \brief  Get the beginning of a range.
   *  \return Range beginning
   */
  constexpr auto begin() const noexcept -> Location { return begin_; }

  /** \brief  Get one past the end of the range.
   *  \return Range end
   */
  constexpr auto end() const noexcept -> Location { return begin_ + static_cast<Column>(len_); }

  /** \brief  Get the length/size a range.
   *  \return Size of a range
   */
  constexpr auto size() const noexcept -> std::size_t { return len_; }

private:
  Location begin_;   ///< Beginning location
  std::size_t len_;  ///< Range length
};

}  // namespace GD::CPP

#endif  // CC_LIBCPP_LOCATION_HH_INCLUDED_
