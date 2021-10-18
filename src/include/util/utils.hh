/** \file   include/util/utils.hh
 *  \brief  General utilities
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef UTIL_UTILS_HH
#define UTIL_UTILS_HH

#include "gd/nl_types.h"

#include <climits>
#include <string_view>
#include <utility>

namespace GD {
/** \brief       Set the program name.
 *  \param argv0 argv[0].
 */
void program_name(std::string_view argv0);

/** \brief  Get the program name.
 *  \return Program name.
 */
auto program_name() -> std::string_view;

/** \brief     Class to provide 'overloaded' lambdas.
 *  \tparam Ts Lambdas to combine
 *
 * This is often used in std::visit calls, for example:
 *
 * \code
 * std::visit(Overloaded{
 *                       [](Type t) { return t; },
 *                       [](GD::Util::Number const&) { return Type::number; },
 *                       [](std::string const&) { return Type::string; },
 *                       [](char) { return Type::letter; },
 *                      }, value_);
 * \endcode
 */
template<class... Ts>
struct Overloaded : Ts...
{
  using Ts::operator()...;
};

// Template guide.
template<class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

/** \brief Helper to wrap types when we need to differentiate between the same underlying type.
 */
template<typename T, typename TId = T>
class TypeWrapper
{
public:
  explicit TypeWrapper(T t) : t_(std::move(t)) {}
  explicit TypeWrapper(T&& t) : t_(std::move(t)) {}

  template<typename Arg>
  explicit TypeWrapper(Arg arg) : t_(T(arg))
  {
  }

  auto get() -> T& { return t_; }
  [[nodiscard]] auto get() const -> T const& { return t_; }

private:
  T t_;
};

/** \brief      Write a value out in big endian form a byte at a time.
 *  \tparam T   Integer type to write
 *  \tparam It  Output iterator to write to.
 *  \param  it  Output iterator to write to
 *  \param  v   Value to write
 *
 * sizeof(T) writes are made to *it.  *it must accept std::byte values.
 */
template<typename T, typename It>
void write_be(It it, T v)
{
  T shift = sizeof(T) * CHAR_BIT;
  while (shift != 0) {
    shift -= CHAR_BIT;
    *it++ = static_cast<std::byte>(v >> shift);
  }
}

/** \brief      Write a value out in little endian form a byte at a time.
 *  \tparam T   Integer type to write
 *  \tparam It  Output iterator to write to.
 *  \param  it  Output iterator to write to
 *  \param  v   Value to write
 *
 * sizeof(T) writes are made to *it.  *it must accept std::byte values.
 */
template<typename T, typename It>
void write_le(It it, T v)
{
  T shift = 0;
  while (shift != sizeof(T) * CHAR_BIT) {
    *it++ = static_cast<std::byte>(v >> shift);
    shift += CHAR_BIT;
  }
}

/** \brief     Read a value from a std::byte input iterator, treating it as big endian.
 *  \tparam T  Integer type to read
 *  \tparam It Input iterator to read from.
 *  \param  it Input iterator to read
 *  \return    Read value.
 *
 * \a Reading from *it must return values that can be converted to std::byte.
 * \c *it will be read from sizeof(T) times.
 */
template<typename T, typename It>
auto read_be(It it) -> T
{
  T result = 0;
  T shift = sizeof(T) * CHAR_BIT;
  while (shift != 0) {
    shift -= CHAR_BIT;
    result |= static_cast<T>(*it++) << shift;
  }
  return result;
}

/** \brief     Read a value from a std::byte input iterator, treating it as little endian.
 *  \tparam T  Integer type to read
 *  \tparam It Input iterator to read from.
 *  \param  it Input iterator to read
 *  \return    Read value.
 *
 * \a Reading from *it must return values that can be converted to std::byte.
 * \c *it will be read from sizeof(T) times.
 */
template<typename T, typename It>
auto read_le(It it) -> T
{
  T result = 0;
  T shift = 0;
  while (shift != sizeof(T) * CHAR_BIT) {
    result |= static_cast<T>(*it++) << shift;
    shift += CHAR_BIT;
  }
  return result;
}

}  // namespace GD
#endif  // UTIL_UTILS_HH
