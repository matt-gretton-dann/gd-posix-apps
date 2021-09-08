/** \file   include/util/utils.hh
 *  \brief  General utilities
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef UTIL_UTILS_HH
#define UTIL_UTILS_HH

#include "gd/nl_types.h"

#include <utility>

#include <string_view>

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

}  // namespace GD
#endif  // UTIL_UTILS_HH
