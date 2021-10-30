/** \file   libcpp/include/string-literal-manager.hh
 *  \brief  Identifier management
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_STRING_LITERAL_MANAGER_HH_INCLUDED_
#define CC_LIBCPP_STRING_LITERAL_MANAGER_HH_INCLUDED_

#include "gd/format.hh"

#include "cc-messages.hh"

#include <cstdint>
#include <iostream>
#include <string>
#include <utility>

#include "id-map.hh"

namespace GD::CPP {

/** \brief  ID for a narrow string literal.  */
enum class StringLiteralID : std::uint32_t {};

/** \brief  ID for a wider string literal.  */
enum class WideStringLiteralID : std::uint32_t {};

/** \brief  Manage narrow string literals
 *
 * This class stores identifiers and manages the two way mapping string literal <-> IdentID.
 *
 * We use IDs rather than pointers so that we can limit the total number of identifiers to 2^32, and
 * so keep the size of Token under control.
 *
 * We do not store multiple copies of the same identifier.
 */
class StringLiteralManager
{
public:
  /** \brief  Constructor.  */
  StringLiteralManager();

  StringLiteralManager(StringLiteralManager const&) = delete;
  StringLiteralManager(StringLiteralManager&&) noexcept = delete;
  auto operator=(StringLiteralManager const&) -> StringLiteralManager& = delete;
  auto operator=(StringLiteralManager&&) noexcept -> StringLiteralManager& = delete;
  ~StringLiteralManager() = default;

  /** \brief     Get the ID for and identifier.
   *  \param  id Identifier string.
   *  \return    ID.
   *
   * If \a id has already been seen the same ID as previously will be returned, otherwise there will
   * be a new ID.
   */
  [[nodiscard]] auto id(std::string const& id) -> StringLiteralID;

  /** \brief     Get the display name of an ID.
   *  \param  id Identifier ID to get the display name of
   *  \return    Display name.
   *
   * A display name uses the basic character set and \u and \U sequences.
   */
  [[nodiscard]] auto display_name(StringLiteralID id) const -> std::string;

private:
  IdMap<StringLiteralID, std::string> map_;  ///< String literal map
};

/** \brief  Manage wider (L, u, U, u8) string literals
 *
 * This class stores identifiers and manages the two way mapping string literal <-> IdentID.
 *
 * We use IDs rather than pointers so that we can limit the total number of identifiers to 2^32, and
 * so keep the size of Token under control.
 *
 * We do not store multiple copies of the same identifier.
 */
class WideStringLiteralManager
{
public:
  /** \brief  Constructor.  */
  WideStringLiteralManager();

  WideStringLiteralManager(WideStringLiteralManager const&) = delete;
  WideStringLiteralManager(WideStringLiteralManager&&) noexcept = delete;
  auto operator=(WideStringLiteralManager const&) -> WideStringLiteralManager& = delete;
  auto operator=(WideStringLiteralManager&&) noexcept -> WideStringLiteralManager& = delete;
  ~WideStringLiteralManager() = default;

  /** \brief     Get the ID for and identifier.
   *  \param  id Identifier string.
   *  \return    ID.
   *
   * If \a id has already been seen the same ID as previously will be returned, otherwise there will
   * be a new ID.
   */
  [[nodiscard]] auto id(std::u32string const& id) -> WideStringLiteralID;

  /** \brief     Get the display name of an ID.
   *  \param  id Identifier ID to get the display name of
   *  \return    Display name.
   *
   * A display name uses the basic character set and \u and \U sequences.
   */
  [[nodiscard]] auto display_name(WideStringLiteralID id) const -> std::string;

private:
  IdMap<WideStringLiteralID, std::u32string> map_;  ///< String literal map
};

}  // namespace GD::CPP

#endif  // CC_LIBCPP_STRING_LITERAL_MANAGER_HH_INCLUDED_
