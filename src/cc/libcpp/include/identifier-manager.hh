/** \file   libcpp/include/identifier-manager.hh
 *  \brief  Identifier management
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_IDENTIFIER_MANAGER_HH_INCLUDED_
#define CC_LIBCPP_IDENTIFIER_MANAGER_HH_INCLUDED_

#include "gd/format.hh"

#include "cc-messages.hh"

#include <cstdint>
#include <iostream>
#include <string>
#include <utility>

#include "id-map.hh"

namespace GD::CPP {

/** \brief  ID for an identifier.  */
enum class IdentID : std::uint32_t {};

/** \brief  Manage identifiers
 *
 * This class stores identifiers and manages the two way mapping Identifier <-> IdentID.
 *
 * We use IDs rather than pointers so that we can limit the total number of identifiers to 2^32, and
 * so keep the size of Token under control.
 *
 * We do not store multiple copies of the same identifier.
 */
class IdentifierManager
{
public:
  /** \brief  Constructor.  */
  IdentifierManager();

  IdentifierManager(IdentifierManager const&) = delete;
  IdentifierManager(IdentifierManager&&) noexcept = delete;
  auto operator=(IdentifierManager const&) -> IdentifierManager& = delete;
  auto operator=(IdentifierManager&&) noexcept -> IdentifierManager& = delete;
  ~IdentifierManager() = default;

  /** \brief     Get the ID for and identifier.
   *  \param  id Identifier string.
   *  \return    ID.
   *
   * If \a id has already been seen the same ID as previously will be returned, otherwise there will
   * be a new ID.
   */
  [[nodiscard]] auto id(std::u32string const& id) -> IdentID;

  /** \brief     Get the display name of an ID.
   *  \param  id Identifier ID to get the display name of
   *  \return    Display name.
   *
   * A display name uses the basic character set and \u and \U sequences.
   */
  [[nodiscard]] auto display_name(IdentID id) const -> std::string;

private:
  IdMap<IdentID> map_;  ///< Identifier map
};

}  // namespace GD::CPP

#endif  // CC_LIBCPP_IDENTIFIER_MANAGER_HH_INCLUDED_
