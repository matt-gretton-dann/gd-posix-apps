/** \file   libcpp/include/identifier-manager.hh
 *  \brief  Identifier management
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_PPNUMBER_MANAGER_HH_INCLUDED_
#define CC_LIBCPP_PPNUMBER_MANAGER_HH_INCLUDED_

#include "gd/format.hh"

#include "cc-messages.hh"

#include <cstdint>
#include <iostream>
#include <string>
#include <utility>

#include "id-map.hh"

namespace GD::CPP {

/** \brief  ID for an identifier.  */
enum class PPNumberID : std::uint32_t {};

/** \brief  Manage pre-processor numbers.
 *
 * This class stores pre-processor numbers and manages the two way mapping PPNumber <-> PPNumberID.
 *
 * We use IDs rather than pointers so that we can limit the total number of PPNumbers to 2^32, and
 * so keep the size of Token under control.
 *
 * We do not store multiple copies of the same PPNumber.
 */
class PPNumberManager
{
public:
  /** \brief  Constructor.  */
  PPNumberManager();

  PPNumberManager(PPNumberManager const&) = delete;
  PPNumberManager(PPNumberManager&&) noexcept = delete;
  auto operator=(PPNumberManager const&) -> PPNumberManager& = delete;
  auto operator=(PPNumberManager&&) noexcept -> PPNumberManager& = delete;
  ~PPNumberManager() = default;

  /** \brief     Get the ID for and identifier.
   *  \param  id Identifier string.
   *  \return    ID.
   *
   * If \a id has already been seen the same ID as previously will be returned, otherwise there will
   * be a new ID.
   */
  [[nodiscard]] auto id(std::u32string const& id) -> PPNumberID;

  /** \brief     Get the display name of an ID.
   *  \param  id Identifier ID to get the display name of
   *  \return    Display name.
   *
   * A display name uses the basic character set and \u and \U sequences.
   */
  [[nodiscard]] auto display_name(PPNumberID id) const -> std::string;

private:
  IdMap<PPNumberID> map_;  ///< PPNumber map
};

}  // namespace GD::CPP

#endif  // CC_LIBCPP_PPNUMBER_MANAGER_HH_INCLUDED_
