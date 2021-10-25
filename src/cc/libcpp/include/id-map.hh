/** \file   libcpp/include/id-map.hh
 *  \brief  Class that provides ID <-> u32string mapping
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_ID_MAP_HH_INCLUDED_
#define CC_LIBCPP_ID_MAP_HH_INCLUDED_

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "error.hh"

namespace GD::CPP {

/** \brief  Bidirectional map from an ID to a u32string_view
 *  \tparam Integer type to use as ID.
 *
 * This is used to hold maps from identifiers/pp-numbers/constants etc. which we want to refer to
 * by index generally rather than by value.
 */
template<typename Id>
class IdMap
{
public:
  /** \brief  Constructor.  */
  IdMap() = default;

  /** \brief Destructor.  */
  ~IdMap()
  {
    /* Delete the data in the vector.  Don't do this for the map as it points to the same data.  */
    for (auto id : ids_) {
      delete[] id.data();
    }
  }

  IdMap(IdMap const&) = delete;
  IdMap(IdMap&&) noexcept = default;
  auto operator=(IdMap const&) -> IdMap& = delete;
  auto operator=(IdMap&&) noexcept -> IdMap& = default;

  /** \brief      Get the ID for the given \a str value.
   *  \param  str Value to look up.
   *  \return     ID.
   *
   * ID will be the same for all identical values of str (character by character checking).
   *
   * This takes a copy of \a str if needed.
   */
  [[nodiscard]] auto find_or_insert(std::u32string_view str) -> Id
  {
    auto it = map_.find(str);
    if (it != map_.end()) {
      return it->second;
    }

    assert_ice(ids_.size() < std::numeric_limits<std::underlying_type_t<Id>>::max(),
               "ID Map is full.");
    auto idx = static_cast<Id>(ids_.size());
    auto* data = new (std::nothrow) char32_t[str.size()];
    assert_ice(data != nullptr, "Out of memory allocating IDs.");

    std::copy(str.begin(), str.end(), data);
    std::u32string_view sv(data, str.size());
    ids_.push_back(sv);
    auto ins = map_.insert(std::make_pair(sv, idx));
    assert_ice(ins.second, "Failed to insert into ID map.");
    return idx;
  }

  /** \brief      Get the ID for the given \a str value.
   *  \param  str Value to look up.
   *  \return     ID.
   *
   * ID will be the same for all identical values of str (character by character checking).
   *
   * This takes a copy of \a str if needed.
   */
  [[nodiscard]] auto find_or_insert(std::u32string const& str) -> Id
  {
    return find_or_insert(std::u32string_view{str.data(), str.size()});
  }

  /** \brief     Get the value with ID \a id.
   *  \param  id ID to look up
   *  \return    String value
   *
   * Asserts if \a id is not valid.
   */
  [[nodiscard]] auto get(Id id) const noexcept -> std::u32string_view
  {
    assert_ice(static_cast<std::size_t>(id) < ids_.size(), "ID out of range.");
    return ids_[static_cast<std::size_t>(id)];
  }

private:
  std::unordered_map<std::u32string_view, Id> map_;  ///< Map String to ID.
  std::vector<std::u32string_view> ids_;             ///< Vector of strings - ID is index in vector.
};
}  // namespace GD::CPP

#endif  // CC_LIBCPP_ID_MAP_HH_INCLUDED_
