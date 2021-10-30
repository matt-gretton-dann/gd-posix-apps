/** \file   libcpp/include/id-map.hh
 *  \brief  Class that provides ID <-> u32string mapping
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_ID_MAP_HH_INCLUDED_
#define CC_LIBCPP_ID_MAP_HH_INCLUDED_

#include <cctype>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <vector>

#include "character-classifiers.hh"
#include "error.hh"

namespace GD::CPP {

/** \brief        Bidirectional map from an ID to a u32string_view
 *  \tparam Id    type to use as ID (must be integral)
 *  \tparam Value type to use for the value
 *
 * This is used to hold maps from identifiers/pp-numbers/constants etc. which we want to refer to
 * by index generally rather than by value.
 */
template<typename Id, typename Value>
class IdMap
{
  using UnderlyingT = std::underlying_type_t<Id>;
  static_assert(std::is_integral<UnderlyingT>::value, "Id must be an integral type.");
  static_assert(std::is_unsigned<UnderlyingT>::value, "Id must be an unsigned type.");
  static_assert(sizeof(UnderlyingT) <= sizeof(std::size_t), "Id must be no bigger than size_t");

public:
  /** \brief  Constructor.  */
  IdMap() = default;

  /** \brief Destructor.  */
  ~IdMap()
  {
    /* Delete the data in the vector.  Don't do this for the map as it points to the same data.  */
    for (auto* id : ids_) {
      delete id;
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
  [[nodiscard]] auto find_or_insert(Value const& value) -> Id
  {
    auto it = map_.find(&value);
    if (it != map_.end()) {
      return it->second;
    }

    assert_ice(ids_.size() < std::numeric_limits<std::underlying_type_t<Id>>::max(),
               "ID Map is full.");
    auto idx = static_cast<Id>(ids_.size());
    auto* data = new (std::nothrow) Value(value);
    assert_ice(data != nullptr, "Out of memory allocating IDs.");

    ids_.push_back(data);
    auto ins = map_.insert(std::make_pair(data, idx));
    assert_ice(ins.second, "Failed to insert into ID map.");
    return idx;
  }

  /** \brief     Get the value with ID \a id.
   *  \param  id ID to look up
   *  \return    String value
   *
   * Asserts if \a id is not valid.
   */
  [[nodiscard]] auto get(Id id) const noexcept -> Value const&
  {
    assert_ice(static_cast<std::size_t>(id) < ids_.size(), "ID out of range.");
    return *ids_[static_cast<std::size_t>(id)];
  }

private:
  /** \brief  Hashing class for the Value.
   *
   * We want to do a deep hash (of what the value points to) not just a pointer comparison.
   */
  class Hasher
  {
  public:
    auto operator()(Value const* v) const -> std::size_t
    {
      return v == nullptr ? 0 : std::hash<Value>()(*v);
    }
  };

  /** \brief  Are two keys equal?
   *
   * We cheat by doing a shallow check before the deep one
   */
  class KeyEqual
  {
  public:
    constexpr auto operator()(Value const* v1, Value const* v2) const
    {
      if (v1 == v2) {
        return true;
      }
      if (v1 == nullptr || v2 == nullptr) {
        return false;
      }
      return *v1 == *v2;
    }
  };

  std::unordered_map<Value const*, Id, Hasher, KeyEqual> map_;  ///< Map String to ID.
  std::vector<Value const*> ids_;  ///< Vector of strings - ID is index in vector.
};
}  // namespace GD::CPP

#endif  // CC_LIBCPP_ID_MAP_HH_INCLUDED_
