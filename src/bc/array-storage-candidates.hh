/** \file array-storage-candidates.hh
 * Portable storage candidates for the measured #209 comparison.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_ARRAY_STORAGE_CANDIDATES_HH
#define GD_BC_ARRAY_STORAGE_CANDIDATES_HH
#include <algorithm>
#include <array>
#include <cstddef>
#include <functional>
#include <map>
#include <optional>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include "profiling.hh"

namespace GD::Bc::Details {
/** Map-like storage keeps highest-written index independently of traversal. */
template<typename Number, bool Hashed>
class MapArrayStorage
{
  using Index = typename Number::NumType;
  using Allocator =
    Profile::Allocator<std::pair<Index const, Number>, Profile::AllocationKind::arrays>;
  using Ordered = std::map<Index, Number, std::less<Index>, Allocator>;
  using HashedMap =
    std::unordered_map<Index, Number, std::hash<Index>, std::equal_to<Index>, Allocator>;
  using Values = std::conditional_t<Hashed, HashedMap, Ordered>;
  Values values_;
  Index highest_ = 0;

public:
  MapArrayStorage() = default;
  MapArrayStorage(MapArrayStorage const&) = default;
  MapArrayStorage(MapArrayStorage&& other) noexcept
      : values_(std::move(other.values_)), highest_(std::exchange(other.highest_, Index{0}))
  {
    other.values_.clear();
  }
  auto operator=(MapArrayStorage other) noexcept -> MapArrayStorage&
  {
    values_.swap(other.values_);
    std::swap(highest_, other.highest_);
    return *this;
  }
  auto size() const noexcept -> std::size_t { return values_.size(); }
  auto empty() const noexcept -> bool { return values_.empty(); }
  auto length() const noexcept -> Index { return empty() ? Index{0} : Index(highest_ + Index{1}); }
  auto lookup(Index index) const -> Number const*
  {
    auto const found = values_.find(index);
    return found == values_.end() ? nullptr : &found->second;
  }
  void insert_or_assign(Index index, Number value)
  {
    values_.insert_or_assign(index, std::move(value));
    highest_ = std::max(highest_, index);
  }
};

/** Sorted flat storage charges insertion movement and preserves sparse keys. */
template<typename Number>
class FlatArrayStorage
{
  using Index = typename Number::NumType;
  using Entry = std::pair<Index, Number>;
  using Values = std::vector<Entry, Profile::Allocator<Entry, Profile::AllocationKind::arrays>>;
  Values values_;

public:
  FlatArrayStorage() = default;
  FlatArrayStorage(FlatArrayStorage const&) = default;
  FlatArrayStorage(FlatArrayStorage&& other) noexcept : values_(std::move(other.values_))
  {
    other.values_.clear();
  }
  auto operator=(FlatArrayStorage other) noexcept -> FlatArrayStorage&
  {
    values_.swap(other.values_);
    return *this;
  }
  auto size() const noexcept -> std::size_t { return values_.size(); }
  auto empty() const noexcept -> bool { return values_.empty(); }
  auto length() const noexcept -> Index
  {
    return empty() ? Index{0} : Index(values_.back().first + Index{1});
  }
  auto lookup(Index index) const -> Number const*
  {
    auto const found =
      std::lower_bound(values_.begin(), values_.end(), index,
                       [](Entry const& entry, Index key) { return entry.first < key; });
    return found == values_.end() || found->first != index ? nullptr : &found->second;
  }
  void insert_or_assign(Index index, Number value)
  {
    auto const found =
      std::lower_bound(values_.begin(), values_.end(), index,
                       [](Entry const& entry, Index key) { return entry.first < key; });
    if (found != values_.end() && found->first == index) {
      found->second = std::move(value);
    }
    else {
      values_.insert(found, Entry(index, std::move(value)));
    }
  }
};

/** Allocate only written pages, including explicit zero entries within a page. */
template<typename Number>
class PagedArrayStorage
{
  using Index = typename Number::NumType;
  static constexpr Index page_size = 64;
  using Page = std::array<std::optional<Number>, page_size>;
  using Pages =
    std::map<Index, Page, std::less<Index>,
             Profile::Allocator<std::pair<Index const, Page>, Profile::AllocationKind::arrays>>;
  Pages pages_;
  std::size_t entries_ = 0;
  Index highest_ = 0;

public:
  PagedArrayStorage() = default;
  PagedArrayStorage(PagedArrayStorage const&) = default;
  PagedArrayStorage(PagedArrayStorage&& other) noexcept
      : pages_(std::move(other.pages_)), entries_(std::exchange(other.entries_, 0)),
        highest_(std::exchange(other.highest_, Index{0}))
  {
    other.pages_.clear();
  }
  auto operator=(PagedArrayStorage other) noexcept -> PagedArrayStorage&
  {
    pages_.swap(other.pages_);
    std::swap(entries_, other.entries_);
    std::swap(highest_, other.highest_);
    return *this;
  }
  auto size() const noexcept -> std::size_t { return entries_; }
  auto empty() const noexcept -> bool { return entries_ == 0; }
  auto length() const noexcept -> Index { return empty() ? Index{0} : Index(highest_ + Index{1}); }
  auto lookup(Index index) const -> Number const*
  {
    auto const page = pages_.find(static_cast<Index>(index / page_size));
    if (page == pages_.end())
      return nullptr;
    auto const& slot = page->second[static_cast<std::size_t>(index % page_size)];
    return slot ? &*slot : nullptr;
  }
  void insert_or_assign(Index index, Number value)
  {
    auto& slot = pages_.try_emplace(static_cast<Index>(index / page_size))
                   .first->second[static_cast<std::size_t>(index % page_size)];
    if (!slot)
      ++entries_;
    slot = std::move(value);
    highest_ = std::max(highest_, index);
  }
};
}  // namespace GD::Bc::Details
#endif
