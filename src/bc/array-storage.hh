/** \file array-storage.hh
 * Bounded dense array prefix with an ordered sparse fallback.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_ARRAY_STORAGE_HH
#define GD_BC_ARRAY_STORAGE_HH
#include <algorithm>
#include <cstddef>
#include <map>
#include <optional>
#include <utility>
#include <vector>

#include "profiling.hh"
namespace GD::Bc::Details {
template<typename Number>
class HybridArrayStorage
{
  using Index = typename Number::NumType;
  using Dense =
    std::vector<std::optional<Number>,
                Profile::Allocator<std::optional<Number>, Profile::AllocationKind::arrays>>;
  using Sparse =
    std::map<Index, Number, std::less<Index>,
             Profile::Allocator<std::pair<Index const, Number>, Profile::AllocationKind::arrays>>;
  static constexpr std::size_t dense_limit = std::size_t{1} << 20;
  Dense dense_;
  Sparse sparse_;
  std::size_t entries_ = 0;
  std::size_t dense_entries_ = 0;
  Index highest_ = 0;

public:
  HybridArrayStorage() = default;
  HybridArrayStorage(HybridArrayStorage const&) = default;
  HybridArrayStorage(HybridArrayStorage&& other) noexcept
      : dense_(std::move(other.dense_)), sparse_(std::move(other.sparse_)),
        entries_(std::exchange(other.entries_, 0)),
        dense_entries_(std::exchange(other.dense_entries_, 0)),
        highest_(std::exchange(other.highest_, 0))
  {
    other.dense_.clear();
    other.sparse_.clear();
  }
  auto operator=(HybridArrayStorage other) noexcept -> HybridArrayStorage&
  {
    dense_.swap(other.dense_);
    sparse_.swap(other.sparse_);
    std::swap(entries_, other.entries_);
    std::swap(dense_entries_, other.dense_entries_);
    std::swap(highest_, other.highest_);
    return *this;
  }
  auto size() const noexcept -> std::size_t { return entries_; }
  auto empty() const noexcept -> bool { return entries_ == 0; }
  auto length() const noexcept -> Index
  {
    return empty() ? Index{0} : static_cast<Index>(highest_ + Index{1});
  }
  auto lookup(Index index) const -> Number const*
  {
    if (index < dense_.size()) {
      auto const& slot = dense_[static_cast<std::size_t>(index)];
      return slot ? &*slot : nullptr;
    }
    auto const found = sparse_.find(index);
    return found == sparse_.end() ? nullptr : &found->second;
  }
  // Take a value before growing: callers may pass a value borrowed from this
  // very array. Vector growth must not invalidate the source of assignment.
  void insert_or_assign(Index index, Number value)
  {
    if (index >= dense_.size() && index < dense_limit &&
        (index < 64 || index <= 4 * dense_entries_)) {
      auto const old_size = dense_.size();
      dense_.resize(static_cast<std::size_t>(index) + 1);
      // A formerly sparse key may now lie inside the grown prefix. Migrate
      // each stored value once without changing the logical entry count.
      auto it = sparse_.lower_bound(static_cast<Index>(old_size));
      while (it != sparse_.end() && it->first < dense_.size()) {
        dense_[static_cast<std::size_t>(it->first)] = std::move(it->second);
        ++dense_entries_;
        it = sparse_.erase(it);
      }
    }
    if (index < dense_.size()) {
      auto& slot = dense_[static_cast<std::size_t>(index)];
      if (!slot) {
        ++entries_;
        ++dense_entries_;
      }
      slot = std::move(value);
    }
    else {
      auto const inserted = sparse_.insert_or_assign(index, std::move(value)).second;
      if (inserted)
        ++entries_;
    }
    highest_ = std::max(highest_, index);
  }
};
}  // namespace GD::Bc::Details
#endif
