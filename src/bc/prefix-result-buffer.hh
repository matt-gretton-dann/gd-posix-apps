/** \file prefix-result-buffer.hh
 * Reusable result objects with conservative activation cleanup.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_PREFIX_RESULT_BUFFER_HH_INCLUDED
#define GD_BC_PREFIX_RESULT_BUFFER_HH_INCLUDED

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>
#include <vector>

namespace GD::Bc::Details {
/** Retain empty objects, but release every accessed owner at activation end.
 * Reset must restore an empty value without throwing. New T objects must also
 * be empty. A mutable access extends the cleanup prefix before returning its
 * reference, including when the subsequent assignment or operation throws.
 * Const reads cannot introduce owners. No raw mutable storage view escapes.
 *
 * The caller owns one buffer per simultaneous activation depth and must clear
 * it before resizing/reusing it. Retained objects and capacity are not live
 * language values; active size and cleanup prefix are separate from storage.
 */
template<typename T, typename Reset, typename Allocator = std::allocator<T>>
class PrefixResultBuffer
{
  using Storage = std::vector<T, Allocator>;
  static_assert(std::allocator_traits<Allocator>::is_always_equal::value);
  static_assert(std::is_nothrow_move_constructible_v<Storage>);
  static_assert(std::is_nothrow_default_constructible_v<Reset>);
  static_assert(std::is_nothrow_invocable_v<Reset, T&>);

public:
  PrefixResultBuffer() = default;
  PrefixResultBuffer(PrefixResultBuffer const&) = default;
  PrefixResultBuffer(PrefixResultBuffer&& other) noexcept
      : storage_(std::move(other.storage_)), active_(std::exchange(other.active_, 0)),
        accessed_(std::exchange(other.accessed_, 0))
  {
  }
  // Stage copies before replacing either the storage or its ownership bounds.
  auto operator=(PrefixResultBuffer other) noexcept -> PrefixResultBuffer&
  {
    storage_.swap(other.storage_);
    std::swap(active_, other.active_);
    std::swap(accessed_, other.accessed_);
    return *this;
  }

  [[nodiscard]] auto size() const noexcept -> std::size_t { return active_; }
  [[nodiscard]] auto empty() const noexcept -> bool { return active_ == 0; }
  // Logical object counts for diagnostics, not allocator capacity or RSS.
  [[nodiscard]] auto retained_size() const noexcept -> std::size_t { return storage_.size(); }
  [[nodiscard]] auto accessed_size() const noexcept -> std::size_t { return accessed_; }

  void resize(std::size_t count)
  {
    assert(empty());
    // Publish the activation only after potentially throwing growth succeeds.
    // A failed resize leaves the cleared buffer reusable at its old capacity.
    if (count > storage_.size())
      storage_.resize(count);
    active_ = count;
    accessed_ = 0;
  }

  auto operator[](std::size_t index) noexcept -> T&
  {
    assert(index < active_);
    accessed_ = std::max(accessed_, index + 1);
    return storage_[index];
  }
  auto operator[](std::size_t index) const noexcept -> T const&
  {
    assert(index < active_);
    return storage_[index];
  }

  void clear() noexcept
  {
    for (std::size_t i = 0; i < accessed_; ++i)
      Reset{}(storage_[i]);
    active_ = 0;
    accessed_ = 0;
  }

private:
  Storage storage_;
  std::size_t active_ = 0;
  std::size_t accessed_ = 0;
};
}  // namespace GD::Bc::Details
#endif  // GD_BC_PREFIX_RESULT_BUFFER_HH_INCLUDED
