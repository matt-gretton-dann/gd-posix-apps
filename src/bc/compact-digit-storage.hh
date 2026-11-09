/** \file compact-digit-storage.hh
 * Experimental variant-managed coefficient storage with live-prefix copies.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_COMPACT_DIGIT_STORAGE_HH
#define GD_BC_COMPACT_DIGIT_STORAGE_HH

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "profiling.hh"

namespace GD::Bc::Details {

/** Contiguous coefficient storage, with explicit COW at the caller's boundary.
 *
 * Small copies own their inline values. Large copies share a vector until the
 * caller invokes clone(). Growth invalidates iterators like vector growth;
 * shrinking retains heap storage rather than unexpectedly relocating iterators.
 * Reset/assignment and a shared-small clone can return to inline storage.
 * This supplies only the vector operations used by BasicDigits, not a general
 * allocator-aware vector interface. Nullable storage preserves its existing
 * absent-versus-owned-empty distinction without allocating an empty owner.
 */
template<typename Limb, std::size_t InlineCapacity = 3>
class CompactDigitStorage
{
  static_assert(InlineCapacity > 0 && InlineCapacity < 255);
  static_assert(std::is_integral_v<Limb> && std::is_unsigned_v<Limb>);
  using Heap = std::vector<Limb, Profile::Allocator<Limb, Profile::AllocationKind::digits>>;
  struct Inline
  {
    // Only [0, size) is initialized and copied. An absent sentinel exposes no
    // elements. Never copy this aggregate wholesale or read its unused suffix.
    Inline() noexcept {}
    Inline(Inline const&) = delete;
    auto operator=(Inline const&) -> Inline& = delete;
    // The variant must never copy an entire partially initialized buffer.
    std::array<Limb, InlineCapacity> limbs;
    unsigned char size = 0;
  };
  // Standard variant operations manage lifetimes. Only Owner alternatives are
  // transferred wholesale; inline transfers explicitly read the live prefix.
  static constexpr unsigned char absent_size = 255;
  using Owner = std::shared_ptr<Heap>;
  std::variant<Inline, Owner> storage_;

  auto heap() noexcept -> Owner* { return std::get_if<Owner>(&storage_); }
  auto heap() const noexcept -> Owner const* { return std::get_if<Owner>(&storage_); }
  auto inline_value() noexcept -> Inline& { return std::get<Inline>(storage_); }
  auto inline_value() const noexcept -> Inline const& { return std::get<Inline>(storage_); }
  void copy_inline(CompactDigitStorage const& other) noexcept
  {
    assert(!other.has_heap());
    if (has_heap()) {
      storage_.template emplace<Inline>();
    }
    auto& destination = inline_value();
    auto const& source = other.inline_value();
    destination.size = source.size;
    if (source.size != absent_size) {
      assert(source.size <= InlineCapacity);
      std::copy_n(source.limbs.begin(), source.size, destination.limbs.begin());
    }
  }
  auto small() noexcept -> Inline&
  {
    auto& value = inline_value();
    if (value.size == absent_size) {
      value.size = 0;
    }
    return value;
  }
  auto data() -> Limb*
  {
    if (has_heap()) {
      return (*heap())->data();
    }
    return small().limbs.data();
  }
  auto data() const -> Limb const*
  {
    if (has_heap()) {
      return (*heap())->data();
    }
    return inline_value().limbs.data();
  }
  void promote(std::size_t capacity)
  {
    if (has_heap()) {
      return;
    }
    auto owner = GD_BC_PROFILE_SHARED(Profile::AllocationKind::digits, Heap)();
    owner->reserve(capacity);
    auto const& value = small();
    owner->insert(owner->end(), value.limbs.begin(), value.limbs.begin() + value.size);
    storage_.template emplace<Owner>(std::move(owner));
  }

public:
  using value_type = Limb;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using iterator = Limb*;
  using const_iterator = Limb const*;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  static constexpr std::size_t inline_capacity = InlineCapacity;

  // A user-provided constructor leaves unused limb slots uninitialized.
  // Growth fills every new live slot, and copies only read the live prefix.
  CompactDigitStorage() noexcept : storage_(std::in_place_type<Inline>) {}
  CompactDigitStorage(std::nullptr_t) noexcept : CompactDigitStorage()
  {
    inline_value().size = absent_size;
  }
  CompactDigitStorage(size_type count, Limb value) : CompactDigitStorage() { resize(count, value); }
  CompactDigitStorage(const_iterator first, const_iterator last) : CompactDigitStorage()
  {
    auto const count = first == last ? size_type{0} : static_cast<size_type>(last - first);
    if (count <= InlineCapacity) {
      if (count != 0) {
        std::copy(first, last, small().limbs.begin());
      }
      small().size = static_cast<unsigned char>(count);
    }
    else {
      storage_.template emplace<Owner>(
        GD_BC_PROFILE_SHARED(Profile::AllocationKind::digits, Heap)(first, last));
    }
  }
  CompactDigitStorage(std::initializer_list<Limb> values)
      : CompactDigitStorage(values.begin(), values.end())
  {
  }
  CompactDigitStorage(CompactDigitStorage const& other) noexcept : CompactDigitStorage()
  {
    if (other.has_heap()) {
      storage_.template emplace<Owner>(*other.heap());
    }
    else {
      copy_inline(other);
    }
  }
  auto operator=(CompactDigitStorage const& other) noexcept -> CompactDigitStorage&
  {
    if (this != &other) {
      if (other.has_heap()) {
        storage_.template emplace<Owner>(*other.heap());
      }
      else {
        copy_inline(other);
      }
    }
    return *this;
  }
  CompactDigitStorage(CompactDigitStorage&& other) noexcept : CompactDigitStorage()
  {
    if (other.has_heap()) {
      storage_.template emplace<Owner>(std::move(*other.heap()));
    }
    else {
      copy_inline(other);
    }
    other.reset();
  }
  auto operator=(CompactDigitStorage&& other) noexcept -> CompactDigitStorage&
  {
    if (this != &other) {
      if (other.has_heap()) {
        storage_.template emplace<Owner>(std::move(*other.heap()));
      }
      else {
        copy_inline(other);
      }
      other.reset();
    }
    return *this;
  }

  // Weak observations retain the control block, never coefficient storage or
  // a strong owner. Owner ordering distinguishes lifetimes even after expiry.
  using WeakOwner = std::weak_ptr<Heap const>;
  [[nodiscard]] auto weak_owner() const noexcept -> WeakOwner
  {
    return has_heap() ? WeakOwner(*heap()) : WeakOwner{};
  }
  [[nodiscard]] auto shares_owner(WeakOwner const& other) const noexcept -> bool
  {
    return has_heap() && !heap()->owner_before(other) && !other.owner_before(*heap());
  }
  [[nodiscard]] auto capacity() const noexcept -> size_type
  {
    return has_heap() ? (*heap())->capacity() : (static_cast<bool>(*this) ? InlineCapacity : 0);
  }

  [[nodiscard]] auto has_heap() const noexcept -> bool
  {
    return std::holds_alternative<Owner>(storage_);
  }
  explicit operator bool() const noexcept
  {
    return has_heap() || inline_value().size != absent_size;
  }
  auto operator->() noexcept -> CompactDigitStorage* { return this; }
  auto operator->() const noexcept -> CompactDigitStorage const* { return this; }
  auto operator*() noexcept -> CompactDigitStorage& { return *this; }
  auto operator*() const noexcept -> CompactDigitStorage const& { return *this; }
  void reset() noexcept
  {
    storage_.template emplace<Inline>();
    inline_value().size = absent_size;
  }
  [[nodiscard]] auto use_count() const noexcept -> long
  {
    return has_heap() ? (*heap()).use_count() : (static_cast<bool>(*this) ? 1L : 0L);
  }
  [[nodiscard]] auto clone() const -> CompactDigitStorage
  {
    return CompactDigitStorage(begin(), end());
  }
  [[nodiscard]] auto size() const noexcept -> size_type
  {
    if (has_heap()) {
      return (*heap())->size();
    }
    auto const count = inline_value().size;
    return count == absent_size ? 0 : count;
  }
  [[nodiscard]] auto empty() const noexcept -> bool { return size() == 0; }
  auto begin() -> iterator { return data(); }
  auto begin() const -> const_iterator { return data(); }
  auto end() -> iterator { return begin() + size(); }
  auto end() const -> const_iterator { return begin() + size(); }
  auto rbegin() -> reverse_iterator { return reverse_iterator(end()); }
  auto rbegin() const -> const_reverse_iterator { return const_reverse_iterator(end()); }
  auto rend() -> reverse_iterator { return reverse_iterator(begin()); }
  auto rend() const -> const_reverse_iterator { return const_reverse_iterator(begin()); }
  auto operator[](size_type index) -> Limb& { return data()[index]; }
  auto operator[](size_type index) const -> Limb const& { return data()[index]; }
  auto at(size_type index) -> Limb&
  {
    if (index >= size()) {
      throw std::out_of_range("bc digit index");
    }
    return (*this)[index];
  }
  auto at(size_type index) const -> Limb const&
  {
    if (index >= size()) {
      throw std::out_of_range("bc digit index");
    }
    return (*this)[index];
  }
  auto back() -> Limb& { return (*this)[size() - 1]; }
  auto back() const -> Limb const& { return (*this)[size() - 1]; }
  void resize(size_type count, Limb value = Limb{})
  {
    if (count > InlineCapacity) {
      promote(count);
    }
    if (has_heap()) {
      assert(use_count() == 1);  // Caller must detach before mutating shared storage.
      (*heap())->resize(count, value);
    }
    else {
      auto& buffer = small();
      if (count > buffer.size) {
        std::fill(buffer.limbs.begin() + buffer.size, buffer.limbs.begin() + count, value);
      }
      buffer.size = static_cast<unsigned char>(count);
    }
  }
  void push_back(Limb value)
  {
    auto const count = size();
    if (count == InlineCapacity && !has_heap()) {
      promote(2 * InlineCapacity);
    }
    if (has_heap()) {
      assert(use_count() == 1);
      (*heap())->push_back(value);
    }
    else {
      auto& buffer = small();
      buffer.limbs[buffer.size++] = value;
    }
  }
  void pop_back()
  {
    assert(!empty());
    if (has_heap()) {
      assert(use_count() == 1);
      (*heap())->pop_back();
    }
    else {
      --small().size;
    }
  }
};
}  // namespace GD::Bc::Details
#endif
