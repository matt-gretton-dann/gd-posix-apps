/** \file digit-storage.hh
 * EXPERIMENT ONLY: inline limbs with unique, eagerly copied heap storage.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_DIGIT_STORAGE_HH
#define GD_BC_DIGIT_STORAGE_HH

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
#include <vector>

#include "profiling.hh"

namespace GD::Bc::Details {

/** Contiguous coefficient storage, with explicit COW at the caller's boundary.
 *
 * Small copies own their inline values. Large copies eagerly clone the vector.
 * This experimental policy retains the caller facade for value comparisons. Growth invalidates iterators like vector growth;
 * shrinking retains heap storage rather than unexpectedly relocating iterators.
 * Reset/assignment and a shared-small clone can return to inline storage.
 * This supplies only the vector operations used by BasicDigits, not a general
 * allocator-aware vector interface. Nullable storage preserves its existing
 * absent-versus-owned-empty distinction without allocating an empty owner.
 */
template<typename Limb, std::size_t InlineCapacity = 3>
class DigitStorage
{
  static_assert(InlineCapacity > 0 && InlineCapacity < 255);
  static_assert(std::is_integral_v<Limb> && std::is_unsigned_v<Limb>);
  using Heap = std::vector<Limb, Profile::Allocator<Limb, Profile::AllocationKind::digits>>;
  struct Inline
  {
    // Only [0, size) is initialized and copied. An absent sentinel exposes no
    // elements. Never copy this aggregate wholesale or read its unused suffix.
    std::array<Limb, InlineCapacity> limbs;
    unsigned char size = 0;
  };
  // All members stay alive: no union, pointer tagging or explicit lifetime
  // management. A non-null owner selects heap storage. Its inline size is absent.
  static constexpr unsigned char absent_size = 255;
  using Owner = std::unique_ptr<Heap>;
  Inline small_;
  Owner owner_;

  auto heap() -> Owner*
  {
    assert(owner_);
    return std::addressof(owner_);
  }
  auto heap() const -> Owner const*
  {
    assert(owner_);
    return std::addressof(owner_);
  }
  void copy_inline(DigitStorage const& other) noexcept
  {
    assert(!owner_ && !other.owner_);
    small_.size = other.small_.size;
    if (small_.size != absent_size) {
      assert(small_.size <= InlineCapacity);
      std::copy_n(other.small_.limbs.begin(), small_.size, small_.limbs.begin());
    }
  }
  auto small() -> Inline&
  {
    assert(!owner_);
    if (small_.size == absent_size) {
      small_.size = 0;
    }
    return small_;
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
    return small_.limbs.data();
  }
  void promote(std::size_t capacity)
  {
    if (has_heap()) {
      return;
    }
    auto owner = std::make_unique<Heap>();
    owner->reserve(capacity);
    auto const& value = small();
    owner->insert(owner->end(), value.limbs.begin(), value.limbs.begin() + value.size);
    owner_ = std::move(owner);
    small_.size = absent_size;
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
  DigitStorage() noexcept {}
  DigitStorage(std::nullptr_t) noexcept { small_.size = absent_size; }
  DigitStorage(size_type count, Limb value) : DigitStorage() { resize(count, value); }
  DigitStorage(const_iterator first, const_iterator last) : DigitStorage()
  {
    auto const count = first == last ? size_type{0} : static_cast<size_type>(last - first);
    if (count <= InlineCapacity) {
      if (count != 0) {
        std::copy(first, last, small().limbs.begin());
      }
      small().size = static_cast<unsigned char>(count);
    }
    else {
      owner_ = std::make_unique<Heap>(first, last);
      small_.size = absent_size;
    }
  }
  DigitStorage(std::initializer_list<Limb> values) : DigitStorage(values.begin(), values.end()) {}
  DigitStorage(DigitStorage const& other)
      : owner_(other.owner_ ? std::make_unique<Heap>(*other.owner_) : nullptr)
  {
    if (owner_) {
      small_.size = absent_size;
    }
    else {
      copy_inline(other);
    }
  }
  auto operator=(DigitStorage const& other) -> DigitStorage&
  {
    if (this != &other) {
      // Prepare a potentially throwing deep copy before changing this value.
      auto copy = other.owner_ ? std::make_unique<Heap>(*other.owner_) : nullptr;
      owner_ = std::move(copy);
      if (owner_) {
        small_.size = absent_size;
      }
      else {
        copy_inline(other);
      }
    }
    return *this;
  }
  DigitStorage(DigitStorage&& other) noexcept : owner_(std::move(other.owner_))
  {
    if (owner_) {
      small_.size = absent_size;
    }
    else {
      copy_inline(other);
    }
    other.reset();
  }
  auto operator=(DigitStorage&& other) noexcept -> DigitStorage&
  {
    if (this != &other) {
      owner_ = std::move(other.owner_);
      if (owner_) {
        small_.size = absent_size;
      }
      else {
        copy_inline(other);
      }
      other.reset();
    }
    return *this;
  }

  [[nodiscard]] auto has_heap() const noexcept -> bool
  {
    return static_cast<bool>(owner_);
  }
  explicit operator bool() const noexcept
  {
    return owner_ || small_.size != absent_size;
  }
  auto operator->() noexcept -> DigitStorage* { return this; }
  auto operator->() const noexcept -> DigitStorage const* { return this; }
  auto operator*() noexcept -> DigitStorage& { return *this; }
  auto operator*() const noexcept -> DigitStorage const& { return *this; }
  void reset() noexcept
  {
    owner_.reset();
    small_.size = absent_size;
  }
  [[nodiscard]] auto use_count() const noexcept -> long
  {
    return static_cast<bool>(*this) ? 1L : 0L;
  }
  [[nodiscard]] auto clone() const -> DigitStorage { return DigitStorage(begin(), end()); }
  [[nodiscard]] auto size() const noexcept -> size_type
  {
    if (has_heap()) {
      return (*heap())->size();
    }
    auto const count = small_.size;
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
