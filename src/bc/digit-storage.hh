/** \file digit-storage.hh
 * Inline limbs with shared heap storage for larger numeric coefficients.
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
class DigitStorage
{
  static_assert(InlineCapacity > 0 && InlineCapacity < 255);
  using Heap = std::vector<Limb, Profile::Allocator<Limb, Profile::AllocationKind::digits>>;
  struct Inline
  {
    std::array<Limb, InlineCapacity> limbs{};
    unsigned char size = 0;
  };
  using Owner = std::shared_ptr<Heap>;
  std::variant<std::monostate, Inline, Owner> storage_;
  static constexpr std::array<Limb, InlineCapacity> empty_{};

  auto heap() -> Owner* { return std::get_if<Owner>(&storage_); }
  auto heap() const -> Owner const* { return std::get_if<Owner>(&storage_); }
  auto small() -> Inline&
  {
    if (std::holds_alternative<std::monostate>(storage_)) {
      storage_ = Inline{};
    }
    return std::get<Inline>(storage_);
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
    auto const* value = std::get_if<Inline>(&storage_);
    return value ? value->limbs.data() : empty_.data();
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
    storage_ = std::move(owner);
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

  DigitStorage() : storage_(Inline{}) {}
  DigitStorage(std::nullptr_t) noexcept : storage_(std::monostate{}) {}
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
      storage_ = GD_BC_PROFILE_SHARED(Profile::AllocationKind::digits, Heap)(first, last);
    }
  }
  DigitStorage(std::initializer_list<Limb> values) : DigitStorage(values.begin(), values.end()) {}
  DigitStorage(DigitStorage const&) noexcept = default;
  auto operator=(DigitStorage const&) noexcept -> DigitStorage& = default;
  DigitStorage(DigitStorage&& other) noexcept : storage_(std::move(other.storage_))
  {
    other.reset();
  }
  auto operator=(DigitStorage&& other) noexcept -> DigitStorage&
  {
    if (this != &other) {
      storage_ = std::move(other.storage_);
      other.reset();
    }
    return *this;
  }

  [[nodiscard]] auto has_heap() const noexcept -> bool
  {
    auto const* owner = heap();
    return owner && static_cast<bool>(*owner);
  }
  explicit operator bool() const noexcept
  {
    return !std::holds_alternative<std::monostate>(storage_) && (!heap() || has_heap());
  }
  auto operator->() noexcept -> DigitStorage* { return this; }
  auto operator->() const noexcept -> DigitStorage const* { return this; }
  auto operator*() noexcept -> DigitStorage& { return *this; }
  auto operator*() const noexcept -> DigitStorage const& { return *this; }
  void reset() noexcept { storage_ = std::monostate{}; }
  [[nodiscard]] auto use_count() const noexcept -> long
  {
    return has_heap() ? (*heap()).use_count() : (static_cast<bool>(*this) ? 1L : 0L);
  }
  [[nodiscard]] auto clone() const -> DigitStorage { return DigitStorage(begin(), end()); }
  [[nodiscard]] auto size() const noexcept -> size_type
  {
    if (has_heap()) {
      return (*heap())->size();
    }
    auto const* value = std::get_if<Inline>(&storage_);
    return value ? value->size : 0;
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
