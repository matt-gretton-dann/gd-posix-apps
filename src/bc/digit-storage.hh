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
#include <type_traits>
#include <utility>
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
 *
 * Non-heap states keep a live Inline member (empty when absent). Heap states
 * keep a live non-null Owner. The unsigned integral limbs are trivial, so their
 * union member can be replaced without destruction; owners are explicitly
 * constructed/destroyed. All throwing allocation work precedes a tag change.
 * This is ordinary C++ object storage, not pointer tagging or layout punning.
 */
template<typename Limb, std::size_t InlineCapacity = 3>
class DigitStorage
{
  static_assert(InlineCapacity > 0 && InlineCapacity < 255);
  static_assert(std::is_integral_v<Limb> && std::is_unsigned_v<Limb>);
  using Heap = std::vector<Limb, Profile::Allocator<Limb, Profile::AllocationKind::digits>>;
  struct Inline
  {
    std::array<Limb, InlineCapacity> limbs{};
    unsigned char size = 0;
  };
  using Owner = std::shared_ptr<Heap>;
  enum class Kind : unsigned char { absent, inline_value, heap };
  union Storage
  {
    Inline small;
    Owner owner;
    Storage() noexcept {}   // Each constructor starts the selected member explicitly.
    ~Storage() noexcept {}  // DigitStorage destroys the active owner explicitly.
  } storage_;
  Kind kind_ = Kind::absent;

  // At every public boundary heap means a live, non-null Owner; other states
  // have a live Inline member. Moves restore that invariant in their source.
  auto heap() -> Owner*
  {
    assert(has_heap());
    return std::addressof(storage_.owner);
  }
  auto heap() const -> Owner const*
  {
    assert(has_heap());
    return std::addressof(storage_.owner);
  }
  void install_heap(Owner&& owner) noexcept
  {
    assert(!has_heap() && owner);
    std::construct_at(std::addressof(storage_.owner), std::move(owner));
    kind_ = Kind::heap;
  }
  auto small() -> Inline&
  {
    assert(!has_heap());
    if (kind_ == Kind::absent) {
      kind_ = Kind::inline_value;
    }
    return storage_.small;
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
    return storage_.small.limbs.data();
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
    install_heap(std::move(owner));
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

  DigitStorage() noexcept
  {
    std::construct_at(std::addressof(storage_.small));
    kind_ = Kind::inline_value;
  }
  DigitStorage(std::nullptr_t) noexcept { std::construct_at(std::addressof(storage_.small)); }
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
      install_heap(GD_BC_PROFILE_SHARED(Profile::AllocationKind::digits, Heap)(first, last));
    }
  }
  DigitStorage(std::initializer_list<Limb> values) : DigitStorage(values.begin(), values.end()) {}
  DigitStorage(DigitStorage const& other) noexcept
  {
    if (other.has_heap()) {
      std::construct_at(std::addressof(storage_.owner), *other.heap());
    }
    else {
      std::construct_at(std::addressof(storage_.small), other.storage_.small);
    }
    kind_ = other.kind_;
  }
  auto operator=(DigitStorage const& other) noexcept -> DigitStorage&
  {
    if (this != &other) {
      if (has_heap() && other.has_heap()) {
        *heap() = *other.heap();
      }
      else if (other.has_heap()) {
        std::construct_at(std::addressof(storage_.owner), *other.heap());
      }
      else {
        if (has_heap()) {
          std::destroy_at(heap());
        }
        std::construct_at(std::addressof(storage_.small), other.storage_.small);
      }
      kind_ = other.kind_;
    }
    return *this;
  }
  DigitStorage(DigitStorage&& other) noexcept
  {
    if (other.has_heap()) {
      std::construct_at(std::addressof(storage_.owner), std::move(*other.heap()));
    }
    else {
      std::construct_at(std::addressof(storage_.small), other.storage_.small);
    }
    kind_ = other.kind_;
    other.reset();
  }
  auto operator=(DigitStorage&& other) noexcept -> DigitStorage&
  {
    if (this != &other) {
      if (has_heap() && other.has_heap()) {
        *heap() = std::move(*other.heap());
      }
      else if (other.has_heap()) {
        std::construct_at(std::addressof(storage_.owner), std::move(*other.heap()));
      }
      else {
        if (has_heap()) {
          std::destroy_at(heap());
        }
        std::construct_at(std::addressof(storage_.small), other.storage_.small);
      }
      kind_ = other.kind_;
      other.reset();
    }
    return *this;
  }
  ~DigitStorage() noexcept
  {
    if (has_heap()) {
      std::destroy_at(heap());
    }
  }

  [[nodiscard]] auto has_heap() const noexcept -> bool { return kind_ == Kind::heap; }
  explicit operator bool() const noexcept { return kind_ != Kind::absent; }
  auto operator->() noexcept -> DigitStorage* { return this; }
  auto operator->() const noexcept -> DigitStorage const* { return this; }
  auto operator*() noexcept -> DigitStorage& { return *this; }
  auto operator*() const noexcept -> DigitStorage const& { return *this; }
  void reset() noexcept
  {
    if (has_heap()) {
      std::destroy_at(heap());
    }
    std::construct_at(std::addressof(storage_.small));
    kind_ = Kind::absent;
  }
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
    return storage_.small.size;
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
