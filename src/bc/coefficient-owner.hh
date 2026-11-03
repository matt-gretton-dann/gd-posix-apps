/** \file coefficient-owner.hh
 * Narrow strong/weak ownership for heap coefficient vectors.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_COEFFICIENT_OWNER_HH
#define GD_BC_COEFFICIENT_OWNER_HH

#include <atomic>
#include <cassert>
#include <cstddef>
#include <memory>
#include <utility>

#include "profiling.hh"

namespace GD::Bc::Details {

/** One-pointer coefficient owner; independently copied handles remain safe.
 *
 * This supplies only the operations needed by DigitStorage. Weak observations
 * identify a lifetime without retaining its vector or limb allocation. Their
 * control block survives expiry, preventing allocation-address reuse from
 * matching a later coefficient. There is no weak-to-strong promotion API.
 * Sharing a handle does not make concurrent mutation of its vector safe.
 */
template<typename Heap>
class CoefficientOwner
{
  struct Block
  {
    std::atomic<std::size_t> strong{1};
    // One implicit weak reference survives while any strong owner exists.
    std::atomic<std::size_t> weak{1};
    union
    {
      Heap value;
    };

    template<typename... Args>
    explicit Block(Args&&... args) : value(std::forward<Args>(args)...)
    {
    }
    // Last-strong release destroys value before last-weak frees this block.
    ~Block() noexcept {}
  };
  using Allocator = Profile::Allocator<Block, Profile::AllocationKind::digits>;
  using Traits = std::allocator_traits<Allocator>;
  Block* block_ = nullptr;

  static void release_weak(Block* block) noexcept
  {
    if (block && block->weak.fetch_sub(1, std::memory_order_acq_rel) == 1) {
      Allocator allocator;
      Traits::destroy(allocator, block);
      Traits::deallocate(allocator, block, 1);
    }
  }
  void retain() const noexcept
  {
    if (block_) {
      block_->strong.fetch_add(1, std::memory_order_relaxed);
    }
  }

public:
  class Weak
  {
    friend class CoefficientOwner;
    Block* block_ = nullptr;

    void retain() const noexcept
    {
      if (block_) {
        block_->weak.fetch_add(1, std::memory_order_relaxed);
      }
    }

  public:
    Weak() noexcept = default;
    Weak(CoefficientOwner const& owner) noexcept : block_(owner.block_) { retain(); }
    Weak(Weak const& other) noexcept : block_(other.block_) { retain(); }
    Weak(Weak&& other) noexcept : block_(std::exchange(other.block_, nullptr)) {}
    ~Weak() { reset(); }
    auto operator=(Weak const& other) noexcept -> Weak&
    {
      if (this != &other) {
        Weak copy(other);
        std::swap(block_, copy.block_);
      }
      return *this;
    }
    auto operator=(Weak&& other) noexcept -> Weak&
    {
      if (this != &other) {
        reset();
        block_ = std::exchange(other.block_, nullptr);
      }
      return *this;
    }
    void reset() noexcept { release_weak(std::exchange(block_, nullptr)); }
    [[nodiscard]] auto expired() const noexcept -> bool
    {
      return !block_ || block_->strong.load(std::memory_order_acquire) == 0;
    }
  };

  CoefficientOwner() noexcept = default;
  CoefficientOwner(CoefficientOwner const& other) noexcept : block_(other.block_) { retain(); }
  CoefficientOwner(CoefficientOwner&& other) noexcept : block_(std::exchange(other.block_, nullptr))
  {
  }
  ~CoefficientOwner() { reset(); }
  auto operator=(CoefficientOwner const& other) noexcept -> CoefficientOwner&
  {
    if (this != &other) {
      CoefficientOwner copy(other);
      std::swap(block_, copy.block_);
    }
    return *this;
  }
  auto operator=(CoefficientOwner&& other) noexcept -> CoefficientOwner&
  {
    if (this != &other) {
      reset();
      block_ = std::exchange(other.block_, nullptr);
    }
    return *this;
  }
  template<typename... Args>
  static auto make(Args&&... args) -> CoefficientOwner
  {
    Allocator allocator;
    auto* block = Traits::allocate(allocator, 1);
    try {
      Traits::construct(allocator, block, std::forward<Args>(args)...);
    }
    catch (...) {
      Traits::deallocate(allocator, block, 1);
      throw;
    }
    CoefficientOwner result;
    result.block_ = block;
    return result;
  }
  explicit operator bool() const noexcept { return block_ != nullptr; }
  auto operator->() const noexcept -> Heap*
  {
    assert(block_);
    return std::addressof(block_->value);
  }
  void reset() noexcept
  {
    auto* block = std::exchange(block_, nullptr);
    if (block && block->strong.fetch_sub(1, std::memory_order_acq_rel) == 1) {
      block->value.~Heap();
      release_weak(block);
    }
  }
  [[nodiscard]] auto use_count() const noexcept -> long
  {
    return block_ ? static_cast<long>(block_->strong.load(std::memory_order_relaxed)) : 0L;
  }
  [[nodiscard]] auto shares_owner(Weak const& other) const noexcept -> bool
  {
    return block_ && block_ == other.block_;
  }
};
}  // namespace GD::Bc::Details
#endif
