/** \file scalar-bindings.hh
 * Reusable scalar-local storage with dynamically scoped bindings.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_SCALAR_BINDINGS_HH
#define GD_BC_SCALAR_BINDINGS_HH

#include <array>
#include <cassert>
#include <cstddef>
#include <deque>
#include <memory>
#include <span>
#include <stdexcept>
#include <type_traits>
#include <vector>

namespace GD::Bc::Details {

/** Local bindings preserve the caller's values in place rather than moving
 * them through a save buffer. A depth owns its live scalar slots until its
 * lease ends; deeper deque growth cannot relocate those slots. Entry finishes
 * every potentially throwing allocation/construction before changing bindings.
 * Exit restores bindings and destroys local values while retaining capacity.
 * This is dynamically scoped: unselected names still refer to the caller's
 * active locals or globals. Leases must be destroyed in reverse entry order.
 */
template<typename Value, std::size_t Count, typename Allocator = std::allocator<Value>>
class ScalarBindings
{
  static_assert(std::is_nothrow_move_constructible_v<Value>);
  static_assert(std::is_nothrow_destructible_v<Value>);
  struct Storage
  {
    std::vector<Value, Allocator> values;
    using PointerAllocator =
      typename std::allocator_traits<Allocator>::template rebind_alloc<Value*>;
    std::vector<Value*, PointerAllocator> previous;
  };
  using FrameAllocator = typename std::allocator_traits<Allocator>::template rebind_alloc<Storage>;

public:
  ScalarBindings()
  {
    for (std::size_t i = 0; i != Count; ++i)
      current_[i] = &globals_[i];
  }
  ScalarBindings(ScalarBindings const&) = delete;
  auto operator=(ScalarBindings const&) -> ScalarBindings& = delete;
  ScalarBindings(ScalarBindings&&) = delete;
  auto operator=(ScalarBindings&&) -> ScalarBindings& = delete;

  class Frame
  {
  public:
    Frame(ScalarBindings& owner, std::span<unsigned const> keys)
        : owner_(owner), count_(keys.size())
    {
      if (keys.empty())
        return;
      std::array<bool, Count> seen{};
      for (auto key : keys) {
        if (key >= Count)
          throw std::out_of_range("scalar binding index");
        if (seen[key])
          throw std::invalid_argument("duplicate scalar binding");
        seen[key] = true;
      }
      depth_ = owner_.depth_;
      if (depth_ == owner_.frames_.size())
        owner_.frames_.emplace_back();
      auto& frame = owner_.frames_[depth_];
      assert(frame.values.empty());
      frame.previous.resize(keys.size());
      frame.values.resize(keys.size());
      // Everything below is nonthrowing. Failed setup leaves all old bindings
      // and depth unchanged, including a partially constructed value vector.
      for (std::size_t i = 0; i != keys.size(); ++i) {
        auto const key = keys[i];
        keys_[i] = key;
        frame.previous[i] = owner_.current_[key];
        owner_.current_[key] = &frame.values[i];
      }
      ++owner_.depth_;
    }
    Frame(Frame const&) = delete;
    auto operator=(Frame const&) -> Frame& = delete;
    ~Frame()
    {
      if (count_ == 0)
        return;
      assert(owner_.depth_ == depth_ + 1);
      auto& frame = owner_.frames_[depth_];
      for (std::size_t i = 0; i != count_; ++i)
        owner_.current_[keys_[i]] = frame.previous[i];
      frame.values.clear();
      frame.previous.clear();
      --owner_.depth_;
    }

  private:
    ScalarBindings& owner_;
    std::size_t depth_ = 0;
    std::size_t count_;
    std::array<unsigned, Count> keys_;
  };

  [[nodiscard]] auto enter(std::span<unsigned const> keys) -> Frame { return Frame(*this, keys); }
  auto at(std::size_t index) -> Value& { return *current_.at(index); }
  auto at(std::size_t index) const -> Value const& { return *current_.at(index); }

private:
  std::array<Value, Count> globals_;
  std::array<Value*, Count> current_;
  std::deque<Storage, FrameAllocator> frames_;
  std::size_t depth_ = 0;
};

}  // namespace GD::Bc::Details
#endif
