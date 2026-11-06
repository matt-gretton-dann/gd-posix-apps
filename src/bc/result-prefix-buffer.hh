/** Reserved activation storage with a demand-constructed prefix.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_RESULT_PREFIX_BUFFER_HH_INCLUDED
#define GD_BC_RESULT_PREFIX_BUFFER_HH_INCLUDED

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>
#include <type_traits>
#include <vector>

namespace GD::Bc::Details {
/** Keep capacity stable throughout one activation without constructing its tail.
 *
 * prepare() requires an empty buffer and may allocate before the caller acquires
 * a depth lease. Mutable access constructs the missing contiguous prefix;
 * indices must remain below that activation's requested count. Capacity can
 * exceed that count after an earlier larger activation, so it is not a logical
 * bounds oracle. Verified result plans retain responsibility for index validity.
 *
 * No access reallocates after successful preparation. clear() destroys every
 * constructed value and retains capacity, including during exception cleanup.
 * Const access requires an already constructed cell. The eager preparation
 * mode supplies an independent full-length initialization path for diagnostics.
 * Failed preparation leaves an empty buffer; failed prefix growth restores
 * the previous prefix without moving its cells, even if vector retains partial
 * construction on the underlying standard library.
 */
template<typename Value, typename Allocator = std::allocator<Value>>
class ResultPrefixBuffer
{
  static_assert(std::is_nothrow_destructible_v<Value>);

public:
  void prepare(std::size_t count, bool eager = false)
  {
    assert(values_.empty());
    try {
      if (count > values_.capacity()) {
        // Exact reserve on successively larger empty activations can allocate
        // on every call. Retain geometric growth, saturating before doubling.
        auto const capacity = values_.capacity();
        auto const limit = values_.max_size();
        auto const grown = capacity > limit / 2 ? limit : 2 * capacity;
        values_.reserve(std::max(count, grown));
      }
      if (eager)
        values_.resize(count);
    }
    catch (...) {
      // Some vector implementations retain partially appended cells when a
      // default constructor throws. No activation lease exists yet.
      values_.clear();
      throw;
    }
  }

  auto operator[](std::size_t index) -> Value&
  {
    assert(index < values_.capacity());
    if (index >= values_.size()) {
      auto const previous = values_.size();
      try {
        values_.resize(index + 1);
      }
      catch (...) {
        // Preserve the old prefix and its references independently of the
        // standard library's partial-construction rollback policy.
        while (values_.size() > previous)
          values_.pop_back();
        throw;
      }
    }
    return values_[index];
  }

  auto operator[](std::size_t index) const -> Value const&
  {
    assert(index < values_.size());
    return values_[index];
  }

  auto empty() const noexcept -> bool { return values_.empty(); }
  auto size() const noexcept -> std::size_t { return values_.size(); }
  auto capacity() const noexcept -> std::size_t { return values_.capacity(); }
  void clear() noexcept { values_.clear(); }

private:
  std::vector<Value, Allocator> values_;
};
}  // namespace GD::Bc::Details
#endif
