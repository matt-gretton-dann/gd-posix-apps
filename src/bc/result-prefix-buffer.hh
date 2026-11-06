/** Reserved activation storage with a demand-constructed prefix.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_RESULT_PREFIX_BUFFER_HH_INCLUDED
#define GD_BC_RESULT_PREFIX_BUFFER_HH_INCLUDED

#include <cassert>
#include <cstddef>
#include <memory>
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
 */
template<typename Value, typename Allocator = std::allocator<Value>>
class ResultPrefixBuffer
{
public:
  void prepare(std::size_t count, bool eager = false)
  {
    assert(values_.empty());
    values_.reserve(count);
    if (eager)
      values_.resize(count);
  }

  auto operator[](std::size_t index) -> Value&
  {
    assert(index < values_.capacity());
    if (index >= values_.size())
      values_.resize(index + 1);
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
