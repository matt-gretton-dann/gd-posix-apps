/** Ordinary-context ownership for an opaque routed return value.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_INTEGER_CALL_RESULT_HH_INCLUDED
#define GD_BC_INTEGER_CALL_RESULT_HH_INCLUDED

#include <new>

#include <cassert>
#include <memory>
#include <type_traits>
#include <utility>

#include "bc.hh"

namespace GD::Bc::Details {
// This owning helper belongs outside the native router. A complete argument
// owns the value before placement construction starts. Publishing and taking
// the result each add one explicit nonthrowing move.
class IntegerCallResult
{
public:
  IntegerCallResult() = default;
  IntegerCallResult(IntegerCallResult const&) = delete;
  auto operator=(IntegerCallResult const&) -> IntegerCallResult& = delete;
  ~IntegerCallResult() noexcept
  {
    if (constructed_)
      storage_.number.~Number();
  }

  auto has_value() const noexcept -> bool { return constructed_; }

  void construct(Number&& value) noexcept
  {
    static_assert(std::is_nothrow_move_constructible_v<Number>);
    assert(!constructed_);
    // Argument evaluation, including any throwing factory, completes before
    // this call. Only a nonthrowing transfer happens inside placement storage.
    ::new (static_cast<void*>(std::addressof(storage_.number))) Number(std::move(value));
    constructed_ = true;
  }

  auto take() noexcept -> Number
  {
    static_assert(std::is_nothrow_move_constructible_v<Number>);
    assert(constructed_);
    return std::move(storage_.number);
  }

private:
  union Storage
  {
    Storage() noexcept {}
    ~Storage() noexcept {}
    Number number;
  } storage_;
  bool constructed_ = false;
};
}  // namespace GD::Bc::Details
#endif
