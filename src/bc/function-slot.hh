/** \file function-slot.hh
 * One owning or deferred definition per dynamically replaceable function.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_FUNCTION_SLOT_HH
#define GD_BC_FUNCTION_SLOT_HH

#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

namespace GD::Bc::Details {

/** A deferred definition and its decoded owner are mutually exclusive.
 * Keep their storage together rather than reserving a second table in every
 * VM. Construction of an owning value happens before assignment; committing
 * it and clearing the previous alternative cannot throw. A pending descriptor
 * is borrowed only until the next replacement or reset of this slot.
 */
template<typename Value, typename Pending>
class FunctionSlot
{
  static_assert(std::is_nothrow_move_constructible_v<Value>);
  static_assert(std::is_nothrow_destructible_v<Value>);
  static_assert(std::is_nothrow_move_constructible_v<Pending>);
  static_assert(std::is_nothrow_destructible_v<Pending>);

public:
  // Match the optional owner's access protocol: pending is not decoded yet.
  [[nodiscard]] auto has_value() const noexcept -> bool { return state_.index() == 1; }
  explicit operator bool() const noexcept { return has_value(); }
  auto value() -> Value& { return std::get<1>(state_); }
  auto value() const -> Value const& { return std::get<1>(state_); }
  auto operator*() -> Value& { return value(); }
  auto operator*() const -> Value const& { return value(); }

  auto operator=(std::optional<Value>&& incoming) noexcept -> FunctionSlot&
  {
    if (incoming)
      state_.template emplace<1>(std::move(*incoming));
    else
      reset();
    return *this;
  }

  void reset() noexcept { state_.template emplace<0>(); }
  void defer(Pending incoming) noexcept { state_.template emplace<2>(std::move(incoming)); }
  [[nodiscard]] auto pending() const noexcept -> Pending const* { return std::get_if<2>(&state_); }

private:
  std::variant<std::monostate, Value, Pending> state_;
};

}  // namespace GD::Bc::Details
#endif
