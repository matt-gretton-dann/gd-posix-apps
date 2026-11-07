/** Bounded portable integer execution for independently checked scalar streams.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_PORTABLE_SCALAR_HH_INCLUDED
#define GD_BC_PORTABLE_SCALAR_HH_INCLUDED

#include <array>
#include <bitset>
#include <cstdint>
#include <optional>

#include "bc.hh"

namespace GD::Bc::Details {
using ScalarInterruptCheck = bool (*)() noexcept;
using ScalarContextInterruptCheck = bool (*)(void const*) noexcept;
using ScalarValues = std::array<std::uint64_t, Letter::count_>;

class PortableScalarPlan
{
public:
  static constexpr unsigned instruction_limit = 64;
  static constexpr std::uint64_t backedge_limit = 4'000'000;

  [[nodiscard]] auto variables() const -> std::bitset<Letter::count_> { return variables_; }

  // All execution state is private. Failure returns no values, even after
  // earlier stores; the caller can replay the unchanged ordinary VM state.
  // Fuel is always capped at backedge_limit; a smaller bound supports
  // deterministic exhaustion/polling tests.
  [[nodiscard]] auto run(ScalarInterruptCheck interrupted = nullptr,
                         std::uint64_t fuel = backedge_limit) const noexcept
    -> std::optional<ScalarValues>;

  // Caller-owned context avoids hidden global/TLS callback state across an
  // opaque backend boundary. The legacy no-context entry remains available.
  [[nodiscard]] auto run_with_context(ScalarContextInterruptCheck interrupted, void const* context,
                                      std::uint64_t fuel = backedge_limit) const noexcept
    -> std::optional<ScalarValues>;

private:
  enum class Op : std::uint8_t { number, location, load, store, add, less, branch, branch_zero };
  struct Operation
  {
    Op opcode{};
    std::uint8_t first{};
    std::uint8_t second{};
    std::uint64_t value{};
  };
  PortableScalarPlan() = default;
  std::array<Operation, instruction_limit> code_{};
  std::bitset<Letter::count_> variables_;
  unsigned size_{};

  friend auto plan_portable_scalar(Instructions const&) -> std::optional<PortableScalarPlan>;
};

// Validate operand schemas, bounds, types and dominance before lowering.
// The resulting plan owns its operands and outlives changes to source code.
// Decimal input and zero, scale-zero referenced scalars are caller guards;
// this component neither reads nor publishes public VM state.
auto plan_portable_scalar(Instructions const& code) -> std::optional<PortableScalarPlan>;
}  // namespace GD::Bc::Details

#endif
