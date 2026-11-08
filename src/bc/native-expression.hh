/** Bounded integer-only interface to cached native leaf expressions.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_NATIVE_EXPRESSION_HH
#define GD_BC_NATIVE_EXPRESSION_HH

#include <cstddef>
#include <cstdint>
#include <memory>
#include <span>

namespace GD::Bc::Details {
struct NativeExpressionOp
{
  enum class Kind : std::uint8_t { argument, constant, add, multiply };
  Kind kind;
  std::uint8_t left = 0;
  std::uint8_t right = 0;
  std::uint64_t constant = 0;
};

/** Immutable executable ownership; this interface contains no Number objects.
 * Values and intermediates are nonnegative signed-64 integers. Overflow,
 * malformed plans, register pressure and allocation/protection failure decline
 * native execution. The caller retains all observable state until success.
 */
class NativeExpression
{
public:
  static auto compile(std::span<NativeExpressionOp const> operations, std::uint8_t result)
    -> std::shared_ptr<NativeExpression const>;
  ~NativeExpression();
  NativeExpression(NativeExpression const&) = delete;
  auto operator=(NativeExpression const&) -> NativeExpression& = delete;
  auto execute(std::uint64_t argument, std::uint64_t& result) const noexcept -> bool;
  auto code_bytes() const noexcept -> std::span<std::uint8_t const>;

private:
  NativeExpression() = default;
  void* memory_ = nullptr;
  std::size_t size_ = 0;
  std::size_t code_size_ = 0;
};
}  // namespace GD::Bc::Details
#endif
