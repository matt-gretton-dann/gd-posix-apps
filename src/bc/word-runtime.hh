/** Ordinary-context ownership for the experimental recursive word tier.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_WORD_RUNTIME_HH
#define GD_BC_WORD_RUNTIME_HH

#include <array>
#include <memory>

#include "bc.hh"
#include "word-function.hh"

namespace GD::Bc::Details {
class WordFunctions
{
public:
  enum class Outcome { declined, failed, ready };
  static auto create() -> std::unique_ptr<WordFunctions>;
  ~WordFunctions();
  WordFunctions(WordFunctions const&) = delete;
  auto operator=(WordFunctions const&) -> WordFunctions& = delete;

  // Keep conversion and speculative Number locals out of the routing frame.
  // Exact scale-zero signed words need no new Number conversion method.
  static auto argument(Number const& value) -> std::optional<std::int64_t>;
  // The VM owns allocation-free warmup counting before creating this owner.
  auto try_call(unsigned index, Instructions const& source, VariableMask locals, Letter self,
                std::int64_t argument, bool (*interrupted)() noexcept) -> Outcome;
  void reset(unsigned index);
  [[nodiscard]] auto rejected(unsigned index) const -> bool { return entries_.at(index).rejected; }
  void discard_result() noexcept { result_.reset(); }
  auto take_result() noexcept -> Number;

private:
  WordFunctions() = default;
  struct Entry
  {
    bool rejected = false;
    std::optional<Word::Program> program;
    std::optional<std::int64_t> failed_argument;
  };
  auto number(Word::Value value) const -> Number;
  auto publish(Number value, Word::Value& target, std::size_t available) -> bool;
  auto binary(Word::Opcode op, Word::Value left, Word::Value right, Word::Value& target,
              std::size_t available) -> bool;
  auto negate(Word::Value input, Word::Value& target, std::size_t available) -> bool;
  void clear_attempt() noexcept;

  std::array<Entry, Letter::count_> entries_{};
  Word::Scratch scratch_;
  Word::Storage<Number, Profile::AllocationKind::frames> spills_;
  std::size_t spill_bytes_ = 0;
  std::optional<Number> result_;
  bool (*interrupted_)() noexcept = nullptr;
};
}  // namespace GD::Bc::Details
#endif
