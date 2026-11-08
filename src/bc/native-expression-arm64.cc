/** Checked, stackless Apple ARM64 expressions with no calls or nonvolatile writes.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <array>
#include <cstring>
#include <vector>

#if !defined(__APPLE__) || !defined(__aarch64__) || defined(__arm64e__)
#  error Native leaf expressions require the ordinary Apple ARM64 ABI
#endif

#include <unistd.h>

#include "native-expression-plan.hh"
#include "native-expression.hh"
#include <sys/mman.h>

namespace GD::Bc::Details {
namespace {
using Kind = NativeExpressionOp::Kind;
constexpr unsigned argument_register = 0;  // X0, borrowed until return.
constexpr unsigned output_register = 1;    // X1, written only after success.
// X9-X14 are volatile under AAPCS64. X14 is reserved for the high product.
// No stack, frame pointer, link register or platform register is modified.
constexpr std::array<unsigned, 5> temporary_registers{9, 10, 11, 12, 13};
struct Emitter
{
  using Word = std::uint32_t;
  struct Failure
  {
    std::size_t at;
    unsigned width;
  };
  std::vector<Word> words;
  std::vector<Failure> failures;
  void start(unsigned argument)
  {
    words.push_back(0xd503245fU);  // BTI c, including as a hint without BTI.
    negative(argument);
  }
  void failure(Word opcode, unsigned width)
  {
    failures.push_back({words.size(), width});
    words.push_back(opcode);
  }
  void negative(unsigned reg)
  {
    failure(0xb7f80000U | reg, 14);  // TBNZ Xreg, #63, failure.
  }
  void move(unsigned to, unsigned from)
  {
    if (to != from)
      words.push_back(0xaa0003e0U | (from << 16) | to);  // ORR Xto, XZR, Xfrom.
  }
  void constant(unsigned to, std::uint64_t value)
  {
    words.push_back(0xd2800000U | (static_cast<Word>(value & 0xffffU) << 5) | to);
    for (unsigned i = 1; i != 4; ++i) {
      auto const part = static_cast<Word>((value >> (16 * i)) & 0xffffU);
      if (part != 0)
        words.push_back(0xf2800000U | (i << 21) | (part << 5) | to);
    }
  }
  void binary(Kind kind, unsigned to, unsigned from)
  {
    if (kind == Kind::add) {
      words.push_back(0xab000000U | (from << 16) | (to << 5) | to);  // ADDS.
      failure(0x54000006U, 19);  // B.VS: nonnegative signed sum overflow.
    }
    else {
      words.push_back(0x9bc07c00U | (from << 16) | (to << 5) | 14U);  // UMULH X14.
      failure(0xb5000000U | 14U, 19);  // CBNZ X14: product exceeds 64 bits.
      words.push_back(0x9b007c00U | (from << 16) | (to << 5) | to);  // MUL.
      negative(to);  // The remaining high signed bit also leaves the domain.
    }
  }
  void finish(unsigned result)
  {
    words.push_back(0xf9000000U | (output_register << 5) | result);  // STR [X1].
    words.push_back(0x52800020U);                                    // MOV W0, #1.
    words.push_back(0xd65f03c0U);  // RET, with the caller's LR untouched.
    auto const target = words.size();
    words.push_back(0x52800000U);  // MOV W0, #0.
    words.push_back(0xd65f03c0U);
    for (auto const& f : failures) {
      // The operation cap keeps every forward displacement in both ranges.
      auto const delta = static_cast<Word>(target - f.at);
      words[f.at] |= (delta & ((Word{1} << f.width) - 1U)) << 5;
    }
  }
};
}  // namespace

auto NativeExpression::compile(std::span<NativeExpressionOp const> operations, std::uint8_t result)
  -> std::shared_ptr<NativeExpression const>
try {
  auto generated =
    emit_native_expression<Emitter, argument_register>(operations, result, temporary_registers);
  if (!generated)
    return {};
  auto const& emit = *generated;
  auto program = std::shared_ptr<NativeExpression>(new NativeExpression);
  auto const page_size = sysconf(_SC_PAGESIZE);
  auto const byte_count = emit.words.size() * sizeof(std::uint32_t);
  if (page_size <= 0 || byte_count > static_cast<std::size_t>(page_size))
    return {};
  program->size_ = static_cast<std::size_t>(page_size);
  program->memory_ =
    mmap(nullptr, program->size_, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (program->memory_ == MAP_FAILED) {
    program->memory_ = nullptr;
    return {};
  }
  std::memcpy(program->memory_, emit.words.data(), byte_count);
  if (mprotect(program->memory_, program->size_, PROT_READ | PROT_EXEC) != 0)
    return {};
  __builtin___clear_cache(static_cast<char*>(program->memory_),
                          static_cast<char*>(program->memory_) + byte_count);
  program->code_size_ = byte_count;
  return program;
}
catch (std::bad_alloc const&) {
  return {};
}

NativeExpression::~NativeExpression()
{
  if (memory_ != nullptr) {
    munmap(memory_, size_);
  }
}

auto NativeExpression::execute(std::uint64_t argument, std::uint64_t& result) const noexcept -> bool
{
  using Entry = bool (*)(std::uint64_t, std::uint64_t*);
  return reinterpret_cast<Entry>(memory_)(argument, &result);
}
auto NativeExpression::code_bytes() const noexcept -> std::span<std::uint8_t const>
{
  return {static_cast<std::uint8_t const*>(memory_), code_size_};
}
}  // namespace GD::Bc::Details
