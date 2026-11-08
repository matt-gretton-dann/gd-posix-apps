/** Experimental Apple Arm64 scalar emitter.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "native-scalar.hh"

#include <new>

#include <charconv>
#include <cstring>
#include <unistd.h>
#include <vector>

#include <sys/mman.h>

namespace GD::Bc::Details {
namespace {
using Op = Instruction::Opcode;
using Index = Instruction::Index;
using Word = std::uint32_t;
constexpr Index result_slots = native_scalar_result_slots;
constexpr unsigned stack_bytes = 768;
constexpr unsigned scratch = 8;
constexpr unsigned second = 9;
constexpr unsigned sp = 31;
constexpr unsigned output = 0;
constexpr unsigned fuel = 1;
constexpr unsigned zero = 31;
constexpr unsigned callback = 2;
constexpr unsigned link = 30;
constexpr Index output_slot = result_slots + Letter::count_;
constexpr Index callback_slot = output_slot + 1;
constexpr Index fuel_slot = callback_slot + 1;
constexpr Index link_slot = fuel_slot + 1;
static_assert((link_slot + 1) * sizeof(std::uint64_t) <= stack_bytes);
constexpr std::uint64_t max_backedges = 4'000'000;
constexpr Index failure_label = result_slots + 1;

auto relative(Index pc, Instruction::Operand const& operand) -> Index
{
  return pc + static_cast<Index>(std::get<Instruction::Offset>(operand));
}

auto scalar(Instruction const& instruction) -> unsigned
{
  return static_cast<unsigned>(std::get<Variable>(instruction.op1()).get());
}

struct Fixup
{
  Index word;
  Index target;
  unsigned width;
  unsigned shift;
};

struct Emitter
{
  std::vector<Word> words;
  std::array<Index, result_slots + 2> labels{};
  std::vector<Fixup> fixups;

  void memory(bool load, unsigned reg, unsigned base, Index slot)
  {
    // LDR/STR (unsigned immediate), scaled by the 64-bit access width.
    words.push_back((load ? 0xf9400000U : 0xf9000000U) | (static_cast<Word>(slot) << 10) |
                    (base << 5) | reg);
  }

  void immediate(unsigned reg, std::uint64_t value)
  {
    // MOVZ followed by the nonzero MOVK halfwords.
    words.push_back(0xd2800000U | (static_cast<Word>(value & 0xffffU) << 5) | reg);
    for (unsigned part = 1; part < 4; ++part) {
      auto const chunk = static_cast<Word>((value >> (16 * part)) & 0xffffU);
      if (chunk)
        words.push_back(0xf2800000U | (part << 21) | (chunk << 5) | reg);
    }
  }

  void branch(Index target, Word opcode, unsigned width, unsigned shift)
  {
    fixups.push_back({words.size(), target, width, shift});
    words.push_back(opcode);
  }

  void finish()
  {
    for (auto const& f : fixups) {
      // The instruction cap makes all displacements fit even the shorter
      // conditional encoding. Offsets are relative to the branch word.
      auto const delta = static_cast<Word>(labels[f.target] - f.word);
      words[f.word] |= (delta & ((Word{1} << f.width) - 1U)) << f.shift;
    }
  }

  void poll_interrupt()
  {
    // Backedges check the flag at multiples of 1,024 remaining steps.
    // Preserve the live argument registers across the AAPCS64 call.
    words.push_back(0xf240243fU);  // TST x1, #0x3ff
    auto const skip = words.size();
    words.push_back(0x54000001U);  // B.NE past the callback
    memory(false, fuel, sp, fuel_slot);
    memory(true, second, sp, callback_slot);
    words.push_back(0xd63f0120U);               // BLR x9
    branch(failure_label, 0x35000000U, 19, 5);  // CBNZ w0, failure
    memory(true, output, sp, output_slot);
    memory(true, fuel, sp, fuel_slot);
    words[skip] |= static_cast<Word>(words.size() - skip) << 5;
  }

  void return_status(unsigned status)
  {
    memory(true, link, sp, link_slot);
    immediate(output, status);
    words.push_back(0x910003ffU | (stack_bytes << 10));  // ADD sp, sp, #frame
    words.push_back(0xd65f03c0U);                        // RET
  }
};

class CodePage
{
public:
  explicit CodePage(std::vector<Word> const& words)
  {
    auto const page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0)
      return;
    size_ = static_cast<std::size_t>(page_size);
    auto const bytes = words.size() * sizeof(Word);
    if (bytes > size_)
      return;
    memory_ = mmap(nullptr, size_, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
    if (memory_ == MAP_FAILED)
      return;
    std::memcpy(memory_, words.data(), bytes);
    ready_ = mprotect(memory_, size_, PROT_READ | PROT_EXEC) == 0;
    if (ready_)
      __builtin___clear_cache(static_cast<char*>(memory_), static_cast<char*>(memory_) + bytes);
  }

  ~CodePage()
  {
    if (memory_ != MAP_FAILED)
      munmap(memory_, size_);
  }
  CodePage(CodePage const&) = delete;
  auto operator=(CodePage const&) -> CodePage& = delete;

  auto run(std::uint64_t* output_values, NativeInterruptCheck interrupted) const -> bool
  {
    using Function = unsigned (*)(std::uint64_t*, std::uint64_t, NativeInterruptCheck);
    return ready_ &&
           reinterpret_cast<Function>(memory_)(output_values, max_backedges, interrupted) == 0;
  }

private:
  void* memory_{MAP_FAILED};
  std::size_t size_{};
  bool ready_{};
};
}  // namespace

auto run_native_scalar(Instructions const& code, std::bitset<Letter::count_> variables,
                       NativeInterruptCheck interrupted) -> std::optional<NativeScalarResult>
try {
  Emitter emit;
  NativeScalarResult result{};
  emit.words.push_back(0xd10003ffU | (stack_bytes << 10));  // SUB sp, sp, #frame
  emit.memory(false, output, sp, output_slot);
  emit.memory(false, callback, sp, callback_slot);
  emit.memory(false, link, sp, link_slot);
  for (unsigned variable = 0; variable < Letter::count_; ++variable) {
    if (variables.test(variable))
      emit.memory(false, zero, sp, result_slots + variable);
  }
  for (Index pc = 0; pc < code.size(); ++pc) {
    emit.labels[pc] = emit.words.size();
    auto const& instruction = code[pc];
    switch (instruction.opcode()) {
    case Op::number: {
      auto const& text = std::get<std::string>(instruction.op1());
      std::uint64_t value{};
      auto const [end, error] = std::from_chars(text.data(), text.data() + text.size(), value);
      if (error != std::errc{} || end != text.data() + text.size())
        return {};
      emit.immediate(scratch, value);
      emit.memory(false, scratch, sp, pc);
      break;
    }
    case Op::load:
      emit.memory(true, scratch, sp, result_slots + scalar(code[relative(pc, instruction.op1())]));
      emit.memory(false, scratch, sp, pc);
      break;
    case Op::store:
      emit.memory(true, scratch, sp, relative(pc, instruction.op2()));
      emit.memory(false, scratch, sp, result_slots + scalar(code[relative(pc, instruction.op1())]));
      break;
    case Op::add:
    case Op::less_than:
      emit.memory(true, scratch, sp, relative(pc, instruction.op1()));
      emit.memory(true, second, sp, relative(pc, instruction.op2()));
      if (instruction.opcode() == Op::add) {
        emit.words.push_back(0xab090108U);               // ADDS x8, x8, x9
        emit.branch(failure_label, 0x54000002U, 19, 5);  // B.HS (carry)
      }
      else {
        emit.words.push_back(0xeb09011fU);  // CMP x8, x9
        emit.words.push_back(0x9a9f27e8U);  // CSET x8, LO
      }
      emit.memory(false, scratch, sp, pc);
      break;
    case Op::branch: {
      auto const target = relative(pc, instruction.op1());
      if (target <= pc) {
        emit.words.push_back(0xf1000400U | (fuel << 5) | fuel);  // SUBS x1, x1, #1
        emit.branch(failure_label, 0x54000003U, 19, 5);          // B.LO (exhausted)
        emit.poll_interrupt();
      }
      emit.branch(target, 0x14000000U, 26, 0);
      break;
    }
    case Op::branch_zero:
      emit.memory(true, scratch, sp, relative(pc, instruction.op1()));
      emit.branch(relative(pc, instruction.op2()), 0xb4000000U | scratch, 19, 5);
      break;
    case Op::variable:
      break;
    default:
      return {};
    }
  }
  emit.labels[code.size()] = emit.words.size();
  for (unsigned variable = 0; variable < Letter::count_; ++variable) {
    if (variables.test(variable)) {
      emit.memory(true, scratch, sp, result_slots + variable);
      emit.memory(false, scratch, output, variable);
    }
  }
  emit.return_status(0);
  emit.labels[failure_label] = emit.words.size();
  emit.return_status(1);
  emit.finish();
  if (!CodePage(emit.words).run(result.data(), interrupted))
    return {};
  return result;
}
catch (std::bad_alloc const&) {
  // Emitter storage is optional; no VM state has been published. Let the
  // ordinary interpreter continue when compilation cannot allocate scratch.
  return {};
}
}  // namespace GD::Bc::Details
