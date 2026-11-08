/** Experimental x86-64 lowering for bounded verified scalar streams.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <charconv>
#include <cstring>
#include <vector>

#include "native-scalar.hh"

#if defined(_WIN32)
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  ifndef NOMINMAX
#    define NOMINMAX
#  endif
#  include <windows.h>
#else
#  include <unistd.h>

#  include <sys/mman.h>
#endif

namespace GD::Bc::Details {
namespace {
using Op = Instruction::Opcode;
using Index = Instruction::Index;
constexpr Index result_slots = native_scalar_result_slots;
constexpr unsigned sp = 4;
constexpr unsigned scratch = 0;  // RAX
constexpr unsigned second = 10;  // R10; volatile under both ABIs.
#if defined(_WIN32)
constexpr unsigned output = 1;    // RCX
constexpr unsigned fuel = 2;      // RDX
constexpr unsigned callback = 8;  // R8
#else
constexpr unsigned output = 7;    // RDI
constexpr unsigned fuel = 6;      // RSI
constexpr unsigned callback = 2;  // RDX
#endif
// Reserve the Windows callback's home area on both targets. Entry RSP is
// eight modulo sixteen; subtracting this frame aligns every callback call.
constexpr unsigned shadow_bytes = 32;
constexpr unsigned stack_bytes = 808;
constexpr Index output_slot = result_slots + Letter::count_;
constexpr Index callback_slot = output_slot + 1;
constexpr Index fuel_slot = callback_slot + 1;
static_assert(shadow_bytes + (fuel_slot + 1) * sizeof(std::uint64_t) <= stack_bytes);
static_assert(stack_bytes % 16 == 8);
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
  Index displacement;
  Index target;
};

struct Emitter
{
  std::vector<std::uint8_t> bytes;
  std::array<Index, result_slots + 2> labels{};
  std::vector<Fixup> fixups;

  void byte(unsigned value) { bytes.push_back(static_cast<std::uint8_t>(value)); }
  void word(std::uint32_t value)
  {
    for (unsigned i = 0; i != 4; ++i)
      byte(value >> (8 * i));
  }
  void memory(bool load, unsigned reg, unsigned base, Index slot)
  {
    byte(0x48U | ((reg >> 3) << 2) | (base >> 3));
    byte(load ? 0x8bU : 0x89U);
    byte(0x80U | ((reg & 7U) << 3) | (base & 7U));
    if ((base & 7U) == sp)
      byte(0x24);  // No index, RSP base.
    auto const offset = slot * sizeof(std::uint64_t) + (base == sp ? shadow_bytes : 0);
    word(static_cast<std::uint32_t>(offset));
  }
  void immediate(std::uint64_t value)
  {
    byte(0x48);
    byte(0xb8);  // MOVABS RAX, imm64
    word(static_cast<std::uint32_t>(value));
    word(static_cast<std::uint32_t>(value >> 32));
  }
  // Conditional encodings use the low nibble of Intel's condition number.
  auto jump(unsigned condition) -> Index
  {
    byte(0x0f);
    byte(0x80U | condition);
    auto const displacement = bytes.size();
    word(0);
    return displacement;
  }
  void patch(Index displacement, Index target)
  {
    auto const delta =
      static_cast<std::int64_t>(target) - static_cast<std::int64_t>(displacement + 4);
    // Sixty-four source instructions keep every displacement well inside
    // signed rel32. Encode the negative representation without signed shifts.
    auto const encoded = static_cast<std::uint32_t>(delta);
    for (unsigned i = 0; i != 4; ++i)
      bytes[displacement + i] = static_cast<std::uint8_t>(encoded >> (8 * i));
  }
  void branch(Index target, unsigned condition) { fixups.push_back({jump(condition), target}); }
  void branch(Index target)
  {
    byte(0xe9);
    fixups.push_back({bytes.size(), target});
    word(0);
  }
  void finish()
  {
    for (auto const& f : fixups)
      patch(f.displacement, labels[f.target]);
  }
  void poll_interrupt()
  {
    byte(0x48);
    byte(0xf7);
    byte(0xc0U | fuel);
    word(0x3ff);                // TEST fuel, 1023
    auto const skip = jump(5);  // JNE
    memory(false, fuel, sp, fuel_slot);
    memory(true, second, sp, callback_slot);
    byte(0x41);
    byte(0xff);
    byte(0xd2);  // CALL R10
    // C++ bool is returned in AL; the remainder of RAX is unspecified.
    byte(0x84);
    byte(0xc0);  // TEST AL, AL
    branch(failure_label, 5);
    memory(true, output, sp, output_slot);
    memory(true, fuel, sp, fuel_slot);
    patch(skip, bytes.size());
  }
  void return_status(unsigned status)
  {
    byte(0xb8);
    word(status);  // MOV EAX, status
    byte(0x48);
    byte(0x81);
    byte(0xc4);
    word(stack_bytes);  // ADD RSP, frame
    byte(0xc3);
  }
};

class CodePage
{
public:
  explicit CodePage(std::vector<std::uint8_t> const& code)
  {
#if defined(_WIN32)
    SYSTEM_INFO info{};
    GetSystemInfo(&info);
    size_ = info.dwPageSize;
    auto const unwind_offset = (code.size() + 3U) & ~std::size_t{3};
    if (size_ == 0 || unwind_offset + 8 > size_)
      return;
    memory_ = VirtualAlloc(nullptr, size_, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    if (memory_ == nullptr)
      return;
    std::memcpy(memory_, code.data(), code.size());
    // Version 1, no handlers, eleven-byte prologue (ENDBR64 + SUB RSP),
    // two unwind slots: UWOP_ALLOC_LARGE, allocation size / 8.
    std::uint8_t const unwind[]{1,
                                11,
                                2,
                                0,
                                11,
                                1,
                                static_cast<std::uint8_t>(stack_bytes / 8),
                                static_cast<std::uint8_t>((stack_bytes / 8) >> 8)};
    std::memcpy(static_cast<char*>(memory_) + unwind_offset, unwind, sizeof(unwind));
    DWORD old_protection{};
    if (!VirtualProtect(memory_, size_, PAGE_EXECUTE_READ, &old_protection) ||
        !FlushInstructionCache(GetCurrentProcess(), memory_, code.size()))
      return;
    function_.BeginAddress = 0;
    function_.EndAddress = static_cast<DWORD>(code.size());
    function_.UnwindData = static_cast<DWORD>(unwind_offset);
    registered_ = RtlAddFunctionTable(&function_, 1, reinterpret_cast<DWORD64>(memory_)) != 0;
    ready_ = registered_;
#else
    auto const page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0)
      return;
    size_ = static_cast<std::size_t>(page_size);
    if (code.size() > size_)
      return;
    auto* memory = mmap(nullptr, size_, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
    if (memory == MAP_FAILED)
      return;
    memory_ = memory;
    std::memcpy(memory_, code.data(), code.size());
    ready_ = mprotect(memory_, size_, PROT_READ | PROT_EXEC) == 0;
    if (ready_)
      __builtin___clear_cache(static_cast<char*>(memory_),
                              static_cast<char*>(memory_) + code.size());
#endif
  }
  ~CodePage()
  {
#if defined(_WIN32)
    if (registered_)
      RtlDeleteFunctionTable(&function_);
    if (memory_ != nullptr)
      VirtualFree(memory_, 0, MEM_RELEASE);
#else
    if (memory_ != nullptr)
      munmap(memory_, size_);
#endif
  }
  CodePage(CodePage const&) = delete;
  auto operator=(CodePage const&) -> CodePage& = delete;
  auto run(std::uint64_t* values, NativeInterruptCheck interrupted) const -> bool
  {
    using Function = unsigned (*)(std::uint64_t*, std::uint64_t, NativeInterruptCheck);
    return ready_ && reinterpret_cast<Function>(memory_)(values, max_backedges, interrupted) == 0;
  }

private:
  void* memory_ = nullptr;
  std::size_t size_ = 0;
  bool ready_ = false;
#if defined(_WIN32)
  RUNTIME_FUNCTION function_{};
  bool registered_ = false;
#endif
};
}  // namespace

auto run_native_scalar(Instructions const& code, std::bitset<Letter::count_> variables,
                       NativeInterruptCheck interrupted) -> std::optional<NativeScalarResult>
{
  Emitter emit;
  NativeScalarResult result{};
  // ENDBR64 permits an indirect entry with CET IBT; it is a no-op otherwise.
  emit.byte(0xf3);
  emit.byte(0x0f);
  emit.byte(0x1e);
  emit.byte(0xfa);
  emit.byte(0x48);
  emit.byte(0x81);
  emit.byte(0xec);
  emit.word(stack_bytes);
  emit.memory(false, output, sp, output_slot);
  emit.memory(false, callback, sp, callback_slot);
  emit.byte(0x31);
  emit.byte(0xc0);  // XOR EAX, EAX
  for (unsigned v = 0; v < Letter::count_; ++v)
    if (variables.test(v))
      emit.memory(false, scratch, sp, result_slots + v);
  for (Index pc = 0; pc < code.size(); ++pc) {
    emit.labels[pc] = emit.bytes.size();
    auto const& instruction = code[pc];
    switch (instruction.opcode()) {
    case Op::number: {
      auto const& text = std::get<std::string>(instruction.op1());
      std::uint64_t value{};
      auto const [end, error] = std::from_chars(text.data(), text.data() + text.size(), value);
      if (error != std::errc{} || end != text.data() + text.size())
        return {};
      emit.immediate(value);
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
        emit.byte(0x4c);
        emit.byte(0x01);
        emit.byte(0xd0);                // ADD RAX, R10
        emit.branch(failure_label, 2);  // JB: unsigned carry
      }
      else {
        emit.byte(0x4c);
        emit.byte(0x39);
        emit.byte(0xd0);  // CMP RAX, R10
        emit.byte(0x0f);
        emit.byte(0x92);
        emit.byte(0xc0);  // SETB AL
        emit.byte(0x48);
        emit.byte(0x0f);
        emit.byte(0xb6);
        emit.byte(0xc0);  // MOVZX RAX, AL
      }
      emit.memory(false, scratch, sp, pc);
      break;
    case Op::branch: {
      auto const target = relative(pc, instruction.op1());
      if (target <= pc) {
        emit.byte(0x48);
        emit.byte(0x83);
        emit.byte(0xe8U | fuel);
        emit.byte(1);  // SUB fuel, 1
        emit.branch(failure_label, 2);
        emit.poll_interrupt();
      }
      emit.branch(target);
      break;
    }
    case Op::branch_zero:
      emit.memory(true, scratch, sp, relative(pc, instruction.op1()));
      emit.byte(0x48);
      emit.byte(0x85);
      emit.byte(0xc0);  // TEST RAX, RAX
      emit.branch(relative(pc, instruction.op2()), 4);
      break;
    case Op::variable:
      break;
    default:
      return {};
    }
  }
  emit.labels[code.size()] = emit.bytes.size();
  for (unsigned v = 0; v < Letter::count_; ++v) {
    if (variables.test(v)) {
      emit.memory(true, scratch, sp, result_slots + v);
      emit.memory(false, scratch, output, v);
    }
  }
  emit.return_status(0);
  emit.labels[failure_label] = emit.bytes.size();
  emit.return_status(1);
  emit.finish();
  if (!CodePage(emit.bytes).run(result.data(), interrupted))
    return {};
  return result;
}
}  // namespace GD::Bc::Details
