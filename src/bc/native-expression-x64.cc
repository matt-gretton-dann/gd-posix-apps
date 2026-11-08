/** Checked, stackless x86-64 expressions with no calls or nonvolatile writes.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <array>
#include <cstring>
#include <limits>
#include <utility>
#include <vector>

#if (!defined(__x86_64__) && !defined(_M_X64)) || defined(_M_ARM64EC)
#  error Native leaf expressions require the x86-64 ABI
#endif

#include "native-expression.hh"

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
using Kind = NativeExpressionOp::Kind;
constexpr unsigned maximum_operations = 64;
#if defined(_WIN32)
constexpr unsigned argument_register = 1;  // RCX
constexpr unsigned output_register = 2;    // RDX
#else
constexpr unsigned argument_register = 7;  // RDI
constexpr unsigned output_register = 6;    // RSI
#endif
// Every register here is volatile under both ABIs. Neither input register is
// allocated, RSP is never changed, and generated code never calls a helper.
constexpr std::array<unsigned, 5> temporary_registers{0, 8, 9, 10, 11};
struct Emitter
{
  std::vector<std::uint8_t> bytes;
  std::vector<std::size_t> failures;
  void byte(unsigned value) { bytes.push_back(static_cast<std::uint8_t>(value)); }
  void word(std::uint32_t value)
  {
    for (unsigned i = 0; i != 4; ++i)
      byte(value >> (8 * i));
  }
  void conditional_failure(unsigned condition)
  {
    byte(0x0f);
    byte(0x80U | condition);
    failures.push_back(bytes.size());
    word(0);
  }
  void move(unsigned to, unsigned from)
  {
    if (to == from)
      return;
    byte(0x48U | ((from >> 3) << 2) | (to >> 3));
    byte(0x89);
    byte(0xc0U | ((from & 7U) << 3) | (to & 7U));
  }
  void constant(unsigned to, std::uint64_t value)
  {
    byte(0x48U | (to >> 3));
    byte(0xb8U | (to & 7U));
    word(static_cast<std::uint32_t>(value));
    word(static_cast<std::uint32_t>(value >> 32));
  }
  void binary(Kind kind, unsigned to, unsigned from)
  {
    if (kind == Kind::add) {
      byte(0x48U | ((from >> 3) << 2) | (to >> 3));
      byte(0x01);
      byte(0xc0U | ((from & 7U) << 3) | (to & 7U));
    }
    else {
      byte(0x48U | ((to >> 3) << 2) | (from >> 3));
      byte(0x0f);
      byte(0xaf);
      byte(0xc0U | ((to & 7U) << 3) | (from & 7U));
    }
    // Inputs are nonnegative signed-64 values, so signed overflow is exactly
    // the boundary at which this speculative integer domain must decline.
    conditional_failure(0);  // JO
  }
  void finish(unsigned result)
  {
    byte(0x48U | ((result >> 3) << 2));
    byte(0x89);
    byte(((result & 7U) << 3) | output_register);  // MOV [output], result
    byte(0xb8);
    word(1);
    byte(0xc3);
    auto const failure = bytes.size();
    byte(0x31);
    byte(0xc0);  // XOR EAX, EAX
    byte(0xc3);
    for (auto displacement : failures) {
      auto delta = static_cast<std::uint32_t>(failure - (displacement + 4));
      for (unsigned i = 0; i != 4; ++i)
        bytes[displacement + i] = static_cast<std::uint8_t>(delta >> (8 * i));
    }
  }
};
}  // namespace

auto NativeExpression::compile(std::span<NativeExpressionOp const> operations, std::uint8_t result)
  -> std::shared_ptr<NativeExpression const>
try {
  if (operations.empty() || operations.size() > maximum_operations || result >= operations.size())
    return {};
  std::array<unsigned, maximum_operations> last_use{};
  for (unsigned i = 0; i != operations.size(); ++i) {
    auto const& op = operations[i];
    switch (op.kind) {
    case Kind::argument:
      break;
    case Kind::constant:
      if (op.constant > static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()))
        return {};
      break;
    case Kind::add:
    case Kind::multiply:
      if (op.left >= i || op.right >= i)
        return {};
      last_use[op.left] = i;
      last_use[op.right] = i;
      break;
    default:
      return {};
    }
  }
  last_use[result] = static_cast<unsigned>(operations.size());
  Emitter emit;
  for (auto value : {0xf3U, 0x0fU, 0x1eU, 0xfaU})
    emit.byte(value);  // ENDBR64, including on processors where it is a no-op.
  emit.byte(0x48);
  emit.byte(0x85);
  emit.byte(0xc0U | (argument_register << 3) | argument_register);
  emit.conditional_failure(8);  // JS: reject unsigned arguments above INT64_MAX.
  std::array<int, maximum_operations> registers;
  registers.fill(-1);
  std::array<bool, 16> occupied{};
  auto free_register = [&]() -> int {
    for (auto reg : temporary_registers)
      if (!occupied[reg])
        return static_cast<int>(reg);
    return -1;
  };
  for (unsigned i = 0; i != operations.size(); ++i) {
    auto const& op = operations[i];
    int destination = -1;
    if (op.kind == Kind::argument) {
      destination = static_cast<int>(argument_register);
    }
    else if (op.kind == Kind::constant) {
      destination = free_register();
      if (destination < 0)
        return {};
      emit.constant(static_cast<unsigned>(destination), op.constant);
    }
    else {
      auto left = registers[op.left];
      auto right = registers[op.right];
      if (left < 0 || right < 0)
        return {};
      if (left != static_cast<int>(argument_register) && last_use[op.left] == i)
        destination = left;
      else if (right != static_cast<int>(argument_register) && last_use[op.right] == i) {
        destination = right;
        std::swap(left, right);  // Both supported operations are commutative.
      }
      else
        destination = free_register();
      if (destination < 0)
        return {};
      emit.move(static_cast<unsigned>(destination), static_cast<unsigned>(left));
      emit.binary(op.kind, static_cast<unsigned>(destination), static_cast<unsigned>(right));
    }
    for (unsigned old = 0; old < i; ++old) {
      if (last_use[old] == i && registers[old] != static_cast<int>(argument_register)) {
        if (registers[old] >= 0 && registers[old] != destination)
          occupied[static_cast<unsigned>(registers[old])] = false;
        registers[old] = -1;
      }
    }
    registers[i] = destination;
    if (destination != static_cast<int>(argument_register))
      occupied[static_cast<unsigned>(destination)] = last_use[i] > i;
  }
  if (registers[result] < 0)
    return {};
  emit.finish(static_cast<unsigned>(registers[result]));
  auto program = std::shared_ptr<NativeExpression>(new NativeExpression);
#if defined(_WIN32)
  SYSTEM_INFO info{};
  GetSystemInfo(&info);
  program->size_ = info.dwPageSize;
  if (program->size_ == 0 || emit.bytes.size() > program->size_)
    return {};
  program->memory_ =
    VirtualAlloc(nullptr, program->size_, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  if (program->memory_ == nullptr)
    return {};
#else
  auto const page_size = sysconf(_SC_PAGESIZE);
  if (page_size <= 0 || emit.bytes.size() > static_cast<std::size_t>(page_size))
    return {};
  program->size_ = static_cast<std::size_t>(page_size);
  program->memory_ =
    mmap(nullptr, program->size_, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (program->memory_ == MAP_FAILED) {
    program->memory_ = nullptr;
    return {};
  }
#endif
  std::memcpy(program->memory_, emit.bytes.data(), emit.bytes.size());
#if defined(_WIN32)
  DWORD previous{};
  if (!VirtualProtect(program->memory_, program->size_, PAGE_EXECUTE_READ, &previous) ||
      !FlushInstructionCache(GetCurrentProcess(), program->memory_, emit.bytes.size()))
    return {};
#else
  if (mprotect(program->memory_, program->size_, PROT_READ | PROT_EXEC) != 0)
    return {};
  __builtin___clear_cache(static_cast<char*>(program->memory_),
                          static_cast<char*>(program->memory_) + emit.bytes.size());
#endif
  program->code_size_ = emit.bytes.size();
  return program;
}
catch (std::bad_alloc const&) {
  return {};
}

NativeExpression::~NativeExpression()
{
  if (memory_ != nullptr) {
#if defined(_WIN32)
    VirtualFree(memory_, 0, MEM_RELEASE);
#else
    munmap(memory_, size_);
#endif
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
