/** Checked, stackless x86-64 expressions with no calls or nonvolatile writes.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <array>
#include <cstring>
#include <vector>

#if (!defined(__x86_64__) && !defined(_M_X64)) || defined(_M_ARM64EC)
#  error Native leaf expressions require the x86-64 ABI
#endif

#include "native-expression-plan.hh"
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
  void start(unsigned argument)
  {
    for (auto value : {0xf3U, 0x0fU, 0x1eU, 0xfaU})
      byte(value);  // ENDBR64, including as a no-op on processors without IBT.
    byte(0x48);
    byte(0x85);
    byte(0xc0U | (argument << 3) | argument);
    conditional_failure(8);  // JS: input above INT64_MAX.
  }
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

auto NativeExpression::compile(std::span<NativeExpressionOp const> operations, std::uint8_t result,
                               Failure* failure) -> std::shared_ptr<NativeExpression const>
try {
  if (failure != nullptr)
    *failure = Failure::unavailable;
  auto generated =
    emit_native_expression<Emitter, argument_register>(operations, result, temporary_registers);
  if (!generated) {
    if (failure != nullptr)
      *failure = Failure::unsupported;
    return {};
  }
  auto const& emit = *generated;
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
  if (failure != nullptr)
    *failure = Failure::none;
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
