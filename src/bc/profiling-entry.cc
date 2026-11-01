/** \file profiling-entry.cc
 * Explicit diagnostic output for profiling builds; never linked for timings.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <cinttypes>
#include <cstdio>
#include <cstdlib>

#include "bc.hh"
#include "find-multiply-split-point.hh"

namespace GD::Bc::Profile {
namespace {
void write_stats(std::FILE* output, AllocationStats const& values)
{
  std::fprintf(output,
               "{\"allocation_calls\":%" PRIu64 ",\"deallocation_calls\":%" PRIu64
               ",\"allocated_bytes\":%" PRIu64 ",\"live_bytes\":%" PRIu64
               ",\"peak_live_bytes\":%" PRIu64 "}",
               values.allocation_calls, values.deallocation_calls, values.allocated_bytes,
               values.live_bytes, values.peak_live_bytes);
}

void report() noexcept
{
#ifdef _WIN32
  auto const* path = ::_wgetenv(L"GD_BC_PROFILE_OUTPUT");
  auto* output = path == nullptr ? nullptr : ::_wfopen(path, L"wb");
#else
  auto const* path = std::getenv("GD_BC_PROFILE_OUTPUT");
  auto* output = path == nullptr ? nullptr : std::fopen(path, "wb");
#endif
  if (path == nullptr || *path == 0) {
    return;
  }
  if (output == nullptr) {
    std::fputs("bc: cannot open profiling output\n", stderr);
    std::_Exit(EXIT_FAILURE);
  }
  auto const values = snapshot();
  std::fprintf(
    output,
    "{\"schema\":1,\"instrumented\":true,"
    "\"scope\":\"selected C++ container and literal memo requested storage; not process malloc or RSS\","
    "\"limb_bytes\":%zu,\"limb_base\":%" PRIu64 ",\"multiply_split_point\":%d,\"total\":",
    sizeof(Number::NumType), static_cast<std::uint64_t>(Number::base_), BC_MULTIPLY_SPLIT_POINT);
  write_stats(output, values.total);
  std::fputs(",\"allocations\":{", output);
  for (std::size_t i = 0; i < values.allocations.size(); ++i) {
    std::fprintf(output, "%s\"%s\":", i == 0 ? "" : ",", name(static_cast<AllocationKind>(i)));
    write_stats(output, values.allocations[i]);
  }
  std::fputs("},\"events\":{", output);
  for (std::size_t i = 0; i < values.events.size(); ++i) {
    std::fprintf(output, "%s\"%s\":%" PRIu64, i == 0 ? "" : ",", name(static_cast<Event>(i)),
                 values.events[i]);
  }
  std::fputs("}}\n", output);
  auto const failed = std::ferror(output) != 0;
  auto const closed = std::fclose(output);
  if (failed || closed != 0) {
    std::fputs("bc: cannot write profiling output\n", stderr);
    std::_Exit(EXIT_FAILURE);
  }
}
}  // namespace

void initialize_report() noexcept
{
  // Normal EOF destroys the VM before this runs. Fatal std::exit paths may
  // retain live local storage, which must not be interpreted as a leak.
  if (std::atexit(report) != 0) {
    std::fputs("bc: cannot register profiling output\n", stderr);
    std::_Exit(EXIT_FAILURE);
  }
}
}  // namespace GD::Bc::Profile
