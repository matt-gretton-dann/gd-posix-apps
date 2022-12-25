/** \file   src/util/program-name.cc
 *  \brief  Program name management.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */
#include "gd/libgen.h"

#include "gd/string.h"

#include "util/utils.hh"

#include <string>
#include <string_view>

namespace {
/** Manage the Program Name.
 */
class ProgramName
{
public:
  /** Get the static program name object.  */
  static auto get() -> ProgramName&
  {
    static ProgramName pn;
    return pn;
  }

  /** \brief Get the program name.
   */
  [[nodiscard]] auto program_name() const noexcept -> std::string_view { return name_; }

  /** \brief Set the program name.
   *
   * We have to jump around some hoops here.  basename() may alter the parameter it is passed, or
   * alternatively may return a pointer to data that is then resused.  So we have to copy the input
   * and output.
   *
   * We don't fail - setting the program name to the input string if we can't duplicate.
   */
  void program_name(std::string_view argv0)
  {
    std::string n(argv0);
    name_ = ::basename(n.data());  // NOLINT(concurrency-mt-unsafe)
    if (name_.empty()) {
      name_ = argv0;
    }
  }

private:
  // This is safe from exceptions: std::string's default constructor does not throw.
  ProgramName() noexcept = default;  // NOLINT(bugprone-exception-escape)

  std::string name_{};
};
}  // namespace

auto GD::program_name() noexcept -> std::string_view { return ProgramName::get().program_name(); }

void GD::program_name(std::string_view argv0) { ProgramName::get().program_name(argv0); }
