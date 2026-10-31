/** \file test-vm-arrays.cc
 * Valid array load, parameter and local-restoration contracts.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <sstream>

#include "bc.hh"

TEST_CASE("VM loads and pops array values", "[bc][vm][arrays]")
{
  using GD::Bc::Array;
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  using D = I::Offset;
  // Use an actual function activation so the caller's argument marker and the
  // callee's incoming prologue have their normal execution contracts.
  GD::Bc::Instructions instructions{
    I(O::function_begin, GD::Bc::VariableMask("", "b"), GD::Bc::Location("fixture", 1, 1)),
    I(O::pop_param_array),
    I(O::array, Array('b')),
    I(O::store, D(-1), D(-2)),
    I(O::pop_param_mark),
    I(O::number, "0"),
    I(O::array_element, Array('b'), D(-1)),
    I(O::load, D(-1)),
    I(O::return_, D(-1)),
    I(O::function_end, GD::Bc::Letter('f'), D(-9)),
    I(O::push_param_mark),
    I(O::number, "6"),
    I(O::number, "0"),
    I(O::array_element, Array('a'), D(-1)),
    I(O::store, D(-1), D(-3)),
    I(O::array, Array('a')),
    I(O::load, D(-1)),
    I(O::push_param, D(-1)),
    I(O::call, GD::Bc::Letter('f'), GD::Bc::Location("fixture", 2, 1)),
    I(O::print, D(-1), I::Stream::output),
    I(O::number, "0"),
    I(O::array_element, Array('b'), D(-1)),
    I(O::load, D(-1)),
    I(O::print, D(-1), I::Stream::output),
    I(O::number, "0"),
    I(O::array_element, Array('a'), D(-1)),
    I(O::load, D(-1)),
    I(O::print, D(-1), I::Stream::output),
    I(O::eof)};
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false);
  REQUIRE_FALSE(vm.execute(instructions));
  REQUIRE(output.str() ==
          "6\n0\n6\n");  // Returned element, restored local, unchanged caller array.
  REQUIRE(errors.str().empty());
}
