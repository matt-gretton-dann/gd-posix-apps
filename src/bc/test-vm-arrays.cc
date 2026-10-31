/** \file test-vm-arrays.cc
 * Valid array load and parameter result contracts.
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
  GD::Bc::Instructions instructions{I(O::push_param_mark),
                                    I(O::number, "6"),
                                    I(O::number, "0"),
                                    I(O::array_element, Array('a'), D(-1)),
                                    I(O::store, D(-1), D(-3)),
                                    I(O::array, Array('a')),
                                    I(O::load, D(-1)),
                                    I(O::push_param, D(-1)),
                                    I(O::pop_param_array),
                                    I(O::array, Array('b')),
                                    I(O::store, D(-1), D(-2)),
                                    I(O::pop_param_mark),
                                    I(O::number, "0"),
                                    I(O::array_element, Array('b'), D(-1)),
                                    I(O::load, D(-1)),
                                    I(O::print, D(-1), I::Stream::output),
                                    I(O::eof)};
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false);
  REQUIRE_FALSE(vm.execute(instructions));
  REQUIRE(output.str() == "6\n");
  REQUIRE(errors.str().empty());
}
