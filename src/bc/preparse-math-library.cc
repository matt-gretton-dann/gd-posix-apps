// SPDX-License-Identifier: Apache-2.0
#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>

#include "bc.hh"

namespace GD::Bc::Library {
auto text() -> std::string_view;
}

namespace {
using namespace GD::Bc;

// Encode every byte independently. Numeric escapes also preserve embedded NULs
// and avoid relying on the source file's execution character encoding.
void write_string(std::ostream& out, std::string_view value)
{
  constexpr std::string_view octal = "01234567";
  out << "std::string(\"";
  for (char character : value) {
    auto const byte = static_cast<unsigned char>(character);
    out << '\\' << octal[byte >> 6U] << octal[(byte >> 3U) & 7U] << octal[byte & 7U];
  }
  out << "\", " << value.size() << ')';
}

void write_operand(std::ostream& out, Instruction::Operand const& operand)
{
  std::visit(
    [&out](auto const& value) {
      using T = std::remove_cvref_t<decltype(value)>;
      if constexpr (std::is_same_v<T, std::string>) {
        write_string(out, value);
      }
      else if constexpr (std::is_same_v<T, Instruction::Stream>) {
        out << "static_cast<Instruction::Stream>(" << static_cast<unsigned>(value) << ')';
      }
      else if constexpr (std::is_same_v<T, Instruction::Offset>) {
        out << "Instruction::Offset{" << value << '}';
      }
      else if constexpr (std::is_same_v<T, Location>) {
        out << "Location(";
        write_string(out, value.file_name());
        out << ", " << value.line() << ", " << value.column() << ')';
      }
      else if constexpr (std::is_same_v<T, VariableMask>) {
        std::ostringstream variables;
        std::ostringstream arrays;
        value.for_each_variable([&](Letter letter) { variables << letter; });
        value.for_each_array([&](Letter letter) { arrays << letter; });
        out << "VariableMask(";
        write_string(out, variables.str());
        out << ", ";
        write_string(out, arrays.str());
        out << ')';
      }
      else if constexpr (std::is_same_v<T, unsigned>) {
        out << value << 'U';
      }
      else if constexpr (std::is_same_v<T, Letter>) {
        out << "Letter('" << value << "')";
      }
      else if constexpr (std::is_same_v<T, Variable>) {
        out << "Variable('" << value.get() << "')";
      }
      else {
        static_assert(std::is_same_v<T, Array>);
        out << "Array('" << value.get() << "')";
      }
    },
    operand);
}

void generate(std::string_view source, std::ostream& out)
{
  Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(source)), true);
  out << "// Generated from math-library.bc by the ordinary bc parser.\n";
  for (;;) {
    auto const instructions = parser.parse();
    if (parser.seen_quit() ||
        std::ranges::any_of(*instructions, [](Instruction const& instruction) {
          return instruction.opcode() == Instruction::Opcode::quit;
        })) {
      throw std::runtime_error("math library contains quit, halt, or a parse error");
    }
    out << "code.clear();\ncode.reserve(" << instructions->size() << ");\n";
    for (auto const& instruction : *instructions) {
      out << "code.emplace_back(static_cast<Instruction::Opcode>("
          << static_cast<unsigned>(instruction.opcode()) << ')';
      if (instruction.has_op1()) {
        out << ", ";
        write_operand(out, instruction.op1());
      }
      if (instruction.has_op2()) {
        out << ", ";
        write_operand(out, instruction.op2());
      }
      out << ");\n";
    }
    out << "if (!vm.execute(code)) { return; }\n";
    if (!instructions->empty() && instructions->back().opcode() == Instruction::Opcode::eof) {
      return;
    }
  }
}
}  // namespace

auto main(int argc, char** argv) -> int
{
  if (argc != 2) {
    return 1;
  }
  try {
    std::ofstream output(argv[1], std::ios::binary);
    output.exceptions(std::ios::failbit | std::ios::badbit);
    generate(GD::Bc::Library::text(), output);
    output.close();
  }
  catch (std::exception const& error) {
    std::cerr << error.what() << '\n';
    return 1;
  }
  return 0;
}
