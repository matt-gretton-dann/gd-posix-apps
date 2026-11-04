// SPDX-License-Identifier: Apache-2.0
#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

#include "bc.hh"
#include "math-library-image.hh"

namespace GD::Bc::Library {
auto text() -> std::string_view;
}

namespace {
using namespace GD::Bc;

using namespace GD::Bc::Library;

class ImageWriter
{
public:
  explicit ImageWriter(std::ostream& out) : out_(out) {}

  [[nodiscard]] auto size() const noexcept -> std::size_t { return count_; }

  void byte(unsigned value)
  {
    if (value > 255) {
      throw std::runtime_error("math library image byte overflow");
    }
    constexpr std::string_view octal = "01234567";
    if (count_++ % 24 == 0) {
      out_ << "\"\n\"";
    }
    out_ << '\\' << octal[value >> 6U] << octal[(value >> 3U) & 7U] << octal[value & 7U];
  }

  void word(std::size_t value)
  {
    if (value > image_word_max) {
      throw std::runtime_error("math library image word overflow");
    }
    byte(static_cast<unsigned>(value & 255U));
    byte(static_cast<unsigned>(value >> 8U));
  }

  void string(std::string_view value)
  {
    word(value.size());
    for (char character : value) {
      byte(static_cast<unsigned char>(character));
    }
  }

private:
  std::ostream& out_;
  std::size_t count_{0};
};

void write_operand(ImageWriter& out, Instruction::Operand const& operand)
{
  auto tag = [&out](ImageOperand kind) { out.byte(static_cast<unsigned>(kind)); };
  std::visit(
    [&out, tag](auto const& value) {
      using T = std::remove_cvref_t<decltype(value)>;
      if constexpr (std::is_same_v<T, std::string>) {
        tag(ImageOperand::string);
        out.string(value);
      }
      else if constexpr (std::is_same_v<T, Instruction::Stream>) {
        tag(ImageOperand::stream);
        out.byte(static_cast<unsigned>(value));
      }
      else if constexpr (std::is_same_v<T, Instruction::Offset>) {
        if (value < -image_offset_bias || value >= image_offset_bias) {
          throw std::runtime_error("math library image offset overflow");
        }
        tag(ImageOperand::offset);
        out.word(static_cast<unsigned>(value + image_offset_bias));
      }
      else if constexpr (std::is_same_v<T, Location>) {
        tag(ImageOperand::location);
        out.string(value.file_name());
        out.word(value.line());
        out.word(value.column());
      }
      else if constexpr (std::is_same_v<T, VariableMask>) {
        tag(ImageOperand::mask);
        std::ostringstream variables;
        std::ostringstream arrays;
        value.for_each_variable([&](Letter letter) { variables << letter; });
        value.for_each_array([&](Letter letter) { arrays << letter; });
        out.string(variables.str());
        out.string(arrays.str());
      }
      else if constexpr (std::is_same_v<T, unsigned>) {
        tag(ImageOperand::unsigned_);
        out.word(value);
      }
      else if constexpr (std::is_same_v<T, Letter>) {
        tag(ImageOperand::letter);
        out.byte(static_cast<unsigned>(value));
      }
      else if constexpr (std::is_same_v<T, Variable>) {
        tag(ImageOperand::variable);
        out.byte(static_cast<unsigned>(value.get()));
      }
      else {
        static_assert(std::is_same_v<T, Array>);
        tag(ImageOperand::array);
        out.byte(static_cast<unsigned>(value.get()));
      }
    },
    operand);
}

void generate(std::string_view source, std::ostream& out)
{
  Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(source)), true);
  out << "// Generated from math-library.bc by the ordinary bc parser.\n"
         "constexpr char image[] = \"";
  ImageWriter image(out);
  std::vector<ImageItem> items;
  for (;;) {
    auto const instructions = parser.parse();
    if (parser.seen_quit() ||
        std::ranges::any_of(*instructions, [](Instruction const& instruction) {
          return instruction.opcode() == Instruction::Opcode::quit;
        })) {
      throw std::runtime_error("math library contains quit, halt, or a parse error");
    }
    auto const begin = image.size();
    image.word(instructions->size());
    for (auto const& instruction : *instructions) {
      image.byte(static_cast<unsigned>(instruction.opcode()));
      if (instruction.has_op1()) {
        write_operand(image, instruction.op1());
      }
      if (instruction.has_op2()) {
        write_operand(image, instruction.op2());
      }
    }
    // Only a complete, isolated definition can be deferred without changing
    // the timing of top-level execution or its observable side effects.
    using O = Instruction::Opcode;
    auto function = static_cast<unsigned>(Letter::count_);
    if (instructions->size() >= 2 && instructions->front().opcode() == O::function_begin &&
        instructions->back().opcode() == O::function_end &&
        std::get<Instruction::Offset>(instructions->back().op2()) ==
          -static_cast<Instruction::Offset>(instructions->size() - 1)) {
      function = static_cast<unsigned>(std::get<Letter>(instructions->back().op1()));
    }
    items.push_back({begin, image.size() - begin, function});
    if (!instructions->empty() && instructions->back().opcode() == O::eof) {
      out << "\";\nconstexpr ImageItem items[] = {\n";
      for (auto const& item : items) {
        out << "  {" << item.begin << ", " << item.size << ", " << item.function << "},\n";
      }
      out << "};\n";
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
