#include "bc.hh"
#include <iostream>
int main() { std::cout << "{\"number_bytes\":" << sizeof(GD::Bc::Number) << ",\"storage_bytes\":" << sizeof(GD::Bc::Details::DigitStorage<unsigned>) << ",\"instruction_bytes\":" << sizeof(GD::Bc::Instruction) << "}\n"; }
