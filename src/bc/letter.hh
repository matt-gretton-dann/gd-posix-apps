/** Letter names shared by the VM and its non-owning call router.
 * Copyright 2021, Matthew Gretton-Dann
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_LETTER_HH_INCLUDED
#define GD_BC_LETTER_HH_INCLUDED

namespace GD::Bc {
/** \brief A letter
 *
 * Used for variable, function, and array names.
 */
class Letter
{
public:
  static constexpr unsigned count_ = 26;

  /** \brief  Construct letter from char. */
  explicit Letter(char l);

  /** \brief Convert to unsigned.  */
  explicit operator unsigned() const;

  /* Comparison operators.  */
  auto operator==(Letter l) const -> bool;
  auto operator==(char l) const -> bool;

private:
  friend class VariableMask;
  struct EncodedIndex
  {
  };
  Letter(unsigned index, EncodedIndex) : letter_(index) {}
  static auto encode(char l) -> unsigned;
  unsigned letter_;  ///< The letter.
};
}  // namespace GD::Bc
#endif
