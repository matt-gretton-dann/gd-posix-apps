# Compress LZW Compression File Format

We emit LZW compress files which are compatible with other implementations of the LZW .Z file
format.  In particular [ncompress](https://github.com/vapier/ncompress).

However, this is a ground up reimplementation, and the file-format has been reverse engineered from
examining generated files and reading some commentary on Wikipedia.

## File Format overview.

The file can be considered to be a "little-endian" "bit" oriented file.  By which I mean that
objects stored in the file are measured in bit sizes and not number of bytes occupied, and that
data is stored in a little-endian(ish) format.

For example supposing we wanted to store the numbers 123456789, ABCDEFGHI, abcdefghi in that order
within a file (where the numerals and letters represent bit places).  The byte output will be as
follows:

|  *Byte*  |  *Value*  |
| :------- | :-------- |
| 0 | 23456789 |
| 1 | CDEFGHI1 |
| 2 | defghiBA |
| 3 | xxxxxabc |

`x` represents "Unknown".

There is no indication within the file of how many bits a number occupies.  It is up to the reader
to know this.

## Header

The first 24 bits of the file are a header containing the following information

|  *Bits (inclusive)*  |  *Identifier*  |  *Meaning*  |
| :------------------- | :------------- | :---------- |
| 0-15 | `magic` | 0x9d1f.  Magic identifying this as a .Z compress file. |
| 16-20 | `max_bits` | Maximum number of bits in a code point.  Should be in range 9-16. |
| 21-22 | | 0.  Should be zero.  |
| 23 | `can_reset` | If set then the file has a special 'reset' symbol at 256 which causes compression to start again. |

## Data

Following the header is the file data.  This consists of sequences of code-points describing the
compressed data.  This consists of the following number of values of the given width (assuming
`max_bits` is 16):

|  *Bit size*  |  *Count*  |
| :----------- | :-------- |
| 9 | 256 if `can_reset`; or 257 followed by 7 padding if `!can_reset`.  |
| 10 | 512 |
| 11 | 1024 |
| 12 | 2048 |
| 13 | 4096 |
| 14 | 8192 |
| 15 | 16384 |
| 16 | 32768 or until end of file.  |

If we run out of data to compress we emit just the final bits with no extra padding.  If `max_bits`
< 16 then the table above will be truncated appropriately.

If `can_reset` is set and code 256 is encountered then we ignore the next 0-7 code-points so that we
reach an 8-code-point alignment, and reset the system with the bit width as 9.

## Pseudo-code

The following pseudo-code describes how to interpret the data.

```C++
bits = 9; /* Current number of bits for a code-point. */
next_symbol = 256 + can_reset; /* First symbol to be added. */
reset_symbol = can_reset ? 256 : ((1 << max_bits) + 2); /* Reset/clear symbol.  */
max_symbol = (1 << max_bits) - 1; /* Maximum symbol.  */
cp_count = 0; /* Number of output codepoint.  */

std::vector<std::string> code_points; /* Entry N contains string for code-point N. */
reset_codepoint(code_points); /* Set first 256 entries to map N -> char(N). */

current_cp = read_bits(bits);
std::cout << code_points[current_cp];
++cp_count;

while ((cp = read_bits(bits)) != -1) {
  ++cp_count;
  /* Print the next symbol, handling special case for ABABA repetitions.  */
  if (cp == reset_symbol) {
    /* Historic reasons: Make sure cp_count is a multiple of 8.  */
    while (cp_count & 7) {
      read_bits(bits);
      ++cp_count;
    }
    bts = 9;
    next_symbol = 256 + can_reset;
    code_points.clear();
    reset_codepoint(code_points);
    continue;
  }
  else if (cp == next_symbol) {
    code_points[next_symbol++] = code_points[current_cp] + code_points[current_cp][0];
  } else if (next_symbol <= max_symbol) {
    code_points[next_symbol++] = code_points[current_cp] + code_points[cp][0];
  }
  std::cout << code_points[cp];
  current_cp = cp;

  if (next_symbol == (1 << bits) + 1) {
    /* Need to up the number of bits to read for each code symbol.  */
    /* First because of "historic reasons" we need to read enough blank code points to cp_count is
       a multiple of 8.  */
    while (cp_count & 7) {
      read_bits(bits);
      ++cp_count;
    }

    ++bits;
  }
}
```
