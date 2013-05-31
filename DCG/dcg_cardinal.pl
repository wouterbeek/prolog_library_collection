:- module(
  dcg_cardinal,
  [
    binary_digit//0,
    decimal_digit//0,
    hexadecimal_digit//0,
    octal_digit//0
  ]
).

/** <module> DCG_CARDINAL

DCGs for cardinal numbers.

*/

:- use_module(dcg(dcg_ascii)).

:- reexport(
  library(dcg/basics),
  [
    digit//1,
    digits//1,
    float//1,
    integer//1,
    number//1,
    xdigit//1,
    xdigits//1,
    xinteger//1
  ]
).



binary_digit --> zero.
binary_digit --> one.

decimal_digit --> octal_digit.
decimal_digit --> eight.
decimal_digit --> nine.

hexadecimal_digit --> decimal_digit.
hexadecimal_digit --> a.
hexadecimal_digit --> b.
hexadecimal_digit --> c.
hexadecimal_digit --> d.
hexadecimal_digit --> e.
hexadecimal_digit --> f.

octal_digit --> binary_digit.
octal_digit --> two.
octal_digit --> three.
octal_digit --> four.
octal_digit --> five.
octal_digit --> six.
octal_digit --> seven.

