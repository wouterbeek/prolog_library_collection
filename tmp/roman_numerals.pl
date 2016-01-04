:- module(
  roman_numerals,
  [
    roman_number//2 % ?Number:positive_integer
                    % +Options:list(nvpair)
  ]
).

/** <module> Roman to Arabic numeral converter

@author Wouter Beek
@version 2013/06, 2014/03, 2014/12
*/

:- use_module(library(option)).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_char)).

:- predicate_options(roman_digit/2, 2, [
  case(+oneof([lower,upper])),
  subtractive_notation(+boolean)
]).





%! roman_digit(?Number:positive_integer, +Options:list(nvpair))// .
% Symbols are placed from left to right in order of value
% (from largest to lowest).
%
% ### Subtractive notation
%
% Subtractive notation can be set with the option
% `subtractive_notation(+boolean)` (default: `true`)
% to avoid four identical characters being repeated in succession:
%   - The numeral I can be placed before V and X to make 4 units (IV) and
%     9 units (IX) respectively
%   - X can be placed before L and C to make 40 (XL) and 90 (XC) respectively.
%   - C can be placed before D and M to make 400 (CD) and 900 (CM).
%
% ### Case
%
% Option `case(+oneof([lower,upper]))` (default `upper`)
% is used to set the case of the roman digits.

roman_number(N, Options) -->
  {
    option(subtractive_notation(Subtr), Options, true),
    option(case(Case), Options, upper)
  },
  roman_number(N, Subtr, Case).


%! roman_number(
%!   +Number:positive_integer,
%!   +SubtractiveNotation:boolean,
%!   +Case:oneof([lower,upper])
%! )// .
% Only the first result is correct, since otherwise subtractive notation
% is applied in unauthorized ways. For instance:
% ```prolog
% Codes = [67, 73] ;
% Codes = "XCXI" ;
% Codes = "XCIXII" ;
% Codes = "XCVVI" ;
% Codes = "XCVIVII" ;
% Codes = "XCVIV" ;
% Codes = "XCVIIVI" ;
% Codes = "XCVIIIV" ;
% Codes = "XCVIIIIII" ;
% ...
% ```

roman_number(0, _, _) --> "".
roman_number(N1, Subtr, Case) -->
  roman_digit(N0, Subtr, Case),
  {
    N2 is N1 - N0,
    N2 >= 0
  },
  roman_number(N2, Subtr, Case).


%! roman_digit(
%!   ?Weight:between(1,1000),
%!   +SubtractiveNotation:boolean,
%!   +Case:oneof([lower,upper])
%! )// .
% Case insensitive, prefering uppercase letters for the generative case.

roman_digit(1000, _,   Case) --> char_case(m, Case).
roman_digit(900, true, Case) --> char_case(c, Case), char_case(m, Case).
roman_digit(500, _,    Case) --> char_case(d, Case).
roman_digit(400, true, Case) --> char_case(c, Case), char_case(d, Case).
roman_digit(100, _,    Case) --> char_case(c, Case).
roman_digit(90,  true, Case) --> char_case(x, Case), char_case(c, Case).
roman_digit(50,  _,    Case) --> char_case(l, Case).
roman_digit(40,  true, Case) --> char_case(x, Case), char_case(l, Case).
roman_digit(10,  _,    Case) --> char_case(x, Case).
roman_digit(9,   true, Case) --> char_case(i, Case), char_case(x, Case).
roman_digit(5,   _,    Case) --> char_case(v, Case).
roman_digit(4,   true, Case) --> char_case(i, Case), char_case(v, Case).
roman_digit(1,   _,    Case) --> char_case(i, Case).

