:- module(
  rfc4647,
  [
    'extended-language-range'//1, % ?LanguageRange:list(atom)
    'language-range'//1           % ?LanguageRange:list(atom)
  ]
).

/** <module> RFC 4647 - Matching of Language Tags

@author Wouter Beek
@compat RFC 4647
@see https://tools.ietf.org/html/rfc4647
@version 2017/05-2017/06
*/

:- use_module(library(dcg/dcg_ext), except([alphanum//1])).
:- use_module(library(dcg/rfc4234)).





%! alphanum(?Code)// .
%
% ```abnf
% alphanum = ALPHA / DIGIT
% ```

alphanum(C) --> 'ALPHA'(C).
alphanum(C) --> 'DIGIT'(C).



%! 'extended-language-range'(?LanguageRange:list(atom))// .
%
% ```abnf
% extended-language-range = (1*8ALPHA / "*") *("-" (1*8alphanum / "*"))
% ```

'extended-language-range'([H|T]) -->
  ('m*n'(1, 8, 'ALPHA', Cs) -> {atom_codes(H, Cs)} ; "*"),
  *(sep_extended_language_range0, T).

sep_extended_language_range0(A) -->
  "-",
  ('m*n'(1, 8, alphanum, Cs) -> {atom_codes(A, Cs)} ; "*" -> {A = '*'}).



%! 'language-range'(?LanguageRange:list(atom))// .
%
% ```abnf
% language-range = (1*8ALPHA *("-" 1*8alphanum)) / "*"
% ```

'language-range'([H|T]) -->
  'm*n'(1, 8, 'ALPHA', Cs), !,
  {atom_codes(H, Cs)},
  *(sep_language_range0, T).
'language-range'(["*"]) --> "*".

sep_language_range0(A) -->
  "-",
  'm*n'(1, 8, alphanum, Cs),
  {atom_codes(A, Cs)}.
