:- module(
  rfc4647,
  [
    'extended-language-range'//1, % ?LanguageRange:list(string)
    'language-range'//1 % ?LanguageRange:list(string)
  ]
).

/** <module> RFC 4647: Matching of Language Tags

@author Wouter Beek
@compat RFC 4647
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).





%! 'extended-language-range'(?LanguageRange:list(string))// .
% ```abnf
% extended-language-range = (1*8ALPHA / "*") *("-" (1*8alphanum / "*"))
% ```

'extended-language-range'([H|T]) -->
  ('m*n'(1, 8, alpha, Cs) -> {string_codes(H, Cs)} ; "*"),
  *(extended_language_range, T).
extended_language_range(S) -->
  "-",
  ('m*n'(1, 8, alphadigit, Cs) -> {string_codes(S, Cs)} ; "*" -> {S = "*"}).



%! 'language-range'(?LanguageRange:list(string))// .
% ```abnf
% language-range = (1*8ALPHA *("-" 1*8alphanum)) / "*"
% ```

'language-range'([H|T]) -->
  'm*n'(1, 8, alpha, Cs), !, {string_codes(H, Cs)}, *(language_range, T).
'language-range'(["*"]) --> "*".
language_range(S) --> "-", 'm*n'(1, 8, alphadigit, Cs), {string_codes(S, Cs)}.
