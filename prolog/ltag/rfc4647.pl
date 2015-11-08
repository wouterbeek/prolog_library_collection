:- module(
  rfc4647,
  [
    'extended-language-range'//1, % ?LanguageRange:list(string)
    'language-range'//1 % ?LanguageRange:list(string)
  ]
).
:- reexport(
  library(ltag/rfc4646),
  [
    alphanum//1 % ?Code:code
  ]
).

/** <module> RFC 4647: Matching of Language Tags

@author Wouter Beek
@compat RFC 4647
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).





%! 'extended-language-range'(?LanguageRange:list(string))// .
% ```abnf
% extended-language-range = (1*8ALPHA / "*")
%                           *("-" (1*8alphanum / "*"))
% ```

'extended-language-range'([H|T]) -->
  ('m*n'(1, 8, 'ALPHA', H, [convert(1-string)]) ; "*", {H = "*"}), !,
  extended_language_range0(T).
extended_language_range0([H|T]) -->
  "-", !,
  ('m*n'(1, 8, alphanum, H, [convert(1-string)]) ; "*", {H = "*"}),
  extended_language_range0(T).
extended_language_range0([]) --> "".



%! 'language-range'(?LanguageRange:list(string))// .
% ```abnf
% language-range = (1*8ALPHA *("-" 1*8alphanum)) / "*"
% ```

'language-range'([H|T]) -->
  'm*n'(1, 8, 'ALPHA', H, [convert(1-string)]), !,
  language_range0(T).
'language-range'(["*"]) --> "*".
language_range0([H|T]) -->
  "-", !,
  'm*n'(1, 8, alphanum, H, [convert(1-string)]),
  language_range0(T).
language_range0([]) --> "".
