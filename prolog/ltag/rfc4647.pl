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

:- use_module(library(dcg/dcg_re)).





%! 'extended-language-range'(?LanguageRange:list(string))// .
% ```abnf
% extended-language-range = (1*8ALPHA / "*")
%                           *("-" (1*8alphanum / "*"))
% ```

'extended-language-range'([H|T]) --> ext_lrange1(H), *(ext_lrange2, T).
ext_lrange1(X) --> 'm*n'(1, 8, 'ALPHA', Cs), !, {string_codes(X, Cs)}.
ext_lrange1("*") --> "*".
ext_lrange2(X) --> "-", ext_lrange1(X).



%! 'language-range'(?LanguageRange:list(string))// .
% ```abnf
% language-range = (1*8ALPHA *("-" 1*8alphanum)) / "*"
% ```

'language-range'([H|T]) --> lrange1(H), *(lrange2, T).
lrange1(S) --> 'm*n'(1, 8, 'ALPHA', Cs), {string_codes(S, Cs)}.
lrange2(S) --> "-", lrange1(S).
