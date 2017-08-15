:- module(
  rfc4647,
  [
    'extended-language-range'//1, % ?LanguageRange
    'language-range'//1           % ?LanguageRange
  ]
).

/** <module> RFC 4647 - Matching of Language Tags

@author Wouter Beek
@compat RFC 4647
@see https://tools.ietf.org/html/rfc4647
@version 2017/05-2017/08
*/

:- use_module(library(dcg/dcg_ext), except([alphanum//1])).
:- use_module(library(dcg/rfc4234)).





%! alphanum(?Code:code)// .
%
% ```abnf
% alphanum = ALPHA / DIGIT
% ```

alphanum(Code) --> 'ALPHA'(Code).
alphanum(Code) --> 'DIGIT'(Code).



%! 'extended-language-range'(?LanguageRange:list(atom))// .
%
% ```abnf
% extended-language-range = (1*8ALPHA / "*") *("-" (1*8alphanum / "*"))
% ```

'extended-language-range'([H|T]) -->
  (dcg_atom('m*n'(1, 8, 'ALPHA'), H) ; "*"),
  *('extended-language-range_', T).

'extended-language-range_'(Atom) -->
  "-",
  (dcg_atom('m*n'(1, 8, alphanum), Atom) ; "*").



%! 'language-range'(?LanguageRange:list(atom))// .
%
% ```abnf
% language-range = (1*8ALPHA *("-" 1*8alphanum)) / "*"
% ```

'language-range'([H|T]) -->
  dcg_atom('m*n'(1, 8, 'ALPHA'), H),
  *('language-range_', T).
'language-range'(['*']) --> "*".

'language-range_'(Atom) -->
  "-",
  dcg_atom('m*n'(1, 8, alphanum), Atom).
