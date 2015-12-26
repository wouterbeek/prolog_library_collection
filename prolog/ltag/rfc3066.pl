:- module(
  rfc3066,
  [
    'language-range'//1, % -LanguageRange:list(string)
    'Language-Tag'//1 % -LanguageTag:list(string)
  ]
).

/** <module> RFC 3066: Tags for the Identification of Languages

# Mistakes

'language-range'//1 is defined in terms of 'language-tag'//1
which does not exist.  'Language-Tag'//1 does exist.

---

@author Wouter Beek
@compat RFC 3066
@deprecated Use modules `rfc4646` and `rfc4647` instead.
@see http://tools.ietf.org/html/rfc3066
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % -Code:code
     'DIGIT'//2 % -Weight:between(0,9)
                % -Code:code
   ]).





%! 'language-range'(-LanguageRange:list(string))// .
% ```abnf
% language-range = language-tag / "*"
% ```

'language-range'(L) --> 'Language-Tag'(L), !.
'language-range'("*") --> "*".



%! 'Language-Tag'(-LanguageTag:list(string))// .
% ```abnf
% Language-Tag = Primary-subtag *( "-" Subtag )
% ```

'Language-Tag'([H|T]) --> 'Primary-subtag'(H), *(sep_subtag, T).
sep_subtag(S) --> "-", 'Subtag'(S).



%! 'Primary-subtag'(-Subtag:string)// is det.
% ```abnf
% Primary-subtag = 1*8ALPHA
% ```

'Primary-subtag'(S) --> 'm*n'(1, 8, 'ALPHA', Cs), {string_codes(S, Cs)}.



%! 'Subtag'(-Subtag:string)// .
% ```abnf
% Subtag = 1*8(ALPHA / DIGIT)
% ```

'Subtag'(S) --> 'm*n'(1, 8, subtag_code, Cs), {string_codes(S, Cs)}.
subtag_code(C) --> 'ALPHA'(C).
subtag_code(C) --> 'DIGIT'(_, C).
