:- module(
  rfc3066,
  [
    'language-range'//1, % ?LanguageRange:list(string)
    'Language-Tag'//1, % ?LanguageTag:list(string)
    'Subtag'//1 % ?SubTag:string
  ]
).
:- reexport(
  rfc1766,
  [
    'Primary-tag'//1 as 'Primary-subtag' % ?SubTag:string
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
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).





%! 'language-range'(?LanguageRange:list(string))// .
% ```abnf
% language-range = language-tag / "*"
% ```

'language-range'(L)     --> 'Language-Tag'(L), !.
'language-range'(["*"]) --> "*".



%! 'Language-Tag'(?LanguageTag:list(string))// .
% ```abnf
% Language-Tag = Primary-subtag *( "-" Subtag )
% ```

'Language-Tag'([H|T]) --> 'Primary-subtag'(H), *(sep_subtag, T).
sep_subtag(S) --> "-", 'Subtag'(S).



%! 'Subtag'(?Subtag:string)// .
% ```abnf
% Subtag = 1*8(ALPHA / DIGIT)
% ```

'Subtag'(S) --> 'm*n'(1, 8, alphadigit, Cs), {string_codes(S, Cs)}.
