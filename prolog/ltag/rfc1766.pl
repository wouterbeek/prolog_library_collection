:- module(
  rfc1766,
  [
    'Language-Tag'//1, % ?LanguageTag:list(string)
    'Primary-tag'//1, % ?SubTag:list(string)
    'Subtag'//1 % ?SubTag:list(string)
  ]
).

/** <module> RFC 1766: Tags for the Identification of Languages

@author Wouter Beek
@compat RFC 1766
@deprecated Use modules `rfc3066` and `rfc3282` instead.
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).





%! 'Language-Tag'(?LanguageTag:list(string))// .
% ```abnf
% Language-Tag = Primary-tag *( "-" Subtag )
% ```

'Language-Tag'([H|T]) --> 'Primary-tag'(H), *(sep_subtag, T).
sep_subtag(S) --> "-", 'Subtag'(S).



%! 'Primary-tag'(?SubTag:string)// .
% ```abnf
% Primary-tag = 1*8ALPHA
% ```

'Primary-tag'(S) --> 'm*n'(1, 8, 'ALPHA', Cs), {string_codes(S, Cs)}.



%! 'Subtag'(?SubTag:string)// .
% ```abnf
% Subtag = 1*8ALPHA
% ```

'Subtag'(S) --> 'm*n'(1, 8, 'ALPHA', Cs), {string_codes(S, Cs)}.
