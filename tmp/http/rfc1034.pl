:- module(
  rfc1034,
  [
    subdomain//1 % -Subdomain:list(atom)
  ]
).

/** <module> RFC 1034: DOMAIN NAMES - CONCEPTS AND FACILITIES

@author Wouter Beek
@compat RFC 1034
@see https://tools.ietf.org/html/rfc1034
@version 2015/12-2016/01
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1 as alpha0, % ?Code:code
     'DIGIT'//2 as digit0  % ?Weight:between(0,9), ?Code:code
   ]).





%! digit(?Code:code)// .
%
% ```
% <digit> ::= any one of the ten digits 0 through 9
% ```

digit(C) -->
  digit0(_, C).



%! label(-Lbl:atom)// is det.
%
% ```
% <label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
% ```

label(Lbl) -->
  letter(H),
  label_tail(T), !,
  {atom_codes(Lbl, [H|T])}.

label_tail(L2)  -->
  'ldh-str'(L1),
  'let-dig'(X),
  {append(L1, [X], L2)}.
label_tail([H]) -->
  'let-dig'(H).
label_tail([])  --> "".



%! 'ldh-str'(-Codes:list(code))// is det.
%
% ```
% <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
% ```

'ldh-str'(L) -->
  'let-dig-hyp'(H),
  ('ldh-str'(T), {L = [H|T]} ; {L = [H]}).



%! 'let-dig'(?Code:code)// .
%
% ```
% <let-dig> ::= <letter> | <digit>
% ```

'let-dig'(C) --> letter(C), !.
'let-dig'(C) --> digit(C).



%! 'let-dig-hyp'(?Code:code)// .
%
% ```
% <let-dig-hyp> ::= <let-dig> | "-"
% ```

'let-dig-hyp'(0'-) --> "-", !.
'let-dig-hyp'(C)   --> 'let-dig'(C).



%! letter(?Code:code)// .
%
% ```
% <letter> ::= any one of the 52 alphabetic characters A through Z in
% upper case and a through z in lower case
% ```

letter(C) -->
  alpha0(C).



%! subdomain(-Dubdomain:list(atom))// is det.
%
% ```
% <subdomain> ::= <label> | <subdomain> "." <label>
% ```

subdomain([H|T]) -->
  label(H),
  *(sep_label, T), !.

sep_label(Lbl) -->
  ".",
  label(Lbl).
