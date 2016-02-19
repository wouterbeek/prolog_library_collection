:- module(
  rfc1034,
  [
    subdomain//1 % -Subdomain:list(string)
  ]
).

/** <module> RFC 1034: DOMAIN NAMES - CONCEPTS AND FACILITIES

@author Wouter Beek
@compat RFC 1034
@see https://tools.ietf.org/html/rfc1034
@version 2015/12-2016/01
*/

:- use_module(library(dcg/dcg_ext), [
     '?'//2,
     '*'//2
   ]).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1 as alpha0, % ?Code
     'DIGIT'//2 as digit0  % ?Weight:between(0,9), ?Code
   ]).
:- use_module(library(dcg/dcg_word)).





%! digit(?Code)// .
% ```
% <digit> ::= any one of the ten digits 0 through 9
% ```

digit(C) --> digit0(_, C).



%! label(-Label:string)// is det.
% ```
% <label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
% ```

label(S) --> letter(H), label_tail(T), !, {string_codes(S, [H|T])}.

label_tail(L2)  --> 'ldh-str'(L1), 'let-dig'(X), {append(L1, [X], L2)}.
label_tail([H]) --> 'let-dig'(H).
label_tail([])  --> "".



%! 'ldh-str'(-Codes:list(code))// is det.
% ```
% <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
% ```

'ldh-str'(L) --> 'let-dig-hyp'(H), ('ldh-str'(T), {L = [H|T]} ; {L = [H]}).



%! 'let-dig'(?Code)// .
% ```
% <let-dig> ::= <letter> | <digit>
% ```

'let-dig'(C) --> letter(C), !.
'let-dig'(C) --> digit(C).



%! 'let-dig-hyp'(?Code)// .
% ```
% <let-dig-hyp> ::= <let-dig> | "-"
% ```

'let-dig-hyp'(0'-) --> "-", !.
'let-dig-hyp'(C)   --> 'let-dig'(C).



%! letter(?Code)// .
% ```
% <letter> ::= any one of the 52 alphabetic characters A through Z in
% upper case and a through z in lower case
% ```

letter(C) --> alpha0(C).



%! subdomain(-Dubdomain:list(string))// is det.
% ```
% <subdomain> ::= <label> | <subdomain> "." <label>
% ```

subdomain([H|T]) --> label(H), *(sep_label, T).

sep_label(X) --> ".", label(X).
