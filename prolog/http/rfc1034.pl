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
@version 2015/12
*/

:- use_module(library(dcg/dcg_ext), ['*'//2,alpha//1,digit//2]).
:- use_module(library(dcg/dcg_word)).





%! digit(-Code:code)// is det.
% ```
% <digit> ::= any one of the ten digits 0 through 9
% ```

digit(C) --> digit(_, C).



%! label(-Label:string)// .
% ```
% <label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
% ```

label(S) --> 'ldr-str'(S).



%! 'ldh-str'(-String:string)// is det.
% ```
% <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
% ```

'ldr-str'(S) --> dcg_string('let-dig-hyp', S).



%! 'let-dig'(-Code:code)// is det.
% ```
% <let-dig> ::= <letter> | <digit>
% ```

'let-dig'(C) --> letter(C).
'let-dig'(C) --> digit(C).



%! 'let-dig-hyp'(-Code:code)// is det.
% ```
% <let-dig-hyp> ::= <let-dig> | "-"
% ```

'let-dig-hyp'(0'-) --> "-", !.
'let-dig-hyp'(C) --> 'let-dig'(C).



%! letter(-Code:code)// is det.
% ```
% <letter> ::= any one of the 52 alphabetic characters A through Z in
% upper case and a through z in lower case
% ```

letter(C) --> alpha(C).



%! subdomain(-Dubdomain:list(string))// is det.
% ```
% <subdomain> ::= <label> | <subdomain> "." <label>
% ```

subdomain([H|T]) --> label(H), *(sep_label, T).
sep_label(X) --> ".", label(X).
