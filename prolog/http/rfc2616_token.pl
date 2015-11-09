:- module(
  rfc2616_token,
  [
    comment//1, % ?Comment:string
    product//1, % ?Product:dict
    'product-version'//1, % ?ProductVersion:string
    'quoted-string'//1, % ?String:string
    token//1, % ?Token:string
    value//1 % ?Value:string
  ]
).

/** <module> RFC 2616: Tokens

@author Wouter Beek
@compat RFC 2616
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(string_ext)).





%! comment(?Comment:string)// .
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment(S) --> dcg_string(comment_codes1, S).
comment_codes1(L) --> "(", comment_codes2(L), ")".
comment_codes2([H|T]) --> ctext(H), !, comment_codes2(T).
comment_codes2([H|T]) --> 'quoted-pair'(H), !, comment_codes2(T).
comment_codes2(L) --> comment_codes1(L1), !, comment_codes2(L2), {append(L1, L2, L)}.
comment_codes2([]) --> "".



%! product(?Product:dict)// .
% ```abnf
% product = token ["/" product-version]
% ```

product(D) -->
  token(S1),
  (   "/", 'product-version'(S2), {D = product{name: S1, version: S2}}
  ;   {D = product{name: S1}}
  ).



%! 'prduct-version'(?ProductVersion:string)// .
% ```abnf
% 'product-version' = token
% ```

'product-version'(S) --> token(S).



%! 'quoted-string'(?String:string)// .
% ```abnf
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ```

'quoted-string'(S) --> "\"", dcg_string(quoted_string_codes, S), "\"".
quoted_string_codes([H|T]) --> qdtext(H), !, quoted_string_codes(T).
quoted_string_codes([H|T]) --> 'quoted-pair'(H), !, quoted_string_codes(T).
quoted_string_codes([]) --> "".



%! token(?Token:string)// .
% ```abnf
% token = 1*<any CHAR except CTLs or separators>
% ```

token(S) --> +(token_code, S, [convert1(codes_string)]).
token_code(C) --> 'CHAR'(C), {\+ 'CTL'(C, _, _), \+ separators(C, _, _)}.



%! value(?Value:string)// .
% ```abnf
% value = token | quoted-string
% ```

value(S) --> token(S).
value(S) --> 'quoted-string'(S).
