:- module(
  rfc2616_token,
  [
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
