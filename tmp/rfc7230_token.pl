:- module(
  rfc7230_token,
  [
    'quoted-string'//1, % ?String:string
    token//1 % ?String:string
  ]
).

/** <module> RFC 7230: Tokens

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(http/rfc7230_code)).
:- use_module(library(string_ext)).





%! 'quoted-string'(?String:string)// .
% ```abnf
% quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE
% ```

'quoted-string'(S) --> dcg_string(quoted_string_codes1, S).
quoted_string_codes1(Cs) --> 'DQUOTE', quoted_string_codes2(Cs), 'DQUOTE'.
quoted_string_codes2([H|T]) --> qdtext(H), !, quoted_string_codes2(T).
quoted_string_codes2([H|T]) --> 'quoted-pair'(H), !, quoted_string_codes2(T).
quoted_string_codes2([]) --> "".



%! token(?Token:string)// .
% ```abnf
% token = 1*tchar
% ```
%
% @compat RFC 7230

token(S) --> +(tchar, S, [convert1(codes_string)]).
