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

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc7230_code)).





%! 'quoted-string'(?String:string)// .
% ```abnf
% quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE
% ```

'quoted-string'(S) -->
  "\"", *(quoted_string_code, Cs), "\"",
  {string_codes(S, Cs)}.
quoted_string_code(C) --> qdtext(C).
quoted_string_code(C) --> 'quoted-pair'(C).



%! token(?Token:string)// .
% ```abnf
% token = 1*tchar
% ```
%
% @compat RFC 7230

token(S) --> +(tchar, Cs), {string_codes(S, Cs)}.
