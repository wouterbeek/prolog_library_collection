:- module(
  rfc7235,
  [
  ]
).

/** <module> RFC 7232 - HTTP/1.1: Authentication

@author Wouter Beek
@compat RFC 7235
@see https://tools.ietf.org/html/rfc7235
@version 2017/05-2017/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(http/rfc7230)).

:- discontiguous
    http:http_header/1,
    http:http_separable/1.

:- dynamic
    http:http_header/1,
    http:http_separable/1.

:- multifile
    http:http_header/1,
    http:http_separable/1.





%! 'auth-param'(-Pair:pair(atom))// .
%
% ```abnf
% auth-param = token BWS "=" BWS ( token | quoted-string )
% ```

'auth-param'(Key-Val) -->
  token(Key),
  'BWS', "=", 'BWS',
  (token(Val), ! ; 'quoted-string'(Val)).



%! 'auth-scheme'(-Scheme:atom)// .
%
% ```abnf
% auth-scheme = token
% ```

'auth-scheme'(Scheme) -->
  token(Scheme).



%! authorization(-Credentials:list(pair(atom,list(pair(atom)))))// .
%
% ```abnf
% Authorization = credentials
% ```

http:http_header(authorization).
authorization(Credentials) -->
  credentials(Credentials).



%! challenge(-Challenge:pair(atom,list(pair(atom))))// .
%
% ```abnf
% challenge = auth-scheme [ 1*SP ( token68 | #auth-param ) ]
% ```

challenge(Scheme-Params) -->
  'auth-scheme'(Scheme),
  (   +('SP')
  ->  (   *##('auth-param', Params)
      ->  ""
      ;   token68(Param),
          {Params = [Param]}
      )
  ;   {Params = []}
  ).



%! credentials(-Credentials:pair(atom,list(pair(atom))))// .
%
% ```abnf
% credentials = auth-scheme [ 1*SP ( token68 | #auth-param ) ]
% ```

credentials(Scheme-Params) -->
  'auth-scheme'(Scheme),
  (   +('SP')
  ->  (   token68(Token)
      ->  {Params = [Token]}
      ;   *##('auth-param', Params)
      )
  ;   {Params = []}
  ).



%! 'proxy-authenticate'(-Challenges:list(pair(atom,list(pair(atom)))))// .
%
% ```abnf
% Proxy-Authenticate = 1#challenge
% ```

http:http_header('proxy-authenticate').
http:http_separable('proxy-authenticate').
'proxy-authenticate'(Challenges) -->
  +##(challenge, Challenges).



%! 'proxy-authorization'(-Credentials:pair(atom,list(pair(atom))))// .
%
% ```abnf
% Proxy-Authorization = credentials
% ```

http:http_header('proxy-authorization').
'proxy-authorization'(Credentials) -->
  credentials(Credentials).



%! token68(-Token:atom)// .
%
% ```abnf
% token68 = 1*( ALPHA | DIGIT | "-" | "." | "_" | "~" | "+" | "/" ) *"="
% ```

token68(Token) -->
  +(token68_code, Cs), !,
  {atom_codes(Token, Cs)},
  *("="), !.

token68_code(C)   --> 'ALPHA'(C).
token68_code(C)   --> 'DIGIT'(_, C).
token68_code(0'-) --> "-".
token68_code(0'.) --> ".".
token68_code(0'_) --> "_".
token68_code(0'~) --> "~".
token68_code(0'+) --> "+".
token68_code(0'/) --> "/".



%! 'www-authenticate'(-Challenges:list(compound))// .
%
% ```abnf
% WWW-Authenticate = 1#challenge
% ```

http:http_header('www-authenticate').
http:http_separable('www-authenticate').
'www-authenticate'(Challenges) -->
  +##(challenge, Challenges).
