:- module(
  rfc2617,
  [
    'auth-scheme'//1, % ?AuthenticationScheme:string
    'auth-param'//1, % ?AuthenticationParameter:pair(string)
    challenge//1 % ?Challenge:dict
  ]
).

/** <module> RFC 2617: HTTP Authentication

@author Wouter Beek
@deprecated
@see https://tools.ietf.org/html/rfc2617
@version 2015/11
*/

:- use_module(library(dcg/dcg_re)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_helpers)).
:- use_module(library(http/rfc2616_token)).





%! 'auth-scheme'(?AuthenticationScheme:string)// .
% ```abnf
% auth-scheme = token
% ```

'auth-scheme'(S) --> token(S).



%! 'auth-param'(?AuthenticationParameter:pair(string))// .
% ```abnf
% auth-param = token "=" ( token | quoted-string )
% ```

'auth-param'(N-V) --> token(N), "=", (token(V) ; 'quoted-string'(V)).



%! challenge(?Challenge:dict)// .
% ```abnf
% challenge   = auth-scheme 1*SP 1#auth-param
% ```

challenge(challenge{scheme: AuthScheme, parameters: AuthParams}) -->
  'auth-scheme'(AuthScheme),
  +('SP'),
  +#('auth-param', AuthParams).
