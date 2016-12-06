:- module(
  rfc2617,
  [
    'auth-scheme'//1, % -Scheme:atom
    'auth-param'//1,  % -Param:pair
    challenge//1      % -Challenge:compound
  ]
).

/** <module> RFC 2617: HTTP Authentication

@author Wouter Beek
@deprecated
@see https://tools.ietf.org/html/rfc2617
@version 2015/11-2016/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'SP'//0
   ]).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/rfc2616), [
     'quoted-string'//1, % -String:atom
     token//1            % -Token:atom
   ]).





%! 'auth-scheme'(-Scheme:atom)// is det.
%
% ```abnf
% auth-scheme = token
% ```

'auth-scheme'(Scheme) -->
  token(Scheme).



%! 'auth-param'(-Param:pair(atom))// is det.
%
% ```abnf
% auth-param = token "=" ( token | quoted-string )
% ```

'auth-param'(Key-Val) -->
  token(Key),
  "=",
  (token(Val) -> "" ; 'quoted-string'(Val)).



%! challenge(-Challenge:compound)// is det.
%
% ```abnf
% challenge = auth-scheme 1*SP 1#auth-param
% ```

challenge(challenge(Scheme,Params)) -->
  'auth-scheme'(Scheme),
  +('SP'), !,
  +#('auth-param', Params), !.
