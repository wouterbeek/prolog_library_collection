:- module(
  rfc2617,
  [
    'auth-scheme'//1, % -Scheme:string
    'auth-param'//1,  % -Parameter:dict
    challenge//1      % -Challenge:dict
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
     'quoted-string'//1, % -String:string
     token//1            % -Token:string
   ]).





%! 'auth-scheme'(-Scheme:string)// is det.
% ```abnf
% auth-scheme = token
% ```

'auth-scheme'(S) --> token(S).



%! 'auth-param'(-Parameter:dict)// is det.
% ```abnf
% auth-param = token "=" ( token | quoted-string )
% ```

'auth-param'(_{'@type': 'llo:parameter', 'llo:key': Key, 'llo:value': Value}) -->
  token(Key), "=", (token(Value) ; 'quoted-string'(Value)).



%! challenge(-Challenge:dict)// is det.
% ```abnf
% challenge = auth-scheme 1*SP 1#auth-param
% ```

challenge(_{'@type': 'llo:challenge', 'llo:scheme': Scheme, 'llo:parameters': L}) -->
  'auth-scheme'(Scheme), +('SP'), +#('auth-param', L).
