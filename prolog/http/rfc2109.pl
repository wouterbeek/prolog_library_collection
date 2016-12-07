:- module(
  rfc2109,
  [
    attr//1,        % -Attr:atom
    'set-cookie'//1 % -Cookies:list(compound)
  ]
).

/** <module> RFC 2109

@author Wouter Beek
@compat RFC 2109
@deprecated
@see https://tools.ietf.org/html/rfc2109
@version 2015/11-2016/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/rfc2616), [
     'LWS'//0,
     'quoted-string'//1, % -String:atom
     token//1            % -Token:atom
   ]).

:- dynamic
    http_known_known/1.

:- multifile
    http_known_known/1.





%! attr(-Attr:atom)// is det.
%
% ```abnf
% attr = token
% ```

attr(Attr) -->
  token(Attr).



%! cookie(-Cookie:compound)// is det.
%
% ```abnf
% cookie = NAME "=" VALUE *(";" cookie-av)
% ```

cookie(cookie(Key,Val,Params)) -->
  'NAME'(Key),
  "=",
  'VALUE'(Val),
  *(sep_cookie_av, Params), !.

sep_cookie_av(Param) -->
  ";",
  ?('LWS'),
  'cookie-av'(Param).



%! 'cookie-av'(-Param:pair(atom))// is det.
%
% ```abnf
% cookie-av = "Comment" "=" value
%           | "Domain" "=" value
%           | "Max-Age" "=" value
%           | "Path" "=" value
%           | "Secure"
%           | "Version" "=" 1*DIGIT
% ```

'cookie-av'(comment-Comment) -->
  atom_ci('Comment'), !,
  "=",
  value(Comment).
'cookie-av'(domain-Domain) -->
  atom_ci('Domain'), !,
  "=",
  value(Domain).
'cookie-av'('max-age'-MaxAge) -->
  atom_ci('Max-age'), !,
  "=",
  value(MaxAge).
'cookie-av'(path-Path) -->
  atom_ci('Path'), !,
  "=",
  value(Path).
'cookie-av'(secure-true) -->
  atom_ci('Secure'), !.
'cookie-av'(version-Version) -->
  atom_ci('Version'), !,
  "=",
  +(digit, Ds), !,
  {pos_sum(Ds, Version)}.



%! cookies(-Cookies:list(compound))// is det.
%
% ```abnf
% cookies = 1#cookie
% ```

cookies(Cookies) -->
  '+#'(cookie, Cookies), !.



%! 'NAME'(-Name:atom)// is det.
%
% ```abnf
% NAME = attr
% ```

'NAME'(Name) -->
  attr(Name).



%! 'set-cookie'(-Cookies:list(compound))// is det.
%
% ```abnf
% set-cookie = "Set-Cookie:" cookies
% ```

http_known_known('set-cookie').
'set-cookie'(Cookies) -->
  cookies(Cookies).



%! 'VALUE'(-Val:atom)// is det.
%
% ```abnf
% VALUE = value
% ```

'VALUE'(Val) -->
  value(Val).



%! value(-Val:atom)// is det.
%
% ```abnf
% value = word
% ```

value(Val) -->
  word(Val).



%! word(-Word:atom)// is det.
%
% ```abnf
% word = token | quoted-string
% ```

word(Token) -->
  token(Token), !.
word(Str) -->
  'quoted-string'(Str).
