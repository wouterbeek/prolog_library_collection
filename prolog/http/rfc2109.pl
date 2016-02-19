:- module(
  rfc2109,
  [
    attr//1,        % -Attribute:string
    'set-cookie'//1 % -Cookies:list(dict)
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
:- use_module(library(http/rfc2616), ['LWS'//0,'quoted-string'//1,token//1]).





%! attr(-Attribute:string)// is det.
% ```abnf
% attr = token
% ```

attr(S) --> token(S).



%! cookie(-Cookie:dict)// is det.
% ```abnf
% cookie = NAME "=" VALUE *(";" cookie-av)
% ```

cookie(_{'@type': 'llo:cookie', 'llo:key': Key, 'llo:value': Value, 'llo:parameters': L}) -->
  'NAME'(Key), "=", 'VALUE'(Value), *(sep_cookie_av, L).

sep_cookie_av(D) --> ";", ?('LWS'), 'cookie-av'(D).



%! 'cookie-av'(-Parameter:dict)// is det.
% ```abnf
% cookie-av = "Comment" "=" value
%           | "Domain" "=" value
%           | "Max-Age" "=" value
%           | "Path" "=" value
%           | "Secure"
%           | "Version" "=" 1*DIGIT
% ```

'cookie-av'(_{'@type': 'llo:parameter', 'llo:key': Key, 'llo:value': Value}) -->
  cookie_av(Key, Value).

cookie_av("comment", S)   --> atom_ci('Comment='), !, value(S).
cookie_av("domain", S)    --> atom_ci('Domain='), !, value(S).
cookie_av("max-age", S)   --> atom_ci('Max-age='), !, value(S).
cookie_av("path", S)      --> atom_ci('Path='), !, value(S).
cookie_av("secure", true) --> atom_ci('Secure'), !.
cookie_av("version", N)   --> atom_ci('Version='), +(digit, Ds), {pos_sum(Ds, N)}.



%! cookies(-Cookies:list(dict))// is det.
% ```abnf
% cookies = 1#cookie
% ```

cookies(L) --> '+#'(cookie, L).



%! 'NAME'(-Name:string)// is det.
% ```abnf
% NAME = attr
% ```

'NAME'(S) --> attr(S).



%! 'set-cookie'(-Cookies:list(dict))// is det.
% ```abnf
% set-cookie = "Set-Cookie:" cookies
% ```

'set-cookie'(L) --> cookies(L).



%! 'VALUE'(-Value:string)// is det.
% ```abnf
% VALUE = value
% ```

'VALUE'(S) --> value(S).



%! value(-Value:string)// is det.
% ```abnf
% value = word
% ```

value(S) --> word(S).



%! word(-Word:string)// is det.
% ```abnf
% word = token | quoted-string
% ```

word(S) --> token(S), !.
word(S) --> 'quoted-string'(S).
