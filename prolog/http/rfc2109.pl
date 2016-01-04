:- module(
  rfc2109,
  [
    attr//1, % -Attribute:string
    'set-cookie'//1 % -Cookies:list(list(pair))
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



%! cookie(-Cookie:list(pair))// is det.
% ```abnf
% cookie = NAME "=" VALUE *(";" cookie-av)
% ```

cookie([N-V|T]) -->
  'NAME'(N), "=", 'VALUE'(V),
  *(sep_cookie_av, T).
sep_cookie_av(X) --> ";", ?('LWS'), 'cookie-av'(X).



%! 'cookie-av'(-Pair:pair)// is det.
% ```abnf
% cookie-av = "Comment" "=" value
%           | "Domain" "=" value
%           | "Max-Age" "=" value
%           | "Path" "=" value
%           | "Secure"
%           | "Version" "=" 1*DIGIT
% ```

'cookie-av'(comment-V)   --> atom_ci('Comment='), !, value(V).
'cookie-av'(domain-V)    --> atom_ci('Domain='), !, value(V).
'cookie-av'(max_age-V)   --> atom_ci('Max-age='), !, value(V).
'cookie-av'(path-V)      --> atom_ci('Path='), !, value(V).
'cookie-av'(secure-true) --> atom_ci('Secure'), !.
'cookie-av'(version-V)   --> atom_ci('Version='), +(digit, Ds), {pos_sum(Ds, V)}.



%! cookies(-Cookies:list(list(pair)))// is det.
% ```abnf
% cookies = 1#cookie
% ```

cookies(L) --> '+#'(cookie, L).



%! 'NAME'(-Name:string)// is det.
% ```abnf
% NAME = attr
% ```

'NAME'(S) --> attr(S).



%! 'set-cookie'(-Cookies:list(list(pair)))// is det.
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
