:- module(
  rfc2109,
  [
    'set-cookie'//1 % ?Cookies:list(list(pair))
  ]
).

/** <module> RFC 2109

Grammar

```abnf
set-cookie = "Set-Cookie:" cookies
cookies = 1#cookie
cookie = NAME "=" VALUE *(";" cookie-av)
NAME = attr
attr = token
VALUE = value
cookie-av = "Comment" "=" value
          | "Domain" "=" value
          | "Max-Age" "=" value
          | "Path" "=" value
          | "Secure"
          | "Version" "=" 1*DIGIT
value = word
word = token | quoted-string
```

# Common mistakes

Comma's in Set-Cookie values, especially in dates.  This is not allowed since
comma is used as a separator in `#' and `token' is not allowed to contain a
comma.  Example:

```http
__Host-js_csrf=H9teNf8adQDrvSiROZyOCs0N; expires=Wed, 19 Dec 2018 18:04:52 GMT; Path=/; secure
```

@author Wouter Beek
@compat RFC 2109
@deprecated
@see https://tools.ietf.org/html/rfc2109
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/rfc2616), ['LWS'//0,'quoted-string'//1,token//1]).





%! attr(?Attribute:string)// .
% ```abnf
% attr = token
% ```

attr(S) --> token(S).



%! cookie(?Cookie:list(pair))// .
% ```abnf
% cookie = NAME "=" VALUE *(";" cookie-av)
% ```

cookie([N-V|T]) -->
  'NAME'(N), "=", 'VALUE'(V),
  *(sep_cookie_av, T).
sep_cookie_av(X) --> ";", ?('LWS'), 'cookie-av'(X).



%! cookies(?Cookies:list(list(pair)))// .
% ```abnf
% cookies = 1#cookie
% ```

cookies(L) --> '+#'(cookie, L).



%! 'cookie-av'(or([oneof([secure]),pair(atom,string)]))// .
% ```abnf
% cookie-av = "Comment" "=" value
%           | "Domain" "=" value
%           | "Max-Age" "=" value
%           | "Path" "=" value
%           | "Secure"
%           | "Version" "=" 1*DIGIT
% ```

'cookie-av'(comment-V) --> "Comment=", !, value(V).
'cookie-av'(domain-V)  --> "Domain=", !, value(V).
'cookie-av'(max_age-V) --> "Max-age=", !, value(V).
'cookie-av'(path-V)    --> "Path=", !, value(V).
'cookie-av'(secure)    --> "Secure", !.
'cookie-av'(version-V) --> "Version=", +(digit, Ds), {pos_sum(Ds, V)}.



%! 'NAME'(?Name:string)// .
% ```abnf
% NAME = attr
% ```

'NAME'(S) --> attr(S).



%! 'set-cookie'(?Cookies:list(list(pair)))// .
% ```abnf
% set-cookie = "Set-Cookie:" cookies
% ```

'set-cookie'(L) --> cookies(L).



%! 'VALUE'(?Value:string)// .
% ```abnf
% VALUE = value
% ```

'VALUE'(S) --> value(S).



%! value(?Value:string)// .
% ```abnf
% value = word
% ```

value(S) --> word(S).



%! word(?Word:string)// .
% ```abnf
% word = token | quoted-string
% ```

word(S) --> token(S), !.
word(S) --> 'quoted-string'(S).
