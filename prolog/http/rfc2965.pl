:- module(
  rfc2965,
  [
    'set-cookie2'//1 % -Value
  ]
).

/** <module> RFC 2965: HTTP State Management Mechanism

@author Wouter Beek
@compat RFC 2965
@deprecated
@see https://tools.ietf.org/html/rfc2965
@version 2015/12-2016/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/rfc2616), [
     'DIGIT'//1,
     'DIGIT'//2,
     http_URL//1,
     token//1
   ]).





%! attr(-Attribute:string)// is det.
% ```abnf
% attr = token
% ```

attr(S) --> token(S).



%! 'av-pair'(-Parameter:dict)// is det.
% ```abnf
% av-pair = attr ["=" value]   ; optional value
% ```

'av-pair'(D2) -->
  attr(Key),
  {D1 = _{'@type': 'llo:parameter', 'llo:key': Key}},
  ("=" -> value(Value), {D2 = D1.put(_{'llo:value': Value})} ; {D2 = D1}).



%! 'av-pairs'(-Parameters:list(dict))// is det.
% ```abnf
% av-pairs = av-pair *(";" av-pair)
% ```

'av-pairs'([H|T]) --> 'av-pair'(H), *(sep_av_pair, T).

sep_av_pair(D) --> ";", ?('LWS'), 'av-pair'(D).



%! cookie(-Cookie:dict)// is det.
% ```abnf
% cookie = NAME "=" VALUE *(";" set-cookie-av)
% ```

cookie(_{'@type': 'llo:cookie', 'llo:key': Key, 'llo:value': Value, 'llo:parameters': L}) -->
  'NAME'(Key), "=", 'VALUE'(Value), *(sep_set_cookie_av, L).

sep_set_cookie_av(X) --> ";", ?('LWS'), 'set-cookie-av'(X).



%! cookies(-Cookies:list(dict))// is det.
% ```abnf
% cookies = 1#cookie
% ```

cookies(L) --> +#(cookie, L).



%! 'NAME'(-Name:string)// is det.
% ```abnf
% NAME = attr
% ```

'NAME'(S) --> attr(S).



%! portlist(-Ports:list(nonneg))// is det.
% ```abnf
% portlist = 1#portnum
% ```

portlist(L) --> +#(portnum, L).



%! portnum(-Port:nonneg)// is det.
% ```abnf
% portnum = 1*DIGIT
% ```

portnum(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'set-cookie-av'(-Parameter:dict)// is det.
% ```abnf
% set-cookie-av = "Comment" "=" value
%               | "CommentURL" "=" <"> http_URL <">
%               | "Discard"
%               | "Domain" "=" value
%               | "Max-Age" "=" value
%               | "Path" "=" value
%               | "Port" [ "=" <"> portlist <"> ]
%               | "Secure"
%               | "Version" "=" 1*DIGIT

'set-cookie-av'(_{'@type': 'llo:parameter', 'llo:key': Key, 'llo:value': Value}) -->
  set_cookie_av(Key, Value).

set_cookie_av("comment", S)    --> atom_ci('Comment'), "=", !, value(S).
set_cookie_av("commentURL", D) --> atom_ci('CommentURL'), "=\"", !, http_URL(D), "\"".
set_cookie_av("discard", true) --> atom_ci('Discard'), !.
set_cookie_av("domain", S)     --> atom_ci('Domain'), "=", !, value(S).
set_cookie_av("max-age", S)    --> atom_ci('Max-Age'), "=", !, value(S).
set_cookie_av("path", S)       --> atom_ci('Path'), "=", !, value(S).
set_cookie_av("port", L)       --> atom_ci('Port'), "=\"", !, portlist(L), "\"".
set_cookie_av("port", true)    --> atom_ci('Port'), !.
set_cookie_av("secure", true)  --> atom_ci('Secure'), !.
set_cookie_av("version", N)    --> atom_ci('Version'), "=", +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'set-cookie2'(-Cookies:list(dict))// is det.
% ```abnf
% set-cookie = "Set-Cookie2:" cookies
% ```

'set-cookie2'(L) --> cookies(L).



%! value(-Value:string)// is det.
% ```abnf
% value = token | quoted-string
% ```

value(S) --> token(S), !.
value(S) --> 'quoted-string'(S).



%! 'VALUE'(-Value:string)// is det.
% ```abnf
% VALUE = value
% ```

'VALUE'(S) --> value(S).
