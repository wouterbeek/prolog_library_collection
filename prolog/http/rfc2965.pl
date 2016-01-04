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



%! 'av-pair'(-Pair)// is det.
% ```abnf
% av-pair = attr ["=" value]   ; optional value
% ```

'av-pair'(N-V) --> attr(N), "=", !, value(V).
'av-pair'(N) --> attr(N).



%! 'av-pairs'(-Pairs:list)// .
% ```abnf
% av-pairs = av-pair *(";" av-pair)
% ```

'av-pairs'([H|T]) --> 'av-pair'(H), *(sep_av_pair, T).
sep_av_pair(X) --> ";", ?('LWS'), 'av-pair'(X).



%! cookie(-Cookie:dict)// is det.
% ```abnf
% cookie = NAME "=" VALUE *(";" set-cookie-av)
% ```

cookie(cookie{name: Name, value: Value, pairs: L}) -->
  'NAME'(Name),
  "=",
  'VALUE'(Value),
  *(sep_set_cookie_av, L).
sep_set_cookie_av(X) --> ";", ?('LWS'), 'set-cookie-av'(X).



%! cookies(-Cookies:list(dict))// .
% ```abnf
% cookies = 1#cookie
% ```

cookies(L) --> +#(cookie, L).



%! 'NAME'(-Name:string)// is det.
% ```abnf
% NAME = attr
% ```

'NAME'(S) --> attr(S).



%! portlist(-PortNumbers:list(nonneg))// is det.
% ```abnf
% portlist = 1#portnum
% ```

portlist(L) --> +#(portnum, L).



%! portnum(-PortNumber:nonneg)// is det.
% ```abnf
% portnum = 1*DIGIT
% ```

portnum(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'set-cookie-av'(-Pair:pair)// is det.
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

'set-cookie-av'(comment-V) --> atom_ci('Comment'), "=", !, value(V).
'set-cookie-av'(commentURL-V) -->
  atom_ci('CommentURL'),
  "=\"", !,
  http_URL(V),
  "\"".
'set-cookie-av'(discard) --> atom_ci('Discard'), !.
'set-cookie-av'(domain-V) --> atom_ci('Domain'), "=", !, value(V).
'set-cookie-av'(max_age-V) --> atom_ci('Max-Age'), "=", !, value(V).
'set-cookie-av'(path-V) --> atom_ci('Path'), "=", !, value(V).
'set-cookie-av'(port-V) --> atom_ci('Port'), "=\"", !, portlist(V), "\"".
'set-cookie-av'(port) --> atom_ci('Port'), !.
'set-cookie-av'(secure) --> atom_ci('Secure'), !.
'set-cookie-av'(version-V) -->
  atom_ci('Version'),
  "=",
  +('DIGIT', Ds),
  {pos_sum(Ds, V)}.



%! 'set-cookie2'(-Cookies:list(dict))// .
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
