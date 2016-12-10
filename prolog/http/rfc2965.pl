:- module(
  rfc2965,
  [
    'set-cookie2'//1 % -Val
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





%! attr(-Attr:atom)// is det.
%
% ```abnf
% attr = token
% ```

attr(Token) -->
  token(Token).



%! 'av-pair'(-Param:pair(atom))// is det.
%
% ```abnf
% av-pair = attr ["=" value]   ; optional value
% ```

'av-pair'(Key-Val) -->
  attr(Key),
  ("=" -> value(Val) ; {Val = true}).



%! 'av-pairs'(-Params:list(pair(atom)))// is det.
%
% ```abnf
% av-pairs = av-pair *(";" av-pair)
% ```

'av-pairs'([H|T]) -->
  'av-pair'(H),
  *(sep_av_pair, T), !.

sep_av_pair(D) -->
  ";",
  ?('LWS'),
  'av-pair'(D).



%! cookie(-Cookie:compound)// is det.
%
% ```abnf
% cookie = NAME "=" VALUE *(";" set-cookie-av)
% ```

cookie(cookie(Key,Val,Params)) -->
  'NAME'(Key),
  "=",
  'VALUE'(Val),
  *(sep_set_cookie_av, Params), !.

sep_set_cookie_av(Param) -->
  ";",
  ?('LWS'),
  'set-cookie-av'(Param).



%! cookies(-Cookies:list(compound))// is det.
%
% ```abnf
% cookies = 1#cookie
% ```

cookies(Cookies) -->
  +#(cookie, Cookies), !.



%! 'NAME'(-Name:atom)// is det.
%
% ```abnf
% NAME = attr
% ```

'NAME'(Name) -->
  attr(Name).



%! portlist(-Ports:list(nonneg))// is det.
%
% ```abnf
% portlist = 1#portnum
% ```

portlist(Ports) -->
  +#(portnum, Ports), !.



%! portnum(-Port:nonneg)// is det.
%
% ```abnf
% portnum = 1*DIGIT
% ```

portnum(Port) -->
  +('DIGIT', Ds), !,
  {pos_sum(Ds, Port)}.



%! 'set-cookie-av'(-Param:pair)// is det.
%
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

'set-cookie-av'(comment-Comment) -->
  atom_ci('Comment'), !,
  "=",
  value(Comment).
'set-cookie-av'(commentURL-Uri) -->
  atom_ci('CommentURL'), !,
  "=\"",
  http_URL(Uri),
  "\"".
'set-cookie-av'(discard-true) -->
  atom_ci('Discard'), !.
'set-cookie-av'(domain-Domain) -->
  atom_ci('Domain'), !,
  "=",
  value(Domain).
'set-cookie-av'('max-age'-MaxAge) -->
  atom_ci('Max-Age'), !,
  "=",
  value(MaxAge).
'set-cookie-av'(path-Path) -->
  atom_ci('Path'), !,
  "=",
  value(Path).
'set-cookie-av'(port-Ports) -->
  atom_ci('Port'), !,
  "=\"",
  portlist(Ports), "\"".
'set-cookie-av'(port-true) -->
  atom_ci('Port'), !.
'set-cookie-av'(secure-true) -->
  atom_ci('Secure'), !.
'set-cookie-av'(version-Version) -->
  atom_ci('Version'), !,
  "=",
  +('DIGIT', Ds), !,
  {pos_sum(Ds, Version)}.



%! 'set-cookie2'(-Cookies:list(compound))// is det.
%
% ```abnf
% set-cookie = "Set-Cookie2:" cookies
% ```

'set-cookie2'(Cookies) -->
  cookies(Cookies).



%! value(-Val:atom)// is det.
%
% ```abnf
% value = token | quoted-string
% ```

value(Token) -->
  token(Token), !.
value(Str) -->
  'quoted-string'(Str).



%! 'VALUE'(-Val:atom)// is det.
%
% ```abnf
% VALUE = value
% ```

'VALUE'(Val) -->
  value(Val).
