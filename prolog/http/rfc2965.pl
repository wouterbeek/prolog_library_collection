:- module(
  rfc2965,
  [
    'set-cookie2'//1 % -Value
  ]
).
:- reexport(library(http/rfc2109), [
     attr//1, % -Attribute:string
     'NAME'//1, % -Name:string
     'VALUE'//1, % -Value:string
     value//1 % -Value:string
   ]).

/** <module> RFC 2965: HTTP State Management Mechanism

@author Wouter Beek
@compat RFC 2965
@deprecated
@see https://tools.ietf.org/html/rfc2965
@version 2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/rfc2616_code)).





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

cookies(L) --> '+#'(cookie, L).



%! portlist(-PortNumbers:list(nonneg))// is det.
% ```abnf
% portlist = 1#portnum
% ```

portlist(L) --> '+#'(portnum, L).



%! portnum(-PortNumber:nonneg)// is det.
% ```abnf
% portnum = 1*DIGIT
% ```

portnum(N) --> '+digit'(N).



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

'set-cookie-av'(comment-V) --> "Comment=", !, value(V).
'set-cookie-av'(commentURL-V) --> "CommentURL=\"", !, http_URL(V), "\"".
'set-cookie-av'(discard) --> "Discard", !.
'set-cookie-av'(domain-V) --> "Domain=", !, value(V).
'set-cookie-av'(max_age-V) --> "Max-Age=", !, value(V).
'set-cookie-av'(path-V) --> "Path=", !, value(V).
'set-cookie-av'(port-V) --> "Port=\"", !, portlist(V), "\"".
'set-cookie-av'(port) --> "Port", !.
'set-cookie-av'(secure) --> "Secure", !.
'set-cookie-av'(version-V) --> "Version=", '+digit'(V).



%! 'set-cookie2'(-Cookies:list(dict))// .
% ```abnf
% set-cookie = "Set-Cookie2:" cookies
% ```

'set-cookie2'(L) --> cookies(L).
