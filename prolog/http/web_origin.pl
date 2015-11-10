:- module(
  web_origin,
  [
  ]
).

/** <module> Web Origin

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(uri/rfc3986)).





%! origin// .
% ```abnf
% origin = "Origin:" OWS origin-list-or-null OWS
% ```

origin --> "Origin:", 'OWS', 'origin-list-or-null', 'OWS'.



%! 'origin-list-or-null'(?Origins:list(dict))// .
% ```abnf
% origin-list-or-null = %x6E %x75 %x6C %x6C / origin-list
% ```

'origin-list-or-null'([]) -->
  code_radix(hex('6E')),
  code_radix(hex('75')),
  code_radix(hex('6C')),
  code_radix(hex('6C')).
'origin-list-or-null'(L) --> 'origin-list'(L).



%! 'origin-list'(?Origins:list(dict))// .
% ```abnf
% origin-list = serialized-origin *( SP serialized-origin )
% ```

'origin-list' = +('serialized-origin', L, [separator('SP')]).



%! 'serialized-origin'(?Origin:dict)// .
% ```abnf
% serialized-origin = scheme "://" host [ ":" port ]
%                   ; <scheme>, <host>, <port> from RFC 3986
% ```

'serialized-origin'(origin{scheme: Scheme, host: Host, port: Port}) -->
  scheme(Scheme),
  "://",
  host(Host),
  (":", port(Port) ; {Port = 80}).
