:- module(
  rfc6454,
  [
    'obs-fold'//1, % ?Codes:list(code)
    'OWS'//1, % ?Codes:list(code)
    origin//1,
    'origin-list-or-null'//1,
    'origin-list'//1
  ]
).

/** <module> RFC 6454: Web Origin

# Definitions


---

@author Wouter Beek
@compat RFC 6454
@license MIT License
@see http://tools.ietf.org/html/rfc6454
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(uri/rfc3986)).





%! 'obs-fold'(?Codes:list(code))// .
% ```abnf
% obs-fold = CRLF ( SP / HTAB )   ; obsolete line folding
% ```

'obs-fold'([H1,H2,H3]) --> 'CRLF'([H1,H2]), ('SP'(H3) ; 'HTAB'(H3)).



%! 'OWS'// .
%! 'OWS'(?Codes:list(code))// .
% ```abnf
% OWS = *( SP / HTAB / obs-fold )   ; "optional" whitespace
% ```

'OWS' --> 'OWS'(_).
'OWS'(L) = *(ows_code, L, []).
ows_code(C) --> 'SP'(C).
ows_code(C) --> 'HTAB'(C).
ows_code(C) --> 'obs-fold'(C).



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
