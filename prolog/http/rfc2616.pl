:- module(
  rfc2616,
  [
    comment//0,
    http_URL//4, % ?Host:or([list(nonneg),list(string)])
                 % ?Port:nonneg
                 % ?Path:list(string)
                 % ?Query:string
    'HTTP-Version'//2 % ?Major:nonneg
                      % ?Minor:nonneg
  ]
).

/** <module> RFC 2616: Hypertext Transfer Protocol -- HTTP/1.1

@author Wouter Beek
@compat RFC 2616
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(uri/rfc2396)).
:- use_module(library(uri/rfc2396_token)).





%! comment//0 .
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment -->
  "(", comment0, ")".
comment0 --> ctext(_), comment0.
comment0 --> 'quoted-pair'(_), comment0.
comment0 --> comment, comment0.
comment0 --> "".



%! http_URL(
%!   ?Host:or([list(nonneg),list(string)]),
%!   ?Port:nonneg,
%!   ?Path:list(string),
%!   ?Query:string
%! )// .
% Port defaults to 80.
% Path defaults to `/`.
%
% ```abnf
% http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
% ```

http_URL(Host, Port, Path, Query) -->
  "http://",
  host(Host),
  (":", port(Port) ; {Port = 80}),
  (abs_path(Path), ("?", query(Query) ; "") ; {Path = []}).



%! 'HTTP-Version'(?Major:nonneg, ?Minor:nonneg)// .
% ```abnf
% HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ```

'HTTP-Version'(X, Y) -->
  "HTTP/",
  +('DIGIT', X, [convert(1-positional)]),
  ".",
  +('DIGIT', Y, [convert(1-positional)]).
