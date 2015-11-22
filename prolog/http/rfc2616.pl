:- module(
  rfc2616,
  [
    comment//0,
    'generic-message'//4, % ?Version:pair(nonneg)
                          % ?Status:between(100,599)
                          % ?Headers:list(pair(string,compound))
                          % ?Body:list(code)
    http_URL//4, % ?Host:or([list(nonneg),list(string)])
                 % ?Port:nonneg
                 % ?Path:list(string)
                 % ?Query:string
    'HTTP-message'//0,
    'HTTP-Version'//1, % ?Version:pair(nonneg)
    'start-line'//2 % ?Version:pair(nonneg)
                    % ?Status:between(100,599)
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
:- use_module(library(dcg/dcg_re)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(uri/rfc2396)).
:- use_module(library(uri/rfc2396_token)).





%! comment// .
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment --> "(", comment0, ")".
comment0 --> ctext(_),         !, comment0.
comment0 --> 'quoted-pair'(_), !, comment0.
comment0 --> comment,          !, comment0.
comment0 --> "".



%! 'generic-message'(
%!   ?Version:pair(nonneg),
%!   ?Status:between(100,599),
%!   ?Headers:list(pair(string,compound)),
%!   ?Body:list(code)
%! )// .
% ```abnf
% generic-message = start-line
%                   *(message-header CRLF)
%                   CRLF
%                   [ message-body ]
% ```

'generic-message'(Version, Status, Headers, Body) -->
  'start-line'(Version, Status),
  meassage_headers(Headers),
  'CRLF',
  ('message-body'(Body) ; {Body = []}).



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



%! 'HTTP-message'// .
% ```abnf
% HTTP-message = Request | Response   ; HTTP/1.1 messages
% ```

'HTTP-message' --> 'Request'.
'HTTP-message' --> 'Response'.



%! 'HTTP-Version'(?Version:pair(nonneg))// .
% ```abnf
% HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ```

'HTTP-Version'(X-Y) --> "HTTP/", '+digit'(X), ".", '+digit'(Y).



%! 'start-line'(?Version:pair(nonneg), ?Status:between(100,599))// .
% ```abnf
% start-line = Request-Line | Status-Line
% ```

'start-line'(Version, Status) --> 'Request-Line'(Version, Status).
'start-line'(Version, Status) --> 'Status-Line'(Version, Status).
