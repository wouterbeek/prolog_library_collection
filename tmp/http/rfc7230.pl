:- module(
  rfc7230,
  [
    'Connection'//1, % ?Options:list(compound)
    'Content-Length'//1, % ?ContentLength:nonneg
    'HTTP-message'//3, % ?StartLine
                       % ?HeaderFields:list(compound)
                       % ?Body:list(code)
    'HTTP-name'//0,
    'HTTP-version'//2, % ?Major:between(0,9)
                       % ?Minor:between(0,9)
    'Host'//2, % ?Host:compound
               % ?Port:nonneg
    'uri-host'//1 % ?Host:compound
  ]
).

/** <module> RFC 7230: Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing

@author Wouter Beek
@compat RFC 7230
@see http://tools.ietf.org/html/rfc7230
@version 2015/11
*/

:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(http/rfc7230_code)).
:- use_module(library(http/rfc7230_token)).
:- use_module(library(uri/rfc3986_component)).





%! 'Connection'(?Options:list)// .
% ```abnf
% Connection = *( "," OWS )
%              connection-option
%              *( OWS "," [ OWS connection-option ] )
% ```

'Connection'([H|T]) -->
  ows1,
  'connection-option'(H),
  connection_options(T).
ows1 --> ",", 'OWS', ows1.
ows1 --> "".
connection_options([H|T]) -->
  'OWS',
  ",",
  ('OWS', 'connection-option'(H) ; ""),
  connection_options(T).
connection_options([]).



%! 'Content-Length'(?ContentLength:nonneg)// .
% ```abnf
% Content-Length = 1*DIGIT
% ```

'Content-Length'(I) --> +('DIGIT', Ds), {pos_sum(Ds, I)}.



%! 'HTTP-message(?X, ?Y:list(compound), ?Z:list(code))// .
% ```abnf
% HTTP-message = start-line *( header-field CRLF ) CRLF [ message-body ]
% ```

'HTTP-message'(X, Y, Z) -->
  'start-line'(X),
  header_fields(Y),
  'CRLF',
  ('message-body'(Z) ; {Z = []}).
header_fields([H|T]) --> 'header-field'(H), !, 'CRLF', header_fields(T).
header_fields([]) --> "".



%! 'HTTP-name'// .
% ```abnf
% HTTP-name = %x48.54.54.50 ; HTTP
% ```

'HTTP-name' --> "HTTP".



%! 'HTTP-version'(?Major:between(0,9), ?Minor:between(0,9))// .
% ```abnf
% HTTP-version = HTTP-name "/" DIGIT "." DIGIT
% ```

'HTTP-version'(X, Y) --> 'HTTP-name', "/", digit(X), ".", digit(Y).



%! 'Host'(?Host, ?Port)// .
% ```abnf
% Host = uri-host [ ":" port ]
% ```

'Host'(Host, Port) --> 'uri-host'(Host), (":", port(Port) ; "").



%! 'uri-host'(?Host)// .
% ```abnf
% uri-host = <host, see [RFC3986], Section 3.2.2>
% ```

'uri-host'(Host) --> host(Host).
