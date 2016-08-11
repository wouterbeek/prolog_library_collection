:- module(
  http_write,
  [
    reply_http_message/2, % +Req, +Status
    reply_http_message/3, % +Req, +Status, +Headers
    reply_http_message/4  % +Req, +Status, +Headers, +Body
  ]
).

/* <module> HTTP write

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_io)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).

:- setting(http:server, list, [], "The server issuing the HTTP replies.").





%! reply_http_message(+Req, +Status) is det.
%! reply_http_message(+Req, +Status, +Headers) is det.
%! reply_http_message(+Req, +Status, +Headers, +Body) is det.

reply_http_message(Req, Status) :-
  reply_http_message(Req, Status, []).


reply_http_message(Req, Status, Headers) :-
  reply_http_message(Req, Status, Headers, []).


reply_http_message(Req, Status, Headers, Body) :-
  setting(http:server, Server),
  http_output(Req, Out),
  once(
    atom_phrase(
      'HTTP-message'(1-1, Status, ['Server'-Server|Headers], Body),
      A
    )
  ),
  debug(http(write), "~a", [A]),
  format(Out, "~a", [A]).





% GRAMMAR %

'Allow'(Val) -->
  seplist(method, ", ", Val).



'Content-Type'(MT) -->
  'media-type'(MT).



'field-name'(Key) -->
  str(Key).



'field-value'('Allow', Val) -->
  'Allow'(Val).
'field-value'('Content-Type', Val) -->
  'Content-Type'(Val).
'field-value'('Server', Val) -->
  'Server'(Val).



'header-field'(Key-Val) -->
  'field-name'(Key), ":", 'OWS',
  'field-value'(Key, Val), 'CRLF'.



'HTTP-message'(Version, Status, Headers, Body) -->
  'status-line'(Version, Status),
  *('header-field', Headers),
  'CRLF',
  Body.



'HTTP-name' -->
  "HTTP".



'HTTP-version'(X-Y) -->
  'HTTP-name', "/", digit(X), ".", digit(Y).



'media-type'(media_type(Type/Subtype,Params)) -->
  type(Type), "/", subtype(Subtype),
  *(parameter0, Params).

parameter0(Param) -->
  ";", 'OWS', parameter(Param).



method(Method1) -->
  {uppercase_string(Method1, Method2)},
  str(Method2).



parameter(Key-Val) -->
  atom(Key), "=", atom(Val).



product(Name-Version) --> !,
  str(Name),
  "/", 'product-version'(Version).
product(Name) --> !,
  str(Name).



'product-version'(Version) -->
  str(Version).



'reason-phrase'(Status) -->
  {http_status_label(Status, Lbl)},
  str(Lbl).



'Server'(Products) -->
  seplist(product, 'RWS', Products).



'status-code'(Status) -->
  number(Status).



'status-line'(Version, Status) -->
  'HTTP-version'(Version), 'SP',
  'status-code'(Status), 'SP',
  'reason-phrase'(Status), 'CRLF'.



subtype(A) -->
  atom(A).



type(A) -->
  atom(A).





% HELPERS %

'CRLF' --> "\r\n".



'OWS' --> " ".



'RWS' --> " ".



'SP' --> " ".
