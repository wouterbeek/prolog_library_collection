:- module(
  http_write,
  [
    reply_http_message/1, % +Status
    reply_http_message/2, % +Status, +Headers
    reply_http_message/3  % +Status, +Headers, +Body
  ]
).

/* <module> HTTP write

@author Wouter Beek
@version 2016/08, 2016/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_io)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).

:- setting(
     http:server,
     list,
     ['Prolog-Library-Collection'],
     "The server issuing the HTTP replies."
).





%! reply_http_message(+Status) is det.
%! reply_http_message(+Status, +Headers) is det.
%! reply_http_message(+Status, +Headers, +Body) is det.

reply_http_message(Status) :-
  reply_http_message(Status, []).


reply_http_message(Status, Headers) :-
  reply_http_message(Status, Headers, []).


reply_http_message(Status, Headers, Body) :-
  setting(http:server, Server),
  dcg_with_output_to(atom(A),
    'HTTP-message'(1-1, Status, ['Server'-Server|Headers], Body)
  ),
  debug(http(write), "~a", [A]),
  writeln(A).





% GRAMMAR %

'Allow'(Val) -->
  seplist(method, ", ", Val).



'Content-Type'(MT) -->
  'media-type'(MT).



'field-name'(Key) -->
  str(Key).



'header-field'(Key-Val) -->
  'field-name'(Key),
  ":",
  'OWS',
  (   {Key == 'Allow'}
  ->  'Allow'(Val)
  ;   {Key == 'Content-Type'}
  ->  'Content-Type'(Val)
  ;   {Key == 'Server'}
  ->  'Server'(Val)
  ),
  'CRLF'.



'HTTP-message'(Version, Status, Headers, Body) -->
  'status-line'(Version, Status),
  *('header-field', Headers), !,
  'CRLF',
  Body.



'HTTP-name' -->
  "HTTP".



'HTTP-version'(X-Y) -->
  'HTTP-name',
  "/",
  digit(X),
  ".",
  digit(Y).



'media-type'(media_type(Type/Subtype,Params)) -->
  type(Type),
  "/",
  subtype(Subtype),
  *(parameter0, Params), !.

parameter0(Param) -->
  ";",
  'OWS',
  parameter(Param).



method(Method1) -->
  {upcase_atom(Method1, Method2)},
  atom(Method2).



parameter(Key-Val) -->
  atom(Key),
  "=",
  atom(Val).



product(Name-Version) --> !,
  atom(Name),
  "/",
  'product-version'(Version).
product(Name) --> !,
  atom(Name).



'product-version'(Version) -->
  atom(Version).



'reason-phrase'(Status) -->
  {http_status_label(Status, Lbl)},
  atom(Lbl).



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
