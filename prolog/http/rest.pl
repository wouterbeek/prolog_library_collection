:- module(
  rest,
  [
    rest_handler/5,   % +Request, +Endpoint, :Exists_1,   :Singular_3, :Plural_2
    rest_mediatype/3, % +Request, +MediaTypes, :Plural_2
    rest_mediatype/4  % +Request, +Resource,   +MediaTypes, :Singular_3
  ]
).

/** <module> REST

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(atom_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_receive)).
:- use_module(library(settings)).

:- setting(
     rest:host,
     atom,
     'http://cliopatria.lod.labs.vu.nl',
	   'The host IRI with respect to which the REST API should work.'
   ).

:- html_meta
   rest_handler(+, +, 1, 3, 2),
   rest_mediatype(+, +, 2),
   rest_mediatype(+, +, +, 3).



rest_handler(Req, Endpoint, Exists_1, Singular_3, Plural_2) :-
  setting(rest:host, Host),
  memberchk(request_uri(Local), Req),
  atom_concat(Host, Local, Res),
  http_accept(Req, MTs),
  (   same_iri(Res, Endpoint)
  ->  call(Plural_2, Req, MTs)
  ;   call(Exists_1, Res)
  ->  call(Singular_3, Req, Res, MTs)
  ;   http_status_reply(Req, not_found(Res))
  ).
rest_handler(Req, _, _) :-
  memberchk(request_uri(Res), Req),
  http_status_reply(Req, not_found(Res)).

same_iri(X1, Y1):-
  atom_ending_in(X1, /, X2),
  atom_ending_in(Y1, /, Y2),
  X2 == Y2.



rest_mediatype(Req, [], _) :- !,
  http_status_reply(Req, non_acceptable(_)).
rest_mediatype(Req, [MT|_], Plural_2) :-
  call(Plural_2, Req, MT), !.
rest_mediatype(Req, [_|MTs], Plural_2) :-
  rest_mediatype(Req, MTs, Plural_2).


rest_mediatype(Req, _, [], _) :- !,
  http_status_reply(Req, non_acceptable(_)).
rest_mediatype(Req, Res, [MT|_], Singular_3) :-
  call(Singular_3, Req, Res, MT), !.
rest_mediatype(Req, Res, [_|MTs], Singular_3) :-
  rest_mediatype(Req, Res, MTs, Singular_3).
