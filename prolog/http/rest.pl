:- module(
  rest,
  [
    rest_handler/5,   % +Request, +HandleId, :Exists_1,   :Singular_3, :Plural_2
    rest_mediatype/3, % +Method, +MediaTypes, :Plural_2
    rest_mediatype/4  % +Method, +MediaTypes, +Resource, :Singular_3
  ]
).

/** <module> REST

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(atom_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
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



rest_handler(Req, HandleId, Exists_1, Singular_3, Plural_2) :-
  memberchk(request_uri(Local), Req),
  http_accept(Req, MTs),
  http_link_to_id(HandleId, [], Endpoint),
  http_method(Req, Method),
  (   same_iri(Local, Endpoint)
  ->  call(Plural_2, Method, MTs)
  ;   setting(rest:host, Host),
      atom_concat(Host, Local, Res),
      call(Exists_1, Res)
  ->  call(Singular_3, Method, MTs, Res)
  ), !.
rest_handler(Req, _, _, _, _) :-
  memberchk(request_uri(Res), Req),
  http_status_reply(Req, not_found(Res)).

same_iri(X1, Y1):-
  atom_ending_in(X1, /, X2),
  atom_ending_in(Y1, /, Y2),
  X2 == Y2.


rest_mediatype(Method, [], _) :- !, http_status_reply(Method, non_acceptable(_)).
rest_mediatype(Method, [MT|_], Plural_2) :- call(Plural_2, Method, MT), !.
rest_mediatype(Method, [_|MTs], Plural_2) :- rest_mediatype(Method, MTs, Plural_2).


rest_mediatype(Method, [], _, _) :- !, http_status_reply(Method, non_acceptable(_)).
rest_mediatype(Method, [MT|_], Res, Singular_3) :- call(Singular_3, Method, MT, Res), !.
rest_mediatype(Method, [_|MTs], Res, Singular_3) :- rest_mediatype(Method, MTs, Res, Singular_3).
