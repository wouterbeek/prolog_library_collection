:- module(
  http_receive,
  [
    http_accept/2,       % +Request, -Mediatypes
    http_iri/3,          % +Request, +Prefix, -Iri
    http_method/2,       % +Request, -Method
    http_output/2,       % +Request, -Output
    http_search/3,       % +Request, +Key, -Value
    http_search_pl/3,    % +Request, +Key, -Value
    http_status_reply/2, % +Request, +Status
    http_uri/2,          % +Request, -Uri
    mediatype_is_html/1, % +Mediatype
    mediatype_is_json/1  % +Mediatype
  ]
).

/** <module> HTTP receive

Support for receiving an HTTP reply.

@author Wouter Beek
@version 2015/08, 2015/12-2016/01
*/

:- use_module(library(http/http_header)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_prefix)).





%! http_accept(+Request:list(compound), -Mediatypes:list(compound)) is det.

http_accept(Req, MTs):-
  memberchk(accept(L), Req),
  maplist(mediatype_pair, L, Pairs),
  pairs_sorted_values(Pairs, @>=, MTs).
mediatype_pair(media(MT,_,N,_), N-MT).



%! http_iri(+Request:list(compound), +Prefix:atom, -Iri:atom) is det.

http_iri(Req, Prefix, Iri):-
  memberchk(request_uri(Local0), Req),
  sub_atom(Local0, 1, _, 0, Local),
  rdf_global_id(Prefix:Local, Iri).



%! http_method(+Request:list(compound), -Method:atom) is det.

http_method(Req, M):-
  memberchk(method(M), Req).



%! http_output(+Request:list(compound), -Output:stream) is det.

http_output(Req, Out):-
  memberchk(pool(client(_,_,_,Out)), Req).



%! http_search(+Request:list(compound), +Key:atom, -Value:atom) is det.

http_search(Request, Key, Val):-
  memberchk(search(L), Request),
  memberchk(Key=Val, L).



%! http_search_pl(+Request:list(compound), +Key:atom, -Value:atom) is det.

http_search_pl(Request, Key, Val):-
  http_search(Request, Key, Atom),
  term_to_atom(Val, Atom).



%! http_status_reply(+Request:list(compound), +Status:compound) is det.

http_status_reply(Req, Status):-
  http_output(Req, Out),
  http_status_reply(Status, Out, [], _).



%! http_uri(+Request:list(compound), -Uri:atom) is det.

http_uri(Req, Uri):-
  memberchk(request_uri(Uri), Req).



%! mediatype_is_html(+Mediatype:compound) is semidet.

mediatype_is_html(application/'xhtml+xml').
mediatype_is_html(application/xml).
mediatype_is_html(text/html).



%! mediatype_is_json(+Mediatype:compound) is semidet.

mediatype_is_json(application/json).
mediatype_is_json(application/'ld+json').
