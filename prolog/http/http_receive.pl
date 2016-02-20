:- module(
  http_receive,
  [
    http_accept/2,         % +Request, -Mediatypes
    http_header/3,         % +M, +Key, -Value
    http_iri/3,            % +Request, +Prefix, -Iri
    http_link_to_id/2,     % +HandleId, -Local
    http_method/2,         % +Request, -Method
    http_output/2,         % +Request, -Output
    http_read_json_dict/1, % -Data
    http_search/3,         % +Request, +Key, -Value
    http_search_pl/3,      % +Request, +Key, -Value
    http_status_reply/1,   % +Status
    http_status_reply/2,   % +Request, +Status
    http_uri/2             % +Request, -Uri
  ]
).

/** <module> HTTP receive

Support for extracting information from HTTP requests/received messages.

@author Wouter Beek
@version 2015/08, 2015/12-2016/02
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(semweb/rdf_db), [rdf_global_id/2]).





%! http_accept(+Request, -Mediatypes) is det.

http_accept(Req, MTs) :-
  memberchk(accept(L), Req),
  maplist(mediatype_pair, L, Pairs),
  desc_pairs_values(Pairs, MTs).
mediatype_pair(media(MT,_,N,_), N-MT).



%! http_header(+Metadata, +Key, -Value) is nondet.

http_header(D, Key, Value) :-
  get_dict(Key, D, D0s),
  member(D0, D0s),
  Value = D0.'llo:value'.



%! http_iri(+Request, +Prefix, -Iri) is det.

http_iri(Req, Prefix, Iri) :-
  memberchk(request_uri(Local0), Req),
  sub_atom(Local0, 1, _, 0, Local),
  rdf_global_id(Prefix:Local, Iri).



%! http_link_to_id(+HandleId, -Local) is det.

http_link_to_id(HandleId, Local) :-
  http_link_to_id(HandleId, [], Local).



%! http_method(+Request, -Method) is det.

http_method(Req, M) :-
  memberchk(method(M), Req).



%! http_output(+Request, -Output) is det.

http_output(Req, Out) :-
  memberchk(pool(client(_,_,_,Out)), Req).



%! http_read_json_dict(-Data) is det

http_read_json_dict(Data) :-
  http_current_request(Req),
  http_read_json_dict(Req, Data).



%! http_search(+Request, +Key, -Value) is det.

http_search(Request, Key, Val) :-
  memberchk(search(L), Request),
  memberchk(Key=Val, L).



%! http_search_pl(+Request, +Key, -Value) is det.

http_search_pl(Request, Key, Val) :-
  http_search(Request, Key, Atom),
  term_to_atom(Val, Atom).



%! http_status_reply(+Status) is det.
%! http_status_reply(+Request, +Status) is det.

http_status_reply(Status) :-
  http_current_request(Req),
  http_status_reply(Req, Status).


http_status_reply(Req, Status) :-
  http_output(Req, Out),
  http_status_reply(Status, Out, [], _).



%! http_uri(+Request, -Uri) is det.

http_uri(Req, Uri) :-
  memberchk(request_uri(Uri), Req).
