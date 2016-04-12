:- module(
  http_ext,
  [
    http_absolute_location/2, % +Spec, -Path
    http_accept/2,            % +Req, -Mediatypes
    http_dict/2,              % +Req, -Dict
    http_dict/3,              % +Req, +TypeMap, -Dict
    http_header/3,            % +M, +Key, -Value
    http_link_to_id/2,        % +HandleId, -Local
    http_method/2,            % +Req, -Method
    http_output/2,            % +Req, -Output
    http_read_json_dict/1,    % -Data
    http_reply_file/1,        % +File
    http_scheme/1,            % ?Scheme
    http_search/3,            % +Req, +Key, -Value
    http_search_pl/3,         % +Req, +Key, -Value
    http_status_label/2,      % +StatusCode, -Label
    http_status_reply/1,      % +Status
    http_status_reply/2,      % +Req, +Status
    http_uri/2,               % +Req, -Uri
    is_http_error/1           % +StatusCode
  ]
).
:- reexport(library(http/http_path)).

/** <module> HTTP receive

Support for extracting information from HTTP requests/received messages.

@author Wouter Beek
@version 2015/08, 2015/10-2016/02, 2016/04
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).





%! http_absolute_location(+Spec, -Path) is det.

http_absolute_location(Spec, Path) :-
  http_absolute_location(Spec, Path, []).



%! http_accept(+Req, -Mediatypes) is det.

http_accept(Req, MTs) :-
  (memberchk(accept(L), Req) -> true ; L = []),
  maplist(mediatype_pair, L, Pairs),
  desc_pairs_values(Pairs, MTs).
mediatype_pair(media(MT,_,N,_), N-MT).



%! http_dict(+Req, -D) is det.
%! http_dict(+Req, +TypeMap, -D) is det.

http_dict(Req, D) :-
  http_dict(Req, [], D).


http_dict(Req, TypeMap, D) :-
  memberchk(request_uri(Iri1), Req),
  % @tbd IRI mask
  uri_components(Iri1, uri_components(Scheme,Auth,Path,_,_)),
  uri_components(Iri2, uri_components(Scheme,Auth,Path,_,_)),
  (memberchk(search(T1), Req) -> true ; T1 = []),
  maplist(pair_to_pair(TypeMap), T1, T2),
  dict_pairs(D, _, [iri-Iri2|T2]).
http_dict(_, _, _{}).

pair_to_pair(TypeMap, N=V1, N-V2) :-
  (memberchk(N-Goal_2, TypeMap) -> call(Goal_2, V1, V2) ; V2 = V1).



%! http_header(+Metadata, +Key, -Value) is nondet.

http_header(D, Key, Value) :-
  get_dict(Key, D, Values),
  member(Value, Values).



%! http_link_to_id(+HandleId, -Local) is det.

http_link_to_id(HandleId, Local) :-
  http_link_to_id(HandleId, [], Local).



%! http_method(+Req, -Method) is det.

http_method(Req, M) :-
  memberchk(method(M), Req).



%! http_output(+Req, -Output) is det.

http_output(Req, Out) :-
  memberchk(pool(client(_,_,_,Out)), Req).



%! http_read_json_dict(-Data) is det

http_read_json_dict(Data) :-
  http_current_request(Req),
  http_read_json_dict(Req, Data).



%! http_reply_file(+File) is det.

http_reply_file(File) :-
  http_current_request(Req),
  http_reply_file(File, [], Req).



%! http_scheme(+Scheme:atom) is semidet.
%! http_scheme(-Scheme:atom) is multi.

http_scheme(http).
http_scheme(https).



%! http_search(+Req, +Key, -Value) is det.

http_search(Req, Key, Val) :-
  memberchk(search(L), Req),
  memberchk(Key=Val, L).



%! http_search_pl(+Req, +Key, -Value) is det.

http_search_pl(Req, Key, Val) :-
  http_search(Req, Key, Atom),
  term_to_atom(Val, Atom).



%! http_status_label(+Code:between(100,599), -Label:atom) is det.

http_status_label(Code, Label):-
  http_header:status_number_fact(Fact, Code),
  atom_phrase(http_header:status_comment(Fact), Label).



%! http_status_reply(+Status) is det.
%! http_status_reply(+Req, +Status) is det.

http_status_reply(Status) :-
  http_current_request(Req),
  http_status_reply(Req, Status).


http_status_reply(Req, Status) :-
  http_output(Req, Out),
  http_status_reply(Status, Out, [], _).



%! http_uri(+Req, -Uri) is det.

http_uri(Req, Uri) :-
  memberchk(request_uri(Uri), Req).



%! is_http_error(+Status:positive_integer) is semidet.

is_http_error(Status):-
  between(400, 599, Status).
