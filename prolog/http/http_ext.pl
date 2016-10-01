:- module(
  http_ext,
  [
    http_absolute_location/2, % +Spec, -Path
    http_accept/2,            % +Req, -MTs
    http_base_location_iri/2, % +Req, -iri
    http_content_type/1,      % +MT
    http_end_of_header/0,
    http_iri_query/2,         % +Iri, -Comp
    http_link_to_id/2,        % +HandleId, -Local
    http_location_iri/2,      % +Req, -Location
    http_method/2,            % +Req, -Method
    http_output/1,            % -Output
    http_output/2,            % +Req, -Output
    http_read_json_dict/1,    % -Data
    http_relative_iri/2,      % +Req, -Iri
    http_reply_file/1,        % +File
    http_resource_iri/4,      % +Req, -Iri, -Query, -Frag
    http_status_reply/2,      % +Req, +Status
    is_empty_get_request/1    % +Req
  ]
).

/** <module> HTTP receive

Support for extracting information from HTTP requests/received
messages.

@author Wouter Beek
@version 2015/08, 2015/10-2016/02, 2016/04, 2016/06-2016/08
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_io)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(iri/iri_ext)).





%! http_absolute_location(+Spec, -Path) is det.

http_absolute_location(Spec, Path) :-
  http_absolute_location(Spec, Path, []).



%! http_accept(+Req, -MTs) is det.

http_accept(Req, MTs) :-
  (memberchk(accept(L), Req) -> true ; L = []),
  maplist(mediatype_pair, L, Pairs),
  desc_pairs_values(Pairs, MTs).


mediatype_pair(media(MT,_,N,_), N-MT).



%! http_base_location_iri(+Req, -Iri) is det.

http_base_location_iri(Req, Iri2) :-
  http_location_iri(Req, Iri1),
  uri_components(Iri1, uri_components(Scheme,Auth,Path,_,_)),
  uri_components(Iri2, uri_components(Scheme,Auth,Path,_,_)).



%! http_content_type(+MT) is det.

http_content_type(Type/Subtype) :-
  format("Content-Type: ~a/~a; charset=UTF-8~n", [Type,Subtype]).



%! http_end_of_header is det.

http_end_of_header :-
  format("~n").



%! http_iri_query(+Iri, -Comp) is nondet.

http_iri_query(Iri, Comp) :-
  uri_components(Iri, uri_components(_,_,_,Query,_)),
  (var(Query) -> Comps = [] ; uri_query_components(Query, Comps)),
  member(Comp, Comps).


%! http_link_to_id(+HandleId, -Local) is det.

http_link_to_id(HandleId, Local) :-
  http_link_to_id(HandleId, [], Local).



%! http_location_iri(+Req, -Iri) is det.

http_location_iri(Req, Loc) :-
  http_relative_iri(Req, Iri),
  iri_to_location(Iri, Loc).



%! http_method(+Req, -Method) is det.

http_method(Req, M) :-
  memberchk(method(M), Req).



%! http_output(-Output) is det.
%! http_output(+Req, -Output) is det.

http_output(Out) :-
  http_current_request(Req),
  http_output(Req, Out).


http_output(Req, Out) :-
  memberchk(pool(client(_,_,_,Out)), Req).



%! http_read_json_dict(-Data) is det

http_read_json_dict(Data) :-
  http_current_request(Req),
  http_read_json_dict(Req, Data).



%! http_relative_iri(+Req, -Iri) is det.

http_relative_iri(Req, Iri) :-
  memberchk(request_uri(Iri), Req).



%! http_reply_file(+File) is det.

http_reply_file(File) :-
  http_current_request(Req),
  http_reply_file(File, [], Req).



%! http_resource_iri(+Req, -Res, -Query, -Frag) is det.

http_resource_iri(Req, Res, Query, Frag) :-
  http_relative_iri(Req, Iri),
  iri_to_resource(Iri, Res, Query, Frag).



%! http_status_reply(+Req, +Status) is det.

http_status_reply(Req, Status) :-
  http_output(Req, Out),
  http_status_reply(Status, Out, [], _).



%! is_empty_get_request(+Req) is semidet.
%
% Succeeds iff HTTP request Req is a GET request with no parameter.

is_empty_get_request(Req) :-
  option(request_uri(URI), Req),
  uri_components(URI, Components),
  uri_data(search, Components, Search),
  var(Search),
  option(method(get), Req).
