:- module(
  http_ext,
  [
    http_absolute_location/2, % +Spec, -Path
    http_accept/2,            % +Req, -MTs
    http_auth_error/1,        % +Status
    http_error/1,             % +Status
    http_get_dict/3,          % +Key, +D, -Val
    http_header/3,            % +M, +Key, -Val
    http_iri/2,               % +Req, -iri
    http_link_to_id/2,        % +HandleId, -Local
    http_method/2,            % +Req, -Method
    http_output/1,            % -Output
    http_output/2,            % +Req, -Output
    http_query/1,             % -D
    http_query/2,             % +Req, -D
    http_query/3,             % +Req, +Key, -Val
    http_read_json_dict/1,    % -Data
    http_redirect/1,          % +Status
    http_reply_file/1,        % +File
    http_scheme/1,            % ?Scheme
    http_status_label/2,      % +StatusCode, -Lbl
    http_status_reply/1,      % +Status
    http_status_reply/2       % +Req, +Status
  ]
).

/** <module> HTTP receive

Support for extracting information from HTTP requests/received messages.

@author Wouter Beek
@version 2015/08, 2015/10-2016/02, 2016/04, 2016/06
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).





%! http_absolute_location(+Spec, -Path) is det.

http_absolute_location(Spec, Path) :-
  http_absolute_location(Spec, Path, []).



%! http_accept(+Req, -MTs) is det.

http_accept(Req, MTs) :-
  (memberchk(accept(L), Req) -> true ; L = []),
  maplist(mediatype_pair, L, Pairs),
  desc_pairs_values(Pairs, MTs).
mediatype_pair(media(MT,_,N,_), N-MT).



%! http_error(+Status) is semidet.

http_auth_error(401).



%! http_error(+Status) is semidet.

http_error(Status):-
  between(400, 599, Status).



%! http_get_dict(+Key, +Ms, -Val) is nondet.

http_get_dict(Key, M, Val) :-
  get_dict('llo:http_communication', M, Ms),
  last(Ms, M0),
  get_dict(Key, M0, Val).



%! http_header(+M, +Key, -Val) is nondet.

http_header(M, Key, Val) :-
  http_get_dict('llo:headers', M, Headers),
  get_dict(Key, Headers, Vals),
  member(Val, Vals).



%! http_iri(+Req, -Iri) is det.

http_iri(Req, Iri2) :-
  memberchk(request_uri(Iri1), Req),
  uri_components(Iri1, uri_components(Scheme,Auth,Path,_,_)),
  uri_components(Iri2, uri_components(Scheme,Auth,Path,_,_)).



%! http_link_to_id(+HandleId, -Local) is det.

http_link_to_id(HandleId, Local) :-
  http_link_to_id(HandleId, [], Local).



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



%! http_query(-D) is det.
%! http_query(+Req, -D) is det.

http_query(D) :-
  http_current_request(Req),
  http_query(Req, D).


http_query(Req, D) :-
  http_iri(Req, Iri),
  findall(Key-Val, http_query(Req, Key, Val), Pairs),
  dict_pairs(D, [iri-Iri|Pairs]).



%! http_query(+Req, +Key, -Val) is det.

http_query(Req, Key, Val) :-
  memberchk(search(L), Req),
  member(Key=Val0, L),
  atom_to_term(Val0, Val).



%! http_read_json_dict(-Data) is det

http_read_json_dict(Data) :-
  http_current_request(Req),
  http_read_json_dict(Req, Data).



%! http_redirect(+Status) is semidet.

http_redirect(Status) :-
  between(300, 399, Status).



%! http_reply_file(+File) is det.

http_reply_file(File) :-
  http_current_request(Req),
  http_reply_file(File, [], Req).



%! http_scheme(+Scheme:atom) is semidet.
%! http_scheme(-Scheme:atom) is multi.

http_scheme(http).
http_scheme(https).



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
