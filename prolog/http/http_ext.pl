:- module(
  http_ext,
  [
    http_absolute_location/2, % +Spec, -Path
    http_accept/2,            % +Request, -Mediatypes
    http_header/3,            % +M, +Key, -Value
    http_link_to_id/2,        % +HandleId, -Local
    http_method/2,            % +Request, -Method
    http_output/2,            % +Request, -Output
    http_read_json_dict/1,    % -Data
    http_scheme/1,            % ?Scheme
    http_search/3,            % +Request, +Key, -Value
    http_search_pl/3,         % +Request, +Key, -Value
    http_status_label/2,      % +StatusCode, -Label
    http_status_reply/1,      % +Status
    http_status_reply/2,      % +Request, +Status
    http_uri/2,               % +Request, -Uri
    is_http_error/1           % +StatusCode
  ]
).
:- reexport(library(http/http_path)).

/** <module> HTTP receive

Support for extracting information from HTTP requests/received messages.

@author Wouter Beek
@version 2015/08, 2015/10-2016/02
*/

:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).





%! http_absolute_location(+Spec, -Path) is det.

http_absolute_location(Spec, Path) :-
  http_absolute_location(Spec, Path, []).



%! http_accept(+Request, -Mediatypes) is det.

http_accept(Req, MTs) :-
  (memberchk(accept(L), Req) -> true ; L = []),
  maplist(mediatype_pair, L, Pairs),
  desc_pairs_values(Pairs, MTs).
mediatype_pair(media(MT,_,N,_), N-MT).



%! http_header(+Metadata, +Key, -Value) is nondet.

http_header(D, Key, Value) :-
  get_dict(Key, D, D0s),
  member(D0, D0s),
  Value = D0.'llo:value'.


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



%! http_scheme(+Scheme:atom) is semidet.
%! http_scheme(-Scheme:atom) is multi.

http_scheme(http).
http_scheme(https).



%! http_search(+Request, +Key, -Value) is det.

http_search(Request, Key, Val) :-
  memberchk(search(L), Request),
  memberchk(Key=Val, L).



%! http_search_pl(+Request, +Key, -Value) is det.

http_search_pl(Request, Key, Val) :-
  http_search(Request, Key, Atom),
  term_to_atom(Val, Atom).



%! http_status_label(+Code:between(100,599), -Label:atom) is det.

http_status_label(Code, Label):-
  http_header:status_number_fact(Fact, Code),
  atom_phrase(http_header:status_comment(Fact), Label).



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



%! is_http_error(+Status:positive_integer) is semidet.

is_http_error(Status):-
  between(400, 599, Status).
