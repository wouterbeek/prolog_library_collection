:- module(
  http_ext,
  [
    http_absolute_location/2, % +Spec, -Path
    http_base_location_iri/1, % -Iri
    http_cookie/2,            % +Key, -Cookie
    http_default_port/2,      % +Scheme, -DefPort
    http_iri_query/2,         % +Iri, -Comp
    http_is_get/1,            % +Method
    http_link_to_id/2,        % +HandleId, -Local
    http_location_iri/2,      % +Req, -Location
    http_peer/1,              % -PeerIP
    http_read_data/1,         % -Data
    http_read_data/2,         % -Data, +Opts
    http_redirect/2,          % +How, +To
    http_reply_file/1,        % +File
    is_empty_get_request/1    % +Req
  ]
).

/** <module> HTTP receive

Support for extracting information from HTTP requests/received
messages.

@author Wouter Beek
@version 2015/08-2016/12
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_client), [http_read_data/3]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_io)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http11)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(iri/iri_ext)).





%! http_absolute_location(+Spec, -Path) is det.

http_absolute_location(Spec, Path) :-
  http_absolute_location(Spec, Path, []).



%! http_base_location_iri(-Iri) is det.

http_base_location_iri(Iri2) :-
  http_current_request(Req),
  http_location_iri(Req, Iri1),
  uri_components(Iri1, uri_components(Scheme,Auth,Path,_,_)),
  uri_components(Iri2, uri_components(Scheme,Auth,Path,_,_)).



%! http_cookie(+Key, -Cookie) is det.

http_cookie(Key, Cookie) :-
  http_current_request(Req),
  memberchk(cookie(Cookies), Req),
  memberchk(Key=Cookie, Cookies).



%! http_default_port(+Scheme, -DefPort) is det.

http_default_port(http, 80).
http_default_port(https, 443).



%! http_iri_query(+Iri, -Comp) is nondet.

http_iri_query(Iri, Comp) :-
  uri_components(Iri, uri_components(_,_,_,Query,_)),
  (var(Query) -> Comps = [] ; uri_query_components(Query, Comps)),
  member(Comp, Comps).



%! http_is_get(+Method) is semidet.

http_is_get(get).
http_is_get(head).



%! http_link_to_id(+HandleId, -Local) is det.

http_link_to_id(HandleId, Local) :-
  http_link_to_id(HandleId, [], Local).



%! http_location_iri(+Req, -Iri) is det.

http_location_iri(Req, Loc) :-
  memberchk(request_uri(Iri), Req),
  iri_to_location(Iri, Loc).



%! http_peer(-PeerIP) is det.

http_peer(PeerIP) :-
  http_current_request(Req),
  http_peer(Req, PeerIP).



%! http_read_data(-Data) is det.
%! http_read_data(-Data, +Opts) is det.

http_read_data(Data) :-
  http_read_data(Data, []).


http_read_data(Data, Opts) :-
  http_current_request(Req),
  http_read_data(Req, Data, Opts).



%! http_redirect(+How, +To) is det.

http_redirect(How, To) :-
  http_current_request(Req),
  http_redirect(How, To, Req).
  


%! http_reply_file(+File) is det.

http_reply_file(File) :-
  http_current_request(Req),
  http_reply_file(File, [], Req).



%! is_empty_get_request(+Req) is semidet.
%
% Succeeds iff HTTP request Req is a GET request with no parameter.

is_empty_get_request(Req) :-
  memberchk(request_uri(Uri), Req),
  uri_components(Uri, Comps),
  uri_data(search, Comps, Search),
  var(Search),
  option(method(get), Req).
