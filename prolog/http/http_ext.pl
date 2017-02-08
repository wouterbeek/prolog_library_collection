:- module(
  http_ext,
  [
    http_absolute_location/2, % +Spec, -Path
    http_base_location_uri/1, % -Uri
    http_cookie/2,            % +Key, -Cookie
    http_default_port/2,      % +Scheme, -DefPort
    http_uri_query/2,         % +Uri, -Comp
    http_link_to_id/2,        % +HandleId, -Local
    http_location_uri/2,      % +Req, -Location
    http_peer/1,              % -PeerIP
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
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_io)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http11)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(uri/uri_ext)).





%! http_absolute_location(+Spec, -Path) is det.

http_absolute_location(Spec, Path) :-
  http_absolute_location(Spec, Path, []).



%! http_base_location_uri(-Uri) is det.

http_base_location_uri(Uri2) :-
  http_current_request(Req),
  http_location_uri(Req, Uri1),
  uri_components(Uri1, uri_components(Scheme,Auth,Path,_,_)),
  uri_components(Uri2, uri_components(Scheme,Auth,Path,_,_)).



%! http_cookie(+Key, -Cookie) is det.

http_cookie(Key, Cookie) :-
  http_current_request(Req),
  memberchk(cookie(Cookies), Req),
  memberchk(Key=Cookie, Cookies).



%! http_default_port(+Scheme, -DefPort) is det.

http_default_port(http, 80).
http_default_port(https, 443).



%! http_uri_query(+Uri, -Comp) is nondet.

http_uri_query(Uri, Comp) :-
  uri_components(Uri, uri_components(_,_,_,Query,_)),
  (var(Query) -> Comps = [] ; uri_query_components(Query, Comps)),
  member(Comp, Comps).



%! http_link_to_id(+HandleId, -Local) is det.

http_link_to_id(HandleId, Local) :-
  http_link_to_id(HandleId, [], Local).



%! http_location_uri(+Req, -Uri) is det.
%
% @tbd ???

http_location_uri(Req, AbsUri) :-
  memberchk(request_uri(RelUri), Req),
  http_absolute_uri(RelUri, AbsUri).



%! http_peer(-PeerIP) is det.

http_peer(PeerIP) :-
  http_current_request(Req),
  http_peer(Req, PeerIP).



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
