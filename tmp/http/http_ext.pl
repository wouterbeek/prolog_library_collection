:- module(
  http_ext,
  [
    http_uri_query/2,         % +Uri, -Comp
    http_peer/1,              % -PeerIP
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
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_io)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http11)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(uri_ext)).





%! http_uri_query(+Uri, -Comp) is nondet.

http_uri_query(Uri, Comp) :-
  uri_components(Uri, uri_components(_,_,_,Query,_)),
  (var(Query) -> Comps = [] ; uri_query_components(Query, Comps)),
  member(Comp, Comps).



%! http_peer(-PeerIP) is det.

http_peer(PeerIP) :-
  http_current_request(Req),
  http_peer(Req, PeerIP).
  


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
