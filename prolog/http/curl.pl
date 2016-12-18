:- module(
  curl,
  [
    curl_get/0,
    curl_get/1,     % +Uri:atom
    curl_get/2,     % +Uri:atom, +MT:atom
    curl_head/0,
    curl_head/1,    % +Uri:atom
    curl_head/2,    % +Uri:atom, +MT:atom
    curl_options/0,
    curl_options/1, % +Uri:atom
    curl_uri/1,     % -Uri:atom
    curl_uri/2      % -Uri:atom, +Opts
  ]
).

/** <module> HTTP CLI: A command-line interface for SWI

This application allows a default URI to be defined in terms of its
constituent components, defining the following settings:

  - fragment
  - host
  - path
  - port
  - query
  - scheme

@author Wouter Beek
@version 2016/12
*/

:- reexport(library(http/http_io)).
:- use_module(library(print_ext)).
:- use_module(library(settings)).
:- use_module(library(uri/uri_ext)).

:- setting(fragment, any, _, "The fragment of the default URI.").
:- setting(host, atom, 'swi-prolog.org', "The host name of the default authority.").
:- setting(path, list(atom), [], "The path of the default URI.").
:- setting(port, any, _, "The port of the default authority.").
:- setting(query, list(compound), [], "The query of the default URI.").
:- setting(scheme, oneof([http,https]), http, "The scheme of the default URI.").
:- setting(user, any, _, "The user of the default authority.").





%! curl_get is det.
%! curl_get(+Uri:atom) is det.

curl_get :-
  curl_uri(Uri),
  curl_get(Uri).


curl_get(Uri) :-
  curl_get(Uri, _).


curl_get(Uri, MT) :-
  http_get(Uri, print_body0, [request_header(accept=MT),verbose(all)]).

print_body0(In, Path, Path) :-
  copy_stream_data(In, user_output).



%! curl_head is det.
%! curl_head(+Uri:atom) is det.
%! curl_head(+Uri:atom, +MT:atom) is det.

curl_head :-
  curl_uri(Uri),
  curl_head(Uri).


curl_head(Uri) :-
  curl_head(Uri, []).


curl_head(Uri, MT) :-
  http_head(Uri, [request_header(accept=MT),verbose(all)]).



%! curl_options is det.
%! curl_options(+Uri:atom) is det.
%
% The OPTIONS method requests information about the communication
% options available for the target resource, at either the origin
% server or an intervening intermediary.  This method allows a client
% to determine the options and/or requirements associated with a
% resource, or the capabilities of a server, without implying a
% resource action.

curl_options :-
  curl_uri(Uri),
  curl_options(Uri).


curl_options(Uri) :-
  http_options(Uri, [verbose(all)]).



%! curl_uri(-Uri:atom) is det.
%! curl_uri(-Uri:atom, +Opts) is det.
%
% The following options are supported:
%
%   - scheme(+oneof([http,https]))
%   - user(?atom)
%   - host(+atom)
%   - port(?atom)
%   - path(+list(atom))
%   - query(+list(compound))
%   - fragment(?atom)

curl_uri(Uri) :-
  curl_uri(Uri, []).


curl_uri(Uri, Opts) :-
  (option(scheme(Scheme), Opts) -> true ; setting(scheme, Scheme)),
  (option(user(User), Opts) -> true ; setting(user, User)),
  (option(host(Host), Opts) -> true ; setting(host, Host)),
  (option(port(Port), Opts) -> true ; setting(port, Port)),
  (option(path(Path), Opts) -> true ; setting(path, Path)),
  (option(query(Query), Opts) -> true ; setting(query, Query)),
  (option(fragment(Frag), Opts) -> true ; setting(fragment, Frag)),
  uri_comps(Uri, uri(Scheme,auth(User,Host,Port),Path,Query,Frag)).
