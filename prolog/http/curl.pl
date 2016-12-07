:- module(
  curl,
  [
    default_uri/1,  % -Uri:atom
    curl_get/0,
    curl_get/1,     % +Uri:atom
    curl_get/2,     % +Uri:atom, +MT:atom
    curl_head/0,
    curl_head/1,    % +Uri:atom
    curl_head/2,    % +Uri:atom, +MT:atom
    curl_options/0,
    curl_options/1  % +Uri:atom
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





%! default_uri(-Uri:atom) is det.

default_uri(Uri) :-
  setting(scheme, Scheme),
  setting(user, User),
  setting(host, Host),
  setting(port, Port),
  setting(path, Path),
  setting(query, Query),
  setting(fragment, Frag),
  uri_comps(Uri, uri(Scheme,auth(User,Host,Port),Path,Query,Frag)).



%! curl_get is det.
%! curl_get(+Uri:atom) is det.

curl_get :-
  default_uri(Uri),
  curl_get(Uri).


curl_get(Uri) :-
  curl_get(Uri, _).


curl_get(Uri, MT) :-
  http_get(Uri, true0, [request_header(accept=MT),verbose(true)]).

true0(In, Path, Path) :-
  copy_stream_data(In, user_output).



%! curl_head is det.
%! curl_head(+Uri:atom) is det.
%! curl_head(+Uri:atom, +MT:atom) is det.

curl_head :-
  default_uri(Uri),
  curl_head(Uri).


curl_head(Uri) :-
  curl_head(Uri, []).


curl_head(Uri, MT) :-
  http_head(Uri, [request_header(accept=MT),verbose(true)]).



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
  default_uri(Uri),
  curl_options(Uri).


curl_options(Uri) :-
  http_options(Uri, [verbose(true)]).
