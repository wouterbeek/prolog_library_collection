:- module(
  http_cli,
  [
    default_uri/1, % -Uri:atom
    http_options/0
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

:- setting(fragment, atom, '', "The fragment of the default URI.").
:- setting(host, atom, 'swi-prolog.org', "The host name of the default authority.").
:- setting(path, list(atom), [], "The path of the default URI.").
:- setting(port, between(1,9999), 80, "The port of the default authority.").
:- setting(query, list(compound), [], "The query of the default URI.").
:- setting(scheme, oneof([http,https]), http, "The scheme of the default URI.").
:- setting(user, atom, '', "The user of the default authority.").





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



http_options :-
  default_uri(Uri),
  http_options(Uri).
