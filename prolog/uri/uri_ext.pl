:- module(
  uri_ext,
  [
    uri_component/3, % +Uri:uri
                     % +Field:atom
                     % ?Data:atom
  ]
).
:- reexport(library(uri)).

/** <module> URI extensions

Additional predicates for handling URIs.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(aggregate)).
:- use_module(library(error)).





%! uri_component(
%!   +Uri:uri,
%!   +Field:oneof([authority,fragment,host,password,path,port,search,scheme,user]),
%!   +Data:atom
%! ) is semidet.
%! uri_component(
%!   +Uri:uri,
%!   +Field:oneof([authority,fragment,host,password,path,port,search,scheme,user]),
%!   -Data:atom
%! ) is det.
% Abbreviates multiple predicates from `library(uri)`:
%   - uri_authority_components/2
%   - uri_authority_data/3
%   - uri_components/2
%   - uri_data/3

uri_component(Uri, Field, Data):-
  uri_field0(Field), !,
  uri_components(Uri, L),
  uri_data(Field, L, Data).
uri_component(Uri, Field, Data):-
  authority_field0(Field), !,
  uri_components(Uri, L0),
  uri_data(authority, L0, Authority),
  uri_authority_components(Authority, L),
  uri_authority_data(Field, L, Data).
uri_component(_, Field, _):-
  aggregate_all(set(X), uri_field(X), L),
  type_error(oneof(L), Field).

uri_field(Field):-
  authority_field0(Field).
uri_field(Field):-
  uri_field0(Field).

authority_field0(host).
authority_field0(password).
authority_field0(port).
authority_field0(user).

uri_field0(authority).
uri_field0(fragment).
uri_field0(path).
uri_field0(search).
uri_field0(scheme).
