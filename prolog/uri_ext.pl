:- module(
  uri_ext,
  [
    append_segments/3,     % +Segments1, +Segments2, ?Segments3
    uri_comp_set/4,        % +Kind, +Uri1, +Component, -Uri2
    uri_comps/2,           % ?Uri, ?Components
    uri_file_extensions/2, % +Uri, -Extensions
    uri_local_name/2,      % +Uri, -Local
    uri_media_type/2,      % +Uri, -MediaType
    uri_strip/2            % +Uri, -Base
  ]
).

/** <module> Extended support for URIs

*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(plunit)).

:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_generic), []).





%! append_segments(+Segments1:list(atom), +Segments2:list(atom), +Segments3:list(atom)) is semidet.
%! append_segments(+Segments1:list(atom), +Segments2:list(atom), -Segments3:list(atom)) is det.
%
% Appends lists of path segments.  Empty segments commonly appear at
% the beginning and end of URI paths.

append_segments(L1a, L2a, L3) :-
  exclude([X]>>(X==''), L1a, L1b),
  exclude([X]>>(X==''), L2a, L2b),
  append(L1b, L2b, L3).

:- begin_tests(append_segments).

test('append_segments(+,+,+)', [forall(test_append_segments(L1,L2,L3))]) :-
  append_segments(L1, L2, L3).
test('append_segments(+,+,+)', [forall(test_append_segments(L1,L2,L3))]) :-
  append_segments(L1, L2, L3_),
  assertion(L3_ == L3).

test_append_segments(['',a,b,c,''], [''], [a,b,c]).

:- end_tests(append_segments).



%! uri_comp_set(+Kind:oneof([fragment,query]), +Uri1, +Component, -Uri2) is det.
%
% Change a specific URI component.

uri_comp_set(fragment, Uri1, Fragment, Uri2) :-
  uri_components(Uri1, uri_components(Scheme,Authority,Path,Query,_)),
  uri_components(Uri2, uri_components(Scheme,Authority,Path,Query,Fragment)).
uri_comp_set(query, Uri1, QueryComponents, Uri2) :-
  uri_components(Uri1, uri_components(Scheme,Authority,Path,_,Fragment)),
  uri_query_components(Query, QueryComponents),
  uri_components(Uri2, uri_components(Scheme,Authority,Path,Query,Fragment)).



%! uri_comps(+Uri, -Components) is det.
%! uri_comps(-Uri, +Components) is det.
%
% Components is a compound term of the form
% `uri(Scheme,Authority,Segments,Query,Fragment)', where:
%
%   * Authority is either an atom or a compound term of the form
%     `auth(User,Password,Host,Port)'.
%
%   * Segments is a list of atomic path segments.
%
%   * Query is (1) a list of unary compound terms, or (2) a list of
%     pairs, or (3) a flat dict (i.e., a dict with non-dict values).

uri_comps(Uri, uri(Scheme,AuthorityComp,Segments,QueryComponents,Fragment)) :-
  ground(Uri), !,
  uri_components(Uri, uri_components(Scheme,Authority,Path,Query,Fragment)),
  (   atom(Authority),
      var(AuthorityComp)
  ->  AuthorityComp = Authority
  ;   auth_comps_(Scheme, Authority, AuthorityComp)
  ),
  atomic_list_concat([''|Segments], /, Path),
  (   var(Query)
  ->  QueryComponents = []
  ;   % @hack Currently needed because buggy URI query components are
      %       common.
      catch(uri_query_components(Query, QueryComponents0), _, fail)
  ->  list_to_set(QueryComponents0, QueryComponents)
  ;   QueryComponents = []
  ).
uri_comps(Uri, uri(Scheme,Authority0,Segments,QueryComponents,Fragment)) :-
  (   atom(Authority0)
  ->  Authority = Authority0
  ;   auth_comps_(Scheme, Authority, Authority0)
  ),
  (   var(Segments)
  ->  true
  ;   Segments == ['']
  ->  Path = '/'
  ;   atomic_list_concat([''|Segments], /, Path)
  ),
  (   var(QueryComponents)
  ->  true
  ;   is_list(QueryComponents)
  ->  uri_query_components(Query, QueryComponents)
  ;   is_dict(QueryComponents)
  ->  dict_pairs(QueryComponents, QueryPairs),
      uri_query_components(Query, QueryPairs)
  ;   atomic(QueryComponents)
  ->  Query = QueryComponents
  ;   type_error(uri_query_components, QueryComponents)
  ),
  uri_components(Uri, uri_components(Scheme,Authority,Path,Query,Fragment)).

auth_comps_(_, Authority, auth(User,Password,Host,Port)) :-
  ground(Authority), !,
  uri_authority_components(Authority, uri_authority(User,Password,Host,Port)).
auth_comps_(Scheme, Authority, auth(User,Password,Host,Port0)) :-
  (   var(Port0)
  ->  true
  ;   % Leave out the port if it is the default port for the given
      % Scheme.
      ground(Scheme),
      uri:default_port(Scheme, Port0)
  ->  true
  ;   Port = Port0
  ),
  % Create the Authorityority string.
  uri_authority_components(Authority, uri_authority(User,Password,Host,Port)).



%! uri_file_extensions(+Uri:atom, -Extensions:list(atom)) is det.

uri_file_extensions(Uri, Extensions) :-
  uri_local_name(Uri, Local),
  file_extensions(Local, Extensions).



%! uri_local_name(+Uri:atom, -Local:atom) is det.

uri_local_name(Uri, Local) :-
  uri_comps(Uri, uri(_,_,Segments,_,_)),
  last(Segments, Local).



%! uri_media_type(+Uri:atom, -MediaType:compound) is det.

uri_media_type(Uri, MediaType) :-
  uri_file_extensions(Uri, Extensions),
  file_extensions_media_type(Extensions, MediaType).



%! uri_strip(+Uri1:atom, -Uri2:atom) is det.
%
% Uri2 is like Uri1, but without the query and fragment components.

uri_strip(Uri1, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Auth,Segments,_,_)),
  uri_comps(Uri2, uri(Scheme,Auth,Segments,_,_)).
