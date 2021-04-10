:- module(
  uri_ext,
  [
    append_segments/3,     % +Segments1, +Segments2, ?Segments3
    uri_comp_get/3,        % +Kindm +Uri, ?Compound
    uri_comp_set/4,        % +Kind, +Uri1, +Component, -Uri2
    uri_comps/2,           % ?Uri, ?Components
    uri_data_directory/2,  % +Uri, -Directory
    uri_data_file/3,       % +Uri, +Local, -File
    uri_file_extensions/2, % +Uri, -Extensions
    uri_file_is_fresh/2,   % +Uri, +File
    uri_local_name/2,      % +Uri, -Local
    uri_media_type/2,      % +Uri, -MediaType
    uri_relative_path/3,   % +Uri, +Local, -RelativePath
    uri_scheme/2,          % +Uri, ?Scheme
    uri_strip/2            % +Uri, -Base
  ]
).
:- reexport(library(uri)).

/** <module> Extended support for URIs

Extends the support for URIs in the SWI-Prolog standard library.

*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(plunit)).

:- use_module(library(conf)).
:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(http_client2)).



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



%! uri_comp_get(+Kind:oneof([authority,fragment,host,password,path,port,query,scheme,user]),
%!              +Uri:uri,
%!              +Component:term) is semidet.
%! uri_comp_get(+Kind:oneof([authority,fragment,host,password,path,port,query,scheme,user]),
%!              +Uri:uri,
%!              -Component:term) is det.

uri_comp_get(authority, Uri, Authority) :- !,
  uri_comps(Uri, uri(_,Authority,_,_,_)).
uri_comp_get(fragment, Uri, Fragment) :- !,
  uri_comps(Uri, uri(_,_,_,_,Fragment)).
uri_comp_get(host, Uri, Host) :- !,
  uri_comps(Uri, uri(_,auth(_,_,Host,_),_,_,_)).
uri_comp_get(password, Uri, Password) :- !,
  uri_comps(Uri, uri(_,auth(_,Password,_,_),_,_,_)).
uri_comp_get(path, Uri, Segments) :- !,
  uri_comps(Uri, uri(_,_,Segments,_,_)).
uri_comp_get(port, Uri, Port) :- !,
  uri_comps(Uri, uri(_,auth(_,_,_,Port),_,_,_)).
uri_comp_get(query, Uri, Query) :- !,
  uri_comps(Uri, uri(_,_,_,Query,_)).
uri_comp_get(scheme, Uri, Scheme) :- !,
  uri_comps(Uri, uri(Scheme,_,_,_,_)).
uri_comp_get(user, Uri, User) :- !,
  uri_comps(Uri, uri(_,auth(User,_,_,_),_,_,_)).



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
  (atomic_list_concat([''|Segments], /, Path) -> true ; Segments = [Path]),
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
      http_open_cp:default_port(Scheme, Port0)
  ->  true
  ;   Port = Port0
  ),
  % Create the Authorityority string.
  uri_authority_components(Authority, uri_authority(User,Password,Host,Port)).



%! uri_data_directory(+Uri:atom, -Directory:atom) is det.

uri_data_directory(Uri, Dir3) :-
  data_directory(Dir1),
  uri_comps(Uri, uri(Scheme,auth(_,_,Host,_),Segments1,Query,_)),
  add_query_segments_(Segments1, Query, Segments2),
  exclude(==(''), Segments2, Segments3),
  directory_subdirectories(Dir2, [Scheme,Host|Segments3]),
  directory_file_path2(Dir1, Dir2, Dir3).

add_query_segments_(Segments, [], Segments) :- !.
add_query_segments_(Segments1, Query, Segments3) :-
  maplist(query_segment_, Query, Segments2),
  append(Segments1, [?|Segments2], Segments3).

query_segment_(Key=Value, Segment) :-
  format(atom(Segment), "~a=~a", [Key,Value]).



%! uri_data_file(+Uri:atom, +Local:atom, -File:atom) is det.

uri_data_file(Uri, Local, File) :-
  uri_data_directory(Uri, Dir),
  directory_file_path2(Dir, Local, File).



%! uri_file_extensions(+Uri:atom, -Extensions:list(atom)) is det.

uri_file_extensions(Uri, Extensions) :-
  uri_local_name(Uri, Local),
  file_extensions(Local, Extensions).



%! uri_file_is_fresh(+Uri:uri, +File:atom) is det.

uri_file_is_fresh(Uri, File) :-
  http_last_modified(Uri, LMod),
  file_is_fresh(File, LMod).



%! uri_local_name(+Uri:atom, -Local:atom) is det.

uri_local_name(Uri, Local) :-
  uri_comps(Uri, uri(_,_,Segments,_,_)),
  last(Segments, Local).



%! uri_media_type(+Uri:atom, -MediaType:media_type) is det.

uri_media_type(Uri, MediaType) :-
  uri_file_extensions(Uri, Extensions),
  file_extensions_media_type(Extensions, MediaType).



%! uri_relative_path(+Uri:atom, +Local:atom, -RelativePath:atom) is det.

uri_relative_path(Uri, Local, RelativePath) :-
  uri_comps(Uri, uri(Scheme,auth(_,_,Host,_),Segments1,_,_)),
  append(Segments1, [Local], Segments2),
  atomic_list_concat([Scheme,Host|Segments2], /, RelativePath).



%! uri_scheme(+Uri:atom, +Scheme:atom) is semidet.
%! uri_scheme(+Uri:atom, -Scheme:atom) is semidet.

uri_scheme(Uri, Scheme) :-
  uri_components(Uri, uri_components(Scheme,_,_,_,_)).



%! uri_strip(+Uri1:atom, -Uri2:atom) is det.
%
% Uri2 is like Uri1, but without the query and fragment components.

uri_strip(Uri1, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Auth,Segments,_,_)),
  uri_comps(Uri2, uri(Scheme,Auth,Segments,_,_)).
