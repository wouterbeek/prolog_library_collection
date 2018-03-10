:- encoding(utf8).
:- module(
  uri_ext,
  [
    append_segments/3,     % +Segments1, +Segments2, -Segments3
    file_download/2,       % +Uri, +File
    file_download/3,       % +Uri, +File, +Options
    fresh_uri/2,           % -Uri, +Components
    iri_to_uri/2,          % +Iri, -Uri
    is_http_uri/1,         % @Term
    is_uri/1,              % @Term
    uri_comp_add/4,        % +Kind, +Uri1, +Component, -Uri2
    uri_comp_set/4,        % +Kind, +Uri1, +Component, -Uri2
    uri_comps/2,           % ?Uri, ?Components
    uri_file_extensions/2, % +Uri, -Extensions
    uri_file_local/2,      % +Uri, -Local
    uri_hash/2,            % +Uri, -Hash
    uri_media_type/2       % +Uri, -MediaType
  ]
).
:- reexport(library(uri)).

/** <module> URI extensions

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(settings)).
:- use_module(library(uuid)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(library(default)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(http/http_client2)).

:- multifile
    error:has_type/2,
    uri:default_port/2.

error:has_type(uri, Term):-
  uri_components(Term, uri_components(Scheme,Authority,Path,_,_)),
  ground(uri(Scheme,Authority,Path)).





%! append_segments(+Segments1:list(atom), +Segments2:list(atom),
%!                 -Segments3:list(atom)) is det.

append_segments(L1a, L2a, L3) :-
  exclude([X]>>(X==''), L1a, L1b),
  exclude([X]>>(X==''), L2a, L2b),
  append(L1b, L2b, L3).



%! file_download(+Uri:atom, +File:atom) is det.
%! file_download(+Uri:atom, +File:atom, +Options:list(compound)) is det.

file_download(Uri, File) :-
  file_download(Uri, File, []).


file_download(Uri, File, Options) :-
  setup_call_cleanup(
    http_open2(Uri, In, Options),
    setup_call_cleanup(
      open(File, write, Out, [type(binary)]),
      copy_stream_data(In, Out),
      close(Out)
    ),
    close(In)
  ).



%! fresh_uri(-Uri, +Components) is det.

fresh_uri(Uri, uri(Scheme,Authority,Segments1,Query,Fragment)) :-
  uuid(Uuid),
  default_value(Segments1, []),
  append(Segments1, [Uuid], Segments2),
  uri_comps(Uri, uri(Scheme,Authority,Segments2,Query,Fragment)).



%! iri_to_uri(+Iri:atom, -Uri:atom) is det.
%
% # Example
%
% ```prolog
% ?- iri_to_uri('http://dbpedia.org/resource/%C3%84iwoo_language', Uri).
% Uri = 'http://dbpedia.org/resource/Ã%84iwoo_language'.
% ```

iri_to_uri(Iri, Uri) :-
  once(atom_phrase('IRI'(Comps), Iri)),
  once(atom_phrase('URI'(Comps), Uri)).



%! is_http_uri(@Term) is semidet.
%
% Succeeds iff Term is an atom that conforms to the URI grammar.

is_http_uri(Uri) :-
  atom(Uri),
  uri_components(Uri, Comps),
  uri_data(scheme, Comps, Scheme),
  memberchk(Scheme, [http,https]).



%! is_uri(@Term) is semidet.
%
% Succeeds iff Term is an atom that conforms to the URI grammar.

is_uri(Uri) :-
  atom(Uri),
  uri_is_global(Uri).



%! uri_comp_add(+Kind:oneof([path,query]), +Uri1, +Component, -Uri2) is det.
%
% For `Kind=path' we remove empty segments in the middle of a path.
% For example, adding `[b]` to `/a/' is `/a/b' (and not `/a//b').

uri_comp_add(path, Uri1, Segments3, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Authority,Segments1,Query,Fragment)),
  remove_last_segment_if_empty1(Segments1, Segments2),
  append(Segments2, Segments3, Segments4),
  uri_comps(Uri2, uri(Scheme,Authority,Segments4,Query,Fragment)).
uri_comp_add(query, Uri1, QueryComps2, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Authority,Segments,QueryComps1,Fragment)),
  append(QueryComps1, QueryComps2, QueryComps3),
  uri_comps(Uri2, uri(Scheme,Authority,Segments,QueryComps3,Fragment)).

remove_last_segment_if_empty1(Segments1, Segments2) :-
  append(Segments2, [''], Segments1), !.
remove_last_segment_if_empty1(Segments, Segments).



%! uri_comp_set(+Kind:oneof([fragment,query]), +Uri1, +Component, -Uri2) is det.

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
  ;   auth_comps0(Scheme, Authority, AuthorityComp)
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
  ;   auth_comps0(Scheme, Authority, Authority0)
  ),
  (   var(Segments)
  ->  true
  ;   Segments == ['']
  ->  Path = '/'
  ;   atomic_list_concat([''|Segments], /, Path)
  ),
  (   var(QueryComponents)
  ->  true
  ;   is_dict(QueryComponents)
  ->  dict_pairs(QueryComponents, QueryPairs),
      uri_query_components(Query, QueryPairs)
  ;   uri_query_components(Query, QueryComponents)
  ),
  uri_components(Uri, uri_components(Scheme,Authority,Path,Query,Fragment)).

auth_comps0(_, Authority, auth(User,Password,Host,Port)) :-
  ground(Authority), !,
  uri_authority_components(Authority, uri_authority(User,Password,Host,Port)).
auth_comps0(Scheme, Authority, auth(User,Password,Host,Port0)) :-
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



%! uri_dict(+Uri:atom, -Dict:dict) is det.

uri_dict(Uri, Dict) :-
  uri_comps(Uri, uri(Scheme,auth(User,Password,Host,Port),Segments,Query,Fragment)),
  include(ground, [host-Host,password-Password,port-Port,user-User], AuthorityPairs),
  dict_pairs(AuthorityDict, authority, AuthorityPairs),
  include(
    ground,
    [
      authority-AuthorityDict,
      fragment-Fragment,
      scheme-Scheme,
      segments-Segments,
      query-Query
    ],
    Pairs
  ),
  dict_pairs(Dict, uri, Pairs).



%! uri_file_extensions(+Uri:atom, -Extensions:list(atom)) is det.

uri_file_extensions(Uri, Extensions) :-
  uri_file_local(Uri, Local),
  file_extensions(Local, Extensions).



%! uri_file_local(+Uri:atom, -Local:atom) is det.

uri_file_local(Uri, Local) :-
  uri_comps(Uri, uri(_,_,Segments,_,_)),
  last(Segments, Local).
  


%! uri_hash(+Uri:atom, -Hash:atom) is det.

uri_hash(Uri1, Hash) :-
  uri_normalized(Uri1, Uri2),
  md5(Uri2, Hash).



%! uri_media_type(+Uri, -MediaType) is det.

uri_media_type(Uri, MediaType) :-
  uri_file_extensions(Uri, Extensions),
  file_extensions_media_type(Extensions, MediaType).
