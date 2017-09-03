:- module(
  uri_ext,
  [
    call_on_uri/2,         % +UriSpec, :Goal_3
    call_on_uri/3,         % +UriSpec, :Goal_3, +Options
    download/2,            % +Uri, +FileSpec
    download/3,            % +Uri, +FileSpec, +Options
    fresh_uri/2,           % -Uri, +Components
    iri_to_uri/2,          % +Iri, -Uri
    is_uri/1,              % @Term
    uri_comp_add/4,        % +Kind, +Uri1, +Component, -Uri2
    uri_comp_set/4,        % +Kind, +Uri1, +Component, -Uri2
    uri_comps/2,           % ?Uri, ?Components
    uri_file_extensions/2, % +Uri, -Extensions
    uri_media_type/2       % +Uri, -MediaType
  ]
).
:- reexport(library(stream_ext)).
:- reexport(library(uri)).

/** <module> URI extensions

@author Wouter Beek
@version 2017/04-2017/08
*/

:- use_module(library(apply)).
:- use_module(library(default)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(settings)).
:- use_module(library(uri/rfc3987)).
:- use_module(library(uuid)).
:- use_module(library(zlib)).

:- meta_predicate
    call_on_uri(+, 3),
    call_on_uri(+, 3, +),
    call_on_uri_scheme(+, +, 3, -, +).

:- multifile
    error:has_type/2,
    uri:default_port/2.

error:has_type(uri, Term):-
  uri_components(Term, uri_components(Scheme,Authority,Path,_,_)),
  ground(uri(Scheme,Authority,Path)).





%! call_on_uri(+UriSpec:term, :Goal_3) is nondet.
%! call_on_uri(+UriSpec:term, :Goal_3, +Options:list(compound)) is nondet.
%
% @arg UriSpec is either an atomic URI, an atomic file name, a
%      compound term uri/5 as handled by uri_comps/2, or a compound
%      term as processed by absolute_file_name/[2,3].
%
% @arg Options are passed to http_open2/4 if `Uri' has scheme `http'
%      or `https'.

call_on_uri(UriSpec, Goal_3) :-
  call_on_uri(UriSpec, Goal_3, []).


call_on_uri(UriSpec, Goal_3, Options) :-
  uri_spec(UriSpec, read, Uri),
  uri_components(Uri, uri_components(Scheme,_,_,_,_)),
  call_on_uri_scheme(Scheme, Uri, Goal_3, Metadata, Options),
  ignore(option(metadata(Metadata), Options)).

uri_spec(uri(Uri), _, Uri) :- !.
uri_spec(uri(Scheme,Authority,Segments,Query,Fragment), _, Uri) :- !,
  uri_comps(Uri, uri(Scheme,Authority,Segments,Query,Fragment)).
uri_spec(Uri, _, Uri) :-
  uri_is_global(Uri), !.
uri_spec(FileSpec, Mode, Uri) :-
  absolute_file_name(FileSpec, File, [access(Mode),expand(true)]),
  uri_file_name(Uri, File).

call_on_uri_scheme(file, Uri, Goal_3, Metadata3, Options) :- !,
  uri_file_name(Uri, File),
  setup_call_cleanup(
    open(File, read, Stream, Options),
    (
      stream_ext:call_on_stream(Stream, Goal_3, [uri{mode:read,uri:Uri}],
                                Metadata1, Options),
      stream_hash_metadata(Stream, Metadata1, Metadata2, Options)
    ),
    close(Stream)
  ),
  % Due to non-determinism, stream may or may not yet be closed..
  (var(Metadata2) -> Metadata3 = Metadata1 ; Metadata3 = Metadata2).
call_on_uri_scheme(http, Uri, Goal_3, Metadata, Options) :- !,
  http_client2:call_on_http(Uri, Goal_3, Metadata, Options).
call_on_uri_scheme(https, Uri, Goal_3, Metadata, Options) :-
  call_on_uri_scheme(http, Uri, Goal_3, Metadata, Options).



%! download(+UriSpec:term, +FileSpec:term) is det.
%! download(+UriSpec:term, +FileSpec:term, +Options:list(compound)) is det.

download(UriSpec, FileSpec) :-
  download(UriSpec, FileSpec, []).


download(UriSpec, FileSpec, Options1) :-
  merge_options([decompression(false)], Options1, Options2),
  call_on_uri(UriSpec, stream_to_file(FileSpec), Options2).



%! fresh_uri(-Uri, +Components) is det.

fresh_uri(Uri, uri(Scheme,Authority,Segments1,Query,Fragment)) :-
  uuid(Uuid),
  defval([], Segments1),
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
  ;   Segments = []
  ->  Path = '/'
  ;   (Segments = [''|Segments0] -> true ; Segments0 = Segments),
      atomic_list_concat([''|Segments0], /, Path)
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



%! uri_file_extensions(+Uri, -Extensions) is det.

uri_file_extensions(Uri, Extensions) :-
  uri_comps(Uri, uri(_,_,Segments,_,_)),
  last(Segments, Segment),
  file_extensions(Segment, Extensions).



%! uri_media_type(+Uri, -MediaType) is det.

uri_media_type(Uri, MediaType) :-
  uri_file_extensions(Uri, Extensions),
  file_extensions_media_type(Extensions, MediaType).
