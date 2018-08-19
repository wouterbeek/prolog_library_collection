:- encoding(utf8).
:- module(
  uri_ext,
  [
    file_download/2,       % +Uri, +File
    file_download/3,       % +Uri, +File, +Options
    fresh_uri/2,           % -Uri, +Components
    uri_comp_add/4,        % +Kind, +Uri1, +Component, -Uri2
    uri_comp_set/4,        % +Kind, +Uri1, +Component, -Uri2
    uri_hash/2,            % +Uri, -Hash
    uri_local_name/2,      % +Uri, ?Local
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
:- use_module(library(plunit)).
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





%! file_download(+Uri:atom, +File:atom) is det.
%! file_download(+Uri:atom, +File:atom, +Options:list(compound)) is det.

file_download(Uri, File) :-
  file_download(Uri, File, []).


file_download(Uri, File, Options) :-
  http_open2(Uri, In, Options),
  call_cleanup(
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



%! uri_comp_add(+Kind:oneof([path,query]), +Uri1:uri, +Component, -Uri2:uri) is det.
%
% For `Kind=path' we remove empty segments in the middle of a path.
% For example, adding `[b]` to `/a/' is `/a/b' (and not `/a//b').

uri_comp_add(path, Uri1, Segments2, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Authority,Segments1,Query,Fragment)),
  append_segments(Segments1, Segments2, Segments3),
  uri_comps(Uri2, uri(Scheme,Authority,Segments3,Query,Fragment)).
uri_comp_add(query, Uri1, QueryComps2, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Authority,Segments,QueryComps1,Fragment)),
  append(QueryComps1, QueryComps2, QueryComps3),
  uri_comps(Uri2, uri(Scheme,Authority,Segments,QueryComps3,Fragment)).

:- begin_tests(uri_comp_add).

test('uri_comp_add(+,+,+,-)', [forall(test_uri_comp_add(Kind,Uri1,Comp,Uri2))]) :-
  uri_comp_add(Kind, Uri1, Comp, Uri2_),
  assertion(Uri2_ = Uri2).

test_uri_comp_add(query, 'https://example.org/?a=b', [c(d)], 'https://example.org/?a=b&c=d').

:- end_tests(uri_comp_add).



%! uri_comp_set(+Kind:oneof([fragment,query]), +Uri1, +Component, -Uri2) is det.

uri_comp_set(fragment, Uri1, Fragment, Uri2) :-
  uri_components(Uri1, uri_components(Scheme,Authority,Path,Query,_)),
  uri_components(Uri2, uri_components(Scheme,Authority,Path,Query,Fragment)).
uri_comp_set(query, Uri1, QueryComponents, Uri2) :-
  uri_components(Uri1, uri_components(Scheme,Authority,Path,_,Fragment)),
  uri_query_components(Query, QueryComponents),
  uri_components(Uri2, uri_components(Scheme,Authority,Path,Query,Fragment)).



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



%! uri_hash(+Uri:atom, -Hash:atom) is det.

uri_hash(Uri1, Hash) :-
  uri_normalized(Uri1, Uri2),
  md5(Uri2, Hash).



%! uri_local_name(+Uri:atom, +Local:atom) is semidet.
%! uri_local_name(+Uri:atom, -Local:atom) is semidet.

uri_local_name(Uri, Local) :-
  uri_comps(Uri, uri(_,_,Segments,_,Fragment)),
  (   atom(Fragment)
  ->  Local = Fragment
  ;   last(Segments, Local)
  ).
