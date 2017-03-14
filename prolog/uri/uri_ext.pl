:- module(
  uri_ext,
  [
    auth_comps/2,           % ?Auth, ?Comps
    host_uri/1,             % -Uri
    host_uri/2,             % +Uri1, -Uri2
    iri_query_enc//0,
    is_data_uri/1,          % +Uri
    is_image_uri/1,         % @Term
    is_uri/1,               % +Uri
    uri_add_path_postfix/3, % +Uri1, +PathPostfix, -Uri2
    uri_alias_uuid/2,       % -Uri, +Alias
    uri_comp/3,             % +Uri, ?Key, ?Val
    uri_comps/2,            % ?Uri, ?Comps
    uri_comps/3,            % -Uri, +BaseUri, +Comps
    uri_file_extensions/2,  % +Uri,  -Exts
    uri_last_segment/2,     % +Uri, -LastSegment
    uri_optional_query_enc//0,
    uri_query_enc//0,
    uri_remove_fragment/2,  % +Uri, -BaseUri
    uri_resource/2,         % ?Uri, ?Res
    uri_segment_enc//0,
    uri_segments/2,         % ?Uri, ?Segments
    uri_segments_uuid/2     % ?Uri, ?Segments
  ]
).
:- reexport(library(uri)).

/** <module> URI extensions

@author Wouter Beek
@version 2016/11-2017/03
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_host), []).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11), [
     rdf_global_id/2
   ]).
:- use_module(library(settings)).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986)).
:- use_module(library(uri/rfc3987)).

:- setting(
     uri:data_host,
     atom,
     'example.org',
     "The host component of data IRIs."
   ).
:- setting(
     uri:data_scheme,
     oneof([http,https]),
     https,
     "The scheme of data IRIs."
   ).





%! auth_comps(+Auth, -Comps) is det.
%! auth_comps(-Auth, +Comps) is det.

auth_comps(Auth, auth(User,Host,Port)) :-
  uri_authority_components(Auth, uri_authority(User,_,Host,Port)).



%! host_uri(-Uri) is det.
%! host_uri(+Uri1, -Uri2) is det.
%
% The public URI at which the current host can be reached.

host_uri(Uri) :-
  host_uri0(_, _, _, Uri).


host_uri(Uri1, Uri2) :-
  uri_comps(Uri1, uri(_,_,Path,Query,Frag)),
  host_uri0(Path, Query, Frag, Uri2).

host_uri0(Path, Query, Frag, Uri) :-
  setting(http:public_host, Host),
  setting(http:public_port, Port),
  setting(http:public_scheme, Scheme),
  uri_comps(Uri, uri(Scheme,auth(_,Host,Port),Path,Query,Frag)).



%! iri_query_enc// .
%
% ```abnf
% iquery = *( ipchar / iprivate / "/" / "?" )
% ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
% iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
% iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
% ucschar = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
%         / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
%         / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
%         / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
%         / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
%         / %xD0000-DFFFD / %xE1000-EFFFD
% ```

iri_query_enc, "/" --> "/", !, iri_query_enc.
iri_query_enc, "?" --> "?", !, iri_query_enc.
iri_query_enc, ":" --> ":", !, iri_query_enc.
iri_query_enc, "@" --> "@", !, iri_query_enc.
iri_query_enc, [C] --> iunreserved(C), !, iri_query_enc.
iri_query_enc, [C] --> 'sub-delims'(C), !, iri_query_enc.
iri_query_enc, [C] --> iprivate(C), !, iri_query_enc.
iri_query_enc, "%", 'HEXDIG'(W1), 'HEXDIG'(W2) -->
  between_code(0, 255, C), !,
  {W1 is C // 16, W2 is C mod 16},
  iri_query_enc.
iri_query_enc --> "".



%! is_data_uri(+Uri) is semidet.

is_data_uri(Uri) :-
  uri_components(Uri, uri_components(Scheme,Host,_,_,_)),
  setting(uri:data_scheme, Scheme),
  setting(uri:data_host, Host).



%! is_image_uri(+Uri) is semidet.
%
% Succeeds if the given URI is commonly understood to denote an image
% file.

is_image_uri(Uri) :-
  uri_is_global(Uri),
  uri_components(Uri, uri_components(_,_,Path,_,_)),
  is_image_file(Path).



%! is_uri(+Uri) is semidet.

is_uri(Uri) :-
  uri_components(Uri, uri_components(Scheme,Auth,_,_,_)),
  maplist(atom, [Scheme,Auth]).



%! uri_add_path_postfix(+Uri1, +PathPostfix, -Uri2) is det.

uri_add_path_postfix(Uri1, PathPostfix, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Auth,Segments1,_,_)),
  append(Segments1, PathPostfix, Segments2),
  uri_comps(Uri2, uri(Scheme,Auth,Segments2,_,_)).



%! uri_alias_uuid(-Uri, +Alias) is det.

uri_alias_uuid(Uri, Alias) :-
  uuid(Local),
  rdf_global_id(Alias:Local, Uri).



%! uri_comp(+Uri, +Key, +Val) is semidet.
%! uri_comp(+Uri, +Key, -Val) is det.
%! uri_comp(+Uri, -Key, -Val) is multi.
%
% Abbreviates multiple predicates from `library(uri)`:
%   * uri_authority_components/2
%   * uri_authority_data/3
%   * uri_components/2
%   * uri_data/3

uri_comp(Uri, Key, Val) :-
  uri_field0(Key), !,
  uri_components(Uri, Comps),
  uri_data_compatibility(Key, Comps, Val).
uri_comp(Uri, Key, Val) :-
  auth_field0(Key), !,
  uri_components(Uri, UriComps),
  uri_data_compatibility(authority, UriComps, Auth),
  uri_authority_components(Auth, AuthComps),
  uri_authority_data(Key, AuthComps, Val).
uri_comp(_, Key0, _) :-
  aggregate_all(set(Key), uri_field(Key), Keys),
  type_error(oneof(Keys), Key0).

uri_field0(authority).
uri_field0(fragment).
uri_field0(path).
uri_field0(search).
uri_field0(scheme).

uri_data_compatibility(Key, Comps, Val) :-
  Key == query, !,
  uri_data(search, Comps, Val).
uri_data_compatibility(Key, Comps, Val) :-
  uri_data(Key0, Comps, Val),
  (Key0 == search -> Key = query ; Key = Key0).

auth_field0(host).
auth_field0(password).
auth_field0(port).
auth_field0(user).

uri_field(Key) :-
  auth_field0(Key).
uri_field(Key) :-
  uri_field0(Key).



%! uri_comps(+Uri, -Comps) is det.
%! uri_comps(-Uri, +Comps) is det.
%
% Comps is a compound term of the form
% ‘uri(Scheme,Auth,Path,Query,Frag)’, where:
%
%   * Auth is either an atom or a compound term of the form
%     ‘auth(User,Host,Port)’.
%
%   * Path is a list of atomic segments.  Allowed to be
%     uninstantiated.
%
%   * Query is (1) a list of unary compound terms, or (2) a list of
%     pairs, or (3) a flat dict (i.e., non-dict values).  Allowed to
%     be uninstantiated.

uri_comps(Uri, uri(Scheme,AuthComp,Segments,QueryComps,Frag)) :-
  ground(Uri), !,
  uri_components(Uri, uri_components(Scheme,Auth,Path,Query,Frag)),
  (atom(Auth) -> AuthComp = Auth ; auth_comps(Auth, AuthComp)),
  atomic_list_concat([''|Segments], /, Path),
  (var(Query) -> QueryComps = [] ; uri_query_components(Query, QueryComps)).
uri_comps(Uri, uri(Scheme,Auth0,Segments,QueryComps,Frag)) :-
  (atom(Auth0) -> Auth = Auth0 ; auth_comps(Auth, Auth0)),
  (   var(Segments)
  ->  true
  ;   Segments = []
  ->  Path = '/'
  ;   (Segments = [''|Segments0] -> true ; Segments0 = Segments),
      atomic_list_concat([''|Segments0], /, Path)
  ),
  (   var(QueryComps)
  ->  true
  ;   is_dict(QueryComps)
  ->  dict_pairs(QueryComps, QueryPairs),
      uri_query_components(Query, QueryPairs)
  ;   uri_query_components(Query, QueryComps)
  ),
  uri_components(Uri, uri_components(Scheme,Auth,Path,Query,Frag)).



%! uri_comps(-Uri, +BaseUri, +Comps) is det.

uri_comps(Uri, BaseUri, Comps) :-
  uri_comps(RelUri, Comps),
  uri_resolve(RelUri, BaseUri, Uri).



%! uri_file_extensions(+Uri, -Exts) is det.

uri_file_extensions(Uri, Exts) :-
  uri_comps(Uri, uri(_,_,Segments,_,_)),
  last(Segments, Segment),
  file_extensions(Segment, Exts).



%! uri_last_segment(+Uri, -LastSegment) is det.
%
% This is sometimes useful when the name of something is encoded in
% the last segments of an URI path.

uri_last_segment(Uri, LastSegment) :-
  uri_components(Uri, uri_components(_,_,Path,_,_)),
  atomic_list_concat(Segments, /, Path),
  last(Segments, LastSegment).



%! uri_optional_query_enc// .

uri_optional_query_enc, "%2C" --> ",", !, uri_optional_query_enc.
uri_optional_query_enc, "%2F" --> "/", !, uri_optional_query_enc.
uri_optional_query_enc, "%3A" --> ":", !, uri_optional_query_enc.
uri_optional_query_enc, "%40" --> "@", !, uri_optional_query_enc.
uri_optional_query_enc, [C]   --> [C], !, uri_optional_query_enc.
uri_optional_query_enc        --> [].



%! uri_query_enc// .
%
% ```abnf
% query = *( pchar / "/" / "?" )
% pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
% pct-encoded = "%" HEXDIG HEXDIG
% sub-delims = "!" / "$" / "&" / "'" / "(" / ")"
%            / "*" / "+" / "," / ";" / "="
% unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
% ```

uri_query_enc, "/" --> "/",             !, uri_query_enc.
uri_query_enc, "?" --> "?",             !, uri_query_enc.
uri_query_enc, ":" --> ":",             !, uri_query_enc.
uri_query_enc, "@" --> "@",             !, uri_query_enc.
uri_query_enc, [C] --> unreserved(C),   !, uri_query_enc.
uri_query_enc, [C] --> 'sub-delims'(C), !, uri_query_enc.
uri_query_enc, "%", hex(W1), hex(W2) -->
  between_code(0, 255, C), !,
  {W1 is C // 16, W2 is C mod 16},
  uri_query_enc.
uri_query_enc --> [].



%! uri_remove_fragment(+Uri, -BaseUri) is det.
%
% The base URI is the eventual URI that is being read from, wtihout
% the fragment component.

uri_remove_fragment(Uri, BaseUri) :-
  uri_components(Uri, uri_components(Scheme,Auth,Path,Query,_)),
  uri_components(BaseUri, uri_components(Scheme,Auth,Path,Query,_)).



%! uri_resource(+Uri, -Res) is det.
%! uri_resource(-Uri, +Res) is det.
%
% Convert between public URIs and resource/data URIs.

uri_resource(Uri, Res) :-
  ground(Uri), !,
  uri_components(Uri, uri_components(_,_,Path,_,_)),
  setting(uri:data_scheme, Scheme),
  setting(uri:data_host, Host),
  uri_components(Res, uri_components(Scheme,Host,Path,_,_)).
uri_resource(Uri, Res) :-
  uri_components(Res, uri_components(_ResScheme,_ResHost,Path,Query,Frag)), !,
  setting(http:public_scheme, UriScheme),
  setting(http:public_host, UriHost),
  /*
  (   % Data location and public location are the same, so relative
      % resource URIs will do.
      ResScheme == UriScheme,
      ResHost == UriHost
  ->  uri_components(Uri, uri_components(_,_,Path,Query,Frag))
  */
  setting(http:public_port, UriPort),
  % Default ports do not need to set explicitly.
  correct_for_default_port(UriScheme, UriPort, UriPort),
  uri_authority_components(UriAuth, uri_authority(_,_,UriHost,UriPort)),
  uri_components(Uri, uri_components(UriScheme,UriAuth,Path,Query,Frag)).

correct_for_default_port(http, 80, _) :- !.
correct_for_default_port(https, 443, _) :- !.
correct_for_default_port(_, Port, Port).



%! uri_segment_enc// is det.

uri_segment_enc, [C] -->
  pchar(C), !,
  uri_segment_enc.
uri_segment_enc, "-" -->
  [_], !,
  uri_segment_enc.
uri_segment_enc --> "".



%! uri_segments(+Uri, -Segments) is det.
%! uri_segments(-Uri, +Segments) is det.

uri_segments(Uri, Segments) :-
  ground(Uri), !,
  uri_components(Uri, uri_components(_,_,Path,_,_)),
  atomic_list_concat([''|Segments], /, Path).
uri_segments(Uri, Segments1) :-
  setting(uri:data_scheme, Scheme),
  setting(uri:data_host, Host),
  (Segments1 = [''|Segments2] -> true ; Segments2 = Segments1),
  atomic_list_concat([''|Segments2], /, Path),
  uri_components(Uri, uri_components(Scheme,Host,Path,_,_)).



%! uri_segments_uuid(-Uri, +Segments) is det.

uri_segments_uuid(Uri, Segments1) :-
  uuid(Uuid),
  append(Segments1, [Uuid], Segments2),
  uri_segments(Uri, Segments2).
