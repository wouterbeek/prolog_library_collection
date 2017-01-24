:- module(
  uri_ext,
  [
    auth_comps/2,          % ?Auth, ?Comps
    uri_comp/3,            % +Uri, ?Key, ?Val
    uri_comps/2,           % ?Uri, ?Comps
    uri_comps/3,           % -Uri, +BaseUri, +Comps
    uri_file_extensions/2, % +Uri,  -Exts
    uri_optional_query_enc//0,
    uri_query_enc//0,
    uri_remove_fragment/2  % +Uri, -BaseUri
  ]
).
:- reexport(library(uri)).

/** <module> URI extensions



@author Wouter Beek
@version 2016/11-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_host), []).
:- use_module(library(os/file_ext)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986)).





%! auth_comps(+Auth:atom, -Comps:compound) is det.
%! auth_comps(-Auth:atom, +Comps:compound) is det.

auth_comps(Auth, auth(User,Host,Port)) :-
  uri_authority_components(Auth, uri_authority(User,_,Host,Port)).



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



%! uri_comps(+Uri:atom, -Comps:compound) is det.
%! uri_comps(-Uri:atom, +Comps:compound) is det.
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



%! uri_comps(-Uri:atom, +BaseUri:atom, +Comps:compound) is det.

uri_comps(Uri, BaseUri, Comps) :-
  uri_comps(RelUri, Comps),
  uri_resolve(RelUri, BaseUri, Uri).



%! uri_file_extensions(+Uri, -Exts) is det.

uri_file_extensions(Uri, Exts) :-
  uri_comps(Uri, uri(_,_,Segments,_,_)),
  last(Segments, Segment),
  file_extensions(Segment, Exts).



%! uri_optional_query_enc// .

uri_optional_query_enc, "%2C" --> ",", !, uri_optional_query_enc.
uri_optional_query_enc, "%2F" --> "/", !, uri_optional_query_enc.
uri_optional_query_enc, "%3A" --> ":", !, uri_optional_query_enc.
uri_optional_query_enc, "%40" --> "@", !, uri_optional_query_enc.
uri_optional_query_enc, [C]   --> [C], !, uri_optional_query_enc.
uri_optional_query_enc        --> [].



%! uri_query_enc// .
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
