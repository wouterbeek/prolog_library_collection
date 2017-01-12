:- module(
  uri_ext,
  [
    auth_comps/2, % ?Auth:atom, ?Comps:compound
    uri_comps/2,  % ?Uri:atom, ?Comps:compound
    uri_comps/3,  % -Uri:atom, +BaseUri:atom, +Comps:compound
    uri_optional_query_enc//0,
    uri_query_enc//0
  ]
).
:- reexport(library(uri)).

/** <module> URI extensions



@author Wouter Beek
@version 2015/08, 2015/10-2016/04, 2016/11-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_host), []).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986)).





%! auth_comps(+Auth:atom, -Comps:compound) is det.
%! auth_comps(-Auth:atom, +Comps:compound) is det.

auth_comps(Auth, auth(User,Host,Port)) :-
  uri_authority_components(Auth, uri_authority(User,_,Host,Port)).



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
  auth_comps(Auth, AuthComp),
  atomic_list_concat([''|Segments], /, Path),
  (var(Query) -> QueryComps = [] ; uri_query_components(Query, QueryComps)).
uri_comps(Uri, uri(Scheme,Auth0,Segments,QueryComps,Frag)) :-
  (atom(Auth0) -> Auth = Auth0 ; auth_comps(Auth, Auth0)),
  (   var(Segments)
  ->  true
  ;   Segments = []
  ->  Path = '/'
  ;   atomic_list_concat([''|Segments], /, Path0),
      atom_concat(Path0, /, Path)
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
