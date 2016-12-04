:- module(
  uri_ext,
  [
    auth_comps/2, % -Auth:atom, +Comps:compound
    uri_comps/2,  % -Uri:atom, +Comps:compound
    uri_comps/3,  % -Uri:atom, +BaseUri:atom, +Comps:compound
    uri_optional_query_enc//0,
    uri_query_enc//0
  ]
).
:- reexport(library(uri)).

/** <module> URI extensions

@author Wouter Beek
@version 2015/08, 2015/10-2016/04, 2016/11-2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986)).





%! auth_comps(-Auth:atom, +Comps:compound) is det.

auth_comps(Auth, auth(User0,Host0,Port)) :-
  maplist(catch_empty_atom, [User0,Host0], [User,Host]),
  uri_authority_components(Auth, uri_authority(User,_,Host,Port)).



%! uri_comps(-Uri:atom, +Comps:compound) is det.
%
% Comps is of the form uri(Scheme,Auth,Path,Query,Frag), where:
%
%   - Auth if of the form auth(User,Host,Port)
%
%   - Path is a list of atomic segments
%
%   - Query is a list of unary compound terms denoting key/value
%     pairs

uri_comps(Uri, uri(Scheme,AuthComps,Segments,QueryComps,Frag0)) :-
  (ground(AuthComps) -> auth_comps(Auth, AuthComps) ; true),
  atomic_list_concat([''|Segments], /, Path),
  uri_query_components(Query, QueryComps),
  catch_empty_atom(Frag0, Frag),
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





% HELPERS %

catch_empty_atom('', _) :- !.
catch_empty_atom(Val, Val).
