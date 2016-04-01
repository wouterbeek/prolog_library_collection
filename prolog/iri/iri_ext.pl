:- module(
  iri_ext,
  [
    iri_add_query_comp/3,  % +Iri1, +Comp,  -Iri2
    iri_add_query_comps/3, % +Iri1, +Comps, -Iri2
    iri_change_comp/4,     % +Iri1, +N, +V, -Iri2
    iri_comp/3,            % +Iri,  ?N, ?V
    iri_comps/2,           % ?Iri,  ?Comps
    iri_file_extensions/2, % +Iri,  -Exts
    iri_path/2,            % -Iri,  +Path
    iri_query_enc//0
  ]
).
:- reexport(library(uri/uri_ext)).

/** <module> IRI extensions

@author Wouter Beek
@version 2015/11-2015/12, 2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(http/http_path)).
:- use_module(library(iri/rfc3987)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(settings)).
:- use_module(library(uri/rfc3986)).
:- use_module(library(yall)).

:- meta_predicate
    iri_change_query_comps0(+, 2, -),
    iri_change_query_comps00(2, +, -),
    iri_change_query0(+, 2, -).





%! iri_add_query_comp(+Iri1, +Query, -Iri2) is det.
% The query component of a URI is not interpreted as key-value pairs
% and can be any string that adheres to URI syntax.

iri_add_query_comp(Iri1, Query, Iri2) :-
  iri_add_query_comps(Iri1, [Query], Iri2).



%! iri_add_query_comps(+Iri1, +Queries, -Iri2) is det.

iri_add_query_comps(Iri1, Comps, Iri3) :-
  partition(is_pair, Comps, Pairs, Atoms),
  iri_change_query_comps0(Iri1, merge_pairs0(Pairs), Iri2),
  iri_change_query0(Iri2, append_atoms0(Atoms), Iri3).

merge_pairs0([], L, L) :- !.
merge_pairs0([N-_|T1], L2a, [N-V2|T3]) :-
  selectchk(N-V2, L2a, L2b), !,
  merge_pairs0(T1, L2b, T3).
merge_pairs0([N-V|T1], L2, [N-V|T3]) :-
  merge_pairs0(T1, L2, T3).

append_atoms0(L, Q1, Q2) :-
  atomic_list_concat([Q1|L], '&', Q2).



%! iri_change_comp(+Iri1, +N, +V, -Iri2) is det.
% The following values for Field are supported:
%   - `authority'
%   - `fragment'
%   - `path'
%   - `query'
%   - `scheme'

iri_change_comp(Iri1, N, V, Iri2) :-
  iri_comps(Iri1, Comps1),
  iri_change_comp0(N, Comps1, V, Comps2),
  iri_comps(Iri2, Comps2).

iri_change_comp0(authority, uri_components(S,_,P,Q,F), A, uri_components(S,A,P,Q,F)).
iri_change_comp0(fragment,  uri_components(S,A,P,Q,_), F, uri_components(S,A,P,Q,F)).
iri_change_comp0(path,      uri_components(S,A,_,Q,F), P, uri_components(S,A,P,Q,F)).
iri_change_comp0(query,     uri_components(S,A,P,_,F), Q, uri_components(S,A,P,Q,F)).
iri_change_comp0(scheme,    uri_components(_,A,P,Q,F), S, uri_components(S,A,P,Q,F)).



%! iri_comp(+Iri, +N, +V) is semidet.
%! iri_comp(+Iri, +N, -V) is det.
%! iri_comp(+Iri, -N, -V) is multi.
% Abbreviates multiple predicates from `library(uri)`:
%   - uri_authority_components/2
%   - uri_authority_data/3
%   - uri_components/2
%   - uri_data/3

iri_comp(Iri, N, V) :-
  iri_field0(N), !,
  iri_comps(Iri, Comps),
  uri_data(N, Comps, V).
iri_comp(Iri, N, V) :-
  auth_field0(N), !,
  iri_comps(Iri, IriComps),
  uri_data(authority, IriComps, Auth),
  uri_authority_components(Auth, AuthComps),
  uri_authority_data(N, AuthComps, V).
iri_comp(_, N0, _) :-
  aggregate_all(set(N), iri_field(N), Ns),
  type_error(oneof(Ns), N0).

iri_field(N) :-
  auth_field0(N).
iri_field(N) :-
  iri_field0(N).

auth_field0(host).
auth_field0(password).
auth_field0(port).
auth_field0(user).

iri_field0(authority).
iri_field0(fragment).
iri_field0(path).
iri_field0(search).
iri_field0(scheme).



%! iri_comps(+Iri, -Comps) is det.
% Slight optimization that allows this predicate to also be called with
% a compound term i.o. an atomic IRI.  This is more optimal in cases where
% the IRI is build in the called context only to be decomposed inside the call.

iri_comps(
  uri_components(Scheme,Auth,Path,Search,Frag),
  uri_components(Scheme,Auth,Path,Search,Frag)
) :- !.
iri_comps(Iri, Comps) :-
  uri_components(Iri, Comps).



%! iri_file_extensions(+Iri, -Exts) is det.

iri_file_extensions(Iri, Exts) :-
  iri_comp(Iri, path, Path),
  file_extensions(Path, Exts).



%! iri_path(-Iri, +Path) is det.

iri_path(Iri, Path) :-
  http_absolute_uri(/, Prefix0),
  atom_concat(Prefix, /, Prefix0),
  atom_concat(Prefix, Path, Iri).



%! iri_query_enc// .
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





% HELPERS %

%! iri_change_query0(+Iri1, :Goal_2, -Iri2) is det.
% Deterministic if `Goal_2` is deterministic.
%
% Meta-wrapper that is used by every predicate that operates
% on a IRI's query string.
%
% The additional arguments of `Goal_2` are the list of query parameters
% before and after calling.

iri_change_query0(Iri1, Goal_2, Iri2) :-
  iri_comps(Iri1, uri_components(Scheme,Auth,Path,Q1,Frag)),
  % BEWARE: If an IRI has no query string,
  % then iri_comps/2 does not give back the empty atom.
  % Instead, it leaves `Q1` uninstantiated.
  defval('', Q1),
  call(Goal_2, Q1, Q2),
  iri_comps(Iri2, uri_components(Scheme,Auth,Path,Q2,Frag)).



%! iri_change_query_comps0(+Iri1, :Goal_2, -Iri2) is det.

iri_change_query_comps0(Iri1, Goal_2, Iri2) :-
  iri_change_query0(Iri1, iri_change_query_comps00(Goal_2), Iri2).

iri_change_query_comps00(Goal_2, Q1, Q2) :-
  uri_query_components(Q1, Pairs1),
  call(Goal_2, Pairs1, Pairs2),
  uri_query_components(Q2, Pairs2).
