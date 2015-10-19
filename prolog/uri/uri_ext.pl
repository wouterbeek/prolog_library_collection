:- module(
  uri_ext,
  [
    uri_add_query/3, % +From:or([compound,uri])
                     % +New:or([atom,compound,list(compound),list(or([atom,list(compound)]))])
                     % -To:uri
    uri_change_component/4, % +Iri1:atom
                            % +Field:oneof([authority,fragment,path,query,scheme])
                            % +Value:atom
                            % -Iri2:atom
    uri_component/3 % +Uri:uri
                    % +Field:atom
                    % ?Data:atom
  ]
).
:- reexport(library(uri)).

/** <module> URI extensions

Additional predicates for handling URIs.

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(lambda)).
:- use_module(library(option)).
:- use_module(library(uri)).

:- meta_predicate(uri_change_query(+,2,-)).
:- meta_predicate(uri_change_query_options(+,2,-)).





%! uri_add_query(
%!   +From:or([compound,uri]),
%!   +New:or([atom,compound,list(compound),list(or([atom,list(compound)]))]),
%!   -To:uri
%! ) is det.
% The query component of a URI is not interpreted as key-value pairs
% and can be any string that adheres to URI syntax.

uri_add_query(Uri1, Name, Uri2):-
  atom(Name), !,
  uri_change_query(
    Uri1,
    \Query1^Query2^atomic_list_concat([Query1,Name], '&', Query2),
    Uri2
  ).
uri_add_query(Uri1, Opts, Uri2):-
  is_list(Opts), !,
  uri_change_query_options(
    Uri1,
    \Opts1^Opts2^merge_options(Opts, Opts1, Opts2),
    Uri2
  ).
uri_add_query(Uri1, Opt, Uri2):-
  uri_add_query(Uri1, [Opt], Uri2).



%! uri_change_component(
%!   +Iri1:atom,
%!   +Field:oneof([authority,fragment,path,query,scheme]),
%!   +Value:atom,
%!   -Iri2:atom
%! ) is det.

uri_change_component(Iri1, N, V, Iri2):-
  uri_components(Iri1, Comps1),
  uri_change_component0(N, Comps1, V, Comps2),
  uri_components(Iri2, Comps2).

uri_change_component0(authority, uri_components(S,_,P,Q,F), A, uri_components(S,A,P,Q,F)).
uri_change_component0(fragment,  uri_components(S,A,P,Q,_), F, uri_components(S,A,P,Q,F)).
uri_change_component0(path,      uri_components(S,A,_,Q,F), P, uri_components(S,A,P,Q,F)).
uri_change_component0(query,     uri_components(S,A,P,_,F), Q, uri_components(S,A,P,Q,F)).
uri_change_component0(scheme,    uri_components(_,A,P,Q,F), S, uri_components(S,A,P,Q,F)).



%! uri_component(
%!   +Uri:uri,
%!   +Field:oneof([authority,fragment,host,password,path,port,search,scheme,user]),
%!   +Data:atom
%! ) is semidet.
%! uri_component(
%!   +Uri:uri,
%!   +Field:oneof([authority,fragment,host,password,path,port,search,scheme,user]),
%!   -Data:atom
%! ) is det.
% Abbreviates multiple predicates from `library(uri)`:
%   - uri_authority_components/2
%   - uri_authority_data/3
%   - uri_components/2
%   - uri_data/3

uri_component(Uri, Field, Data):-
  uri_field0(Field), !,
  uri_components(Uri, L),
  uri_data(Field, L, Data).
uri_component(Uri, Field, Data):-
  authority_field0(Field), !,
  uri_components(Uri, L0),
  uri_data(authority, L0, Authority),
  uri_authority_components(Authority, L),
  uri_authority_data(Field, L, Data).
uri_component(_, Field, _):-
  aggregate_all(set(X), uri_field(X), L),
  type_error(oneof(L), Field).

uri_field(Field):-
  authority_field0(Field).
uri_field(Field):-
  uri_field0(Field).

authority_field0(host).
authority_field0(password).
authority_field0(port).
authority_field0(user).

uri_field0(authority).
uri_field0(fragment).
uri_field0(path).
uri_field0(search).
uri_field0(scheme).





% HELPERS %

%! uri_components0(+Input:or([compound,url]), -UriComponents:compound) is det.
% Slight optimization that allows predicate to be called with URI components
% compound term i.o. URIs.
% This helps in cases where the URI would otherwise have to be build
% in the called context only to be decomposed inside the call.

uri_components0(
  uri_components(Scheme,Auth,Path,Search,Frag),
  uri_components(Scheme,Auth,Path,Search,Frag)
):- !.
uri_components0(Uri, UriComps):-
  uri_components(Uri, UriComps).



%! uri_change_query(+From:or([compound,uri]), :Goal_2, -To:uri) is det.
% Deterministic if `Goal_2` is deterministic.
%
% Meta-wrapper that is used by every predicate that operates
% on a URI's query string.
%
% The additional arguments of `Goal_2` are the list of query parameters
% before and after calling.

uri_change_query(Uri1, Goal_2, Uri2):-
  % Disasseble from URI.
  uri_components0(Uri1, uri_components(Scheme,Auth,Path,Q1,Frag)),
  % BEWARE: If a URL has no query string,
  % then uri_components/2 does not give back the empty atom.
  % Instead, it leaves `Query1` uninstantiated.
  defval('', Q1),

  call(Goal_2, Q1, Q2),

  % Back to URI.
  uri_components(Uri2, uri_components(Scheme,Auth,Path,Q2,Frag)).



%! uri_change_query_options(
%!   +From:or([compound,uri]),
%!   :Goal_2,
%!   -To:uri
%! ) is det.

uri_change_query_options(Uri1, Goal_2, Uri2):-
  uri_change_query(
    Uri1,
    \Q1^Q2^(
      uri_query_components(Q1, Opts1),
      call(Goal_2, Opts1, Opts2),
      uri_query_components(Q2, Opts2)
    ),
    Uri2
  ).
