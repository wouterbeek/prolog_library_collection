:- module(
  es_api,
  [
    es_add/2,   % +PathComps, +Data
    es_check/0,
    es_count/1, % +PathComps
    es_get/1,   % +PathComps
    es_search/2 % +PathComps, +Pattern
  ]
).

/** <module> Elastic Search API

A typical use of PathComps is [Index,Type,Doc].

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(debug)).
:- use_module(library(http/http_io)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(print_ext)).
:- use_module(library(settings)).
:- use_module(library(uri)).
:- use_module(library(yall)).

:- debug(http(error)).
:- debug(http(receive_reply)).
:- debug(http(send_request)).

:- setting(endpoint_host, atom, localhost, "").
:- setting(endpoint_port, positive_integer, 9200, "").
:- setting(endpoint_scheme, oneof([http,https]), http, "").





%! es_add(+PathComps, +Data) is det.
%! es_add(+PathComps, +Data, -Dict) is det.
%
% ```bash
% curl -XPUT 'http://localhost:9200/twitter/user/kimchy' -d \
% '{ "name" : "Shay Banon" }'
% ```

es_add(PathComps, Data):-
  es_add(PathComps, Data, Dict),
  print_dict(Dict).


es_add(PathComps, Data, Dict):-
  es_post(PathComps, Data, Dict).



%! es_check is det.
%! es_check(-Dict) is det.

es_check :-
  es_check(Dict),
  print_dict(Dict).


es_check(Dict) :-
  es_get([], [], Dict).



%! es_count(+PathComps) is det.
%! es_count(+PathComps, -Dict) is det.
%
% ```bash
% curl -XGET 'http://localhost:9200/_count?pretty' -d '
% {
%     "query": {
%         "match_all": {}
%     }
% }

es_count(PathComps) :-
  es_count(PathComps, Dict),
  print_dict(Dict).


es_count(PathComps1, Dict) :-
  append(PathComps1, ['_count'], PathComps2),
  es_get(PathComps2, [], Dict).
  


%! es_get(+PathComps) is det.
%! es_get(+PathComps, -Dict) is det.

es_get(PathComps) :-
  es_get(PathComps, Dict),
  print_dict(Dict).


es_get(PathComps, Dict) :-
  es_get(PathComps, [], Dict).



%! es_search(+PathComps, +Pattern) is nondet.
%! es_search(+PathComps, +Pattern, -Dict) is nondet.

es_search(PathComps, Pattern) :-
  es_search(PathComps, Pattern, Dict),
  print_dict(Dict).


es_search(PathComps1, Pattern, Dict) :-
  append(PathComps1, ['_search'], PathComps2),
  es_get(PathComps2, [q(Pattern)], Dict).





% HELPERS %

%! es_get(+PathComps, +QueryComps, -Dict) is det.

es_get(PathComps, QueryComps, Dict) :-
  es_iri(PathComps, QueryComps, Iri),
  call_or_fail(
    http_get(
      Iri,
      {Dict}/[In,M,M]>>json_read_dict(In, Dict)
    )
  ).



%! es_iri(+PathComps, +QueryComps, -Iri) is det.

es_iri(PathComps, QueryComps, Iri) :-
  setting(endpoint_scheme, Scheme),
  setting(endpoint_host, Host),
  setting(endpoint_port,Port),
  uri_authority_components(Auth, uri_authority(_,_,Host,Port)),
  atomic_list_concat([''|PathComps], /, Path),
  uri_query_components(Query, QueryComps),
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)).



%! es_post(+PathComps, +Data, -Dict) is det.

es_post(PathComps, Data, Dict) :-
  es_iri(PathComps, [], Iri),
  call_or_fail(
    http_post(
      Iri,
      json(Data),
      {Dict}/[In,M,M]>>json_read_dict(In, Dict),
      [request_header('Accept'='application/json')]
    )
  ).
