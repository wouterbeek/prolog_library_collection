:- module(
  es_api,
  [
    es_check/1,       % -Dict
    es_count/1,       % -Dict
    es_count/2,       % +PathComps, -Dict
    es_create/2,      % +PathComps, +Data
    es_create/3,      % +PathComps, +Data, -Dict
    es_exists/1,      % +PathComps
    es_get/2,         % +PathComps, -Result
    es_get/3,         % +PathComps, +Keys, -Result
    es_health/1,      % -Dict
    es_health_cat/0,
    es_indices_cat/0,
    es_nodes_cat/0,
    es_rm/1,          % +PathComps
    es_rm/2,          % +PathComps, -Dict
    es_search/2,      % +PathComps, -Pagination
    es_search/4,      % +PathComps, +Search, +PageOpts, -Pagination
    es_setting/3,     % +Index, +Key, ?Val
    es_stat/1,        % -Dict
    es_stat/2,        % +PathComps, -Dict
    es_update/2,      % +PathComps, +Data
    es_update/3       % +PathComps, +Data, -Dict
  ]
).

/** <module> Elastic Search API

A typical use of PathComps is [<INDEX>,<TYPE>,<DOC>].

# Query DSL

```swi
_{
  aggs: _{
    <NAME>: _{
      terms: _{
        field: "<KEY>"
      }
    }
  }
}
```

```swi
_{
  highlight: _{
    fields: _{
      <KEY>: _{}
    }
  },
  query: _{
    match: _{
      <KEY>: "<PATTERN>"
    }
  }
}
```

```swi
_{
  query: _{
    bool: _{
      must: _{
        match_phrase: _{
          <KEY>: "<X> <Y>"
        }
      },
      filter: _{
        range: _{
          <KEY>: _{
            gt: <NONNEG>
          }
        }
      }
    },
  }
}
```

@author Wouter Beek
@version 2016/08-2016/12
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_io)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(os/io)).
:- use_module(library(pagination)).
:- use_module(library(print_ext)).
:- use_module(library(readutil)).
:- use_module(library(settings)).
:- use_module(library(true)).
:- use_module(library(uri)).
:- use_module(library(yall)).

:- meta_predicate
    es_get0(+, +, +, 3, -).

:- setting(endpoint_host, atom, localhost, "").
:- setting(endpoint_port, positive_integer, 9200, "").
:- setting(endpoint_scheme, oneof([http,https]), http, "").





%! es_check(-Dict) is det.

es_check(Dict) :-
  es_get0(Status, Dict),
  http_status_must_be(Status, [200]).



%! es_count(-Dict) is det.
%! es_count(+PathComps, -Dict) is det.
%
% ```bash
% curl -XGET 'http://localhost:9200/_count?pretty' -d '
% {
%   "query": {
%     "match_all": {}
%   }
% }

es_count(Dict) :-
  es_count([], Dict).


es_count(PathComps1, Dict) :-
  append(PathComps1, ['_count'], PathComps2),
  es_get0(PathComps2, Status, Dict),
  http_status_must_be(Status, [200]).



%! es_create(+PathComps, +Data) is det.
%! es_create(+PathComps, +Data, -Dict) is det.
%
% Create a new document.
%
% Succeeds if a document with given Id already exists.

es_create(PathComps, Data) :-
  es_create(PathComps, Data, Dict),
  if_debug(es_api, print_dict(Dict)).


es_create([Index,Type], Data, Dict) :- !,
  % The POST method auto-generated an ElasticSearch Id.
  es_post0([Index,Type], Data, Status, Dict),
  http_status_must_be(Status, [201]).
es_create([Index,Type,Id], Data, Dict) :-
  es_put0([Index,Type,Id,'_create'], Data, Status, Dict),
  http_status_must_be(Status, [201,409]).



%! es_exists(+PathComps) is semidet.

es_exists(PathComps) :-
  es_head0(PathComps, Status),
  http_status_must_be(Status, [200]).



%! es_get(+PathComps, -Result) is det.
%! es_get(+PathComps, +Keys, -Result) is det.
%
% PathComps must be of the form [Index,Type,Id].
%
% Result contains the following keys:
%
%   - '_id'(-atom)
%
%   - '_index'(-atom)
%
%   - '_score'(-float)
%
%   - '_source'(-dict)
%
%   - '_type'(-atom)
%
% Keys, if present, filters the keys returned in '_source'.

es_get(PathComps, Result) :-
  es_get(PathComps, _VAR, Result).


es_get(PathComps, Keys, Result) :-
  assertion(PathComps = [_Index,_Type,_Id]),
  (   var(Keys)
  ->  QueryComps = []
  ;   atomic_list_concat(Keys, ',', Search),
      QueryComps = ['_source'(Search)]
  ),
  es_get0(PathComps, QueryComps, Status, Dict),
  es_dict_to_result0(Dict, Result),
  http_status_must_be(Status, [200]).



%! es_health(-Dict) is det.
%! es_health_cat is det.

es_health(Dict) :-
  es_get0(['_cluster',health], Status, Dict),
  http_status_must_be(Status, [200]).


es_health_cat :-
  es_get_cat0([health], Msg),
  writeln(Msg).



%! es_indices_cat is det.

es_indices_cat :-
  es_get_cat0([indices], Msg),
  writeln(Msg).



%! es_nodes_cat is det.

es_nodes_cat :-
  es_get_cat0([nodes], Msg),
  writeln(Msg).



%! es_rm(+PathComps) is det.
%! es_rm(+PathComps, -Dict) is det.

es_rm(PathComps) :-
  es_rm(PathComps, Dict),
  if_debug(es_api, print_dict(Dict)).


es_rm(PathComps, Dict) :-
  es_delete0(PathComps, Status, Dict),
  http_status_must_be(Status, [200]).



%! es_search(+PathComps, -Pagination) is nondet.
%! es_search(+PathComps, +Search, +PageOpts, -Pagination) is nondet.

es_search(PathComps, Pagination) :-
  es_search(PathComps, _VAR, _{}, Pagination).


es_search(PathComps1, Search, PageOpts1, Pagination2) :-
  pagination_init_options(PageOpts1, FirstPage, PageSize, PageOpts2),
  % NONDET
  between(FirstPage, inf, Page),
  From is (Page - 1) * PageSize,
  QueryComps1 = [from(From),size(PageSize)],
  append(PathComps1, ['_search'], PathComps2),
  (   % Query DSL
      is_dict(Search)
  ->  es_post0(PathComps2, QueryComps1, Search, Status, Dict)
  ;   % Simple Search
      (   var(Search)
      ->  QueryComps2 = QueryComps1
      ;   format_simple_search_string0(Search, Str),
          QueryComps2 = [q(Str)|QueryComps1]
      ),
      es_get0(PathComps2, QueryComps2, Status, Dict)
  ),
  Hits = Dict.hits,
  maplist(es_dict_to_result0, Hits.hits, Results),
  length(Results, NumResults),
  % Remove choicepoints when there are no more results.
  (NumResults =:= 0 -> !, true ; true),
  Pagination1 = _{
    number_of_results: NumResults,
    page: Page,
    page_size: PageSize,
    results: Results,
    total_number_of_results: Hits.total
  },
  merge_dicts(PageOpts2, Pagination1, Pagination2),
  http_status_must_be(Status, [200]).


format_simple_search_string0(Comp, Str) :-
  compound(Comp), !,
  Comp =.. [Key,Val],
  format(string(Str), "~a:~w", [Key,Val]).
format_simple_search_string0(Str, Str).



%! es_setting(+Index, +Key, +Val) is det.
%! es_setting(+Index, +Key, -Val) is det.
%
% Get and set settings.
%
% The following keys are supported:
%
%   - number_of_shards(nonneg)
%
%   - number_of_replicas(nonneg)
%
% @tbd Get a setting.

es_setting(Index, Key, Val) :-
  ground(Val), !,
  dict_pairs(Data, [Key-Val]),
  es_put0([Index,'_settings'], Data, Status, Dict),
  Dict.acknowledged == true,
  http_status_must_be(Status, [200]).



%! es_stat(-Dict) is det.
%! es_stat(+PathComps, -Dict) is det.

es_stat(Dict) :-
  es_stat([], Dict).


es_stat(PathComps1, Dict) :-
  append(PathComps1, ['_stats'], PathComps2),
  es_get0(PathComps2, Status, Dict),
  http_status_must_be(Status, [200]).



%! es_update(+PathComps, +Data) is det.
%! es_update(+PathComps, +Data, -Dict) is det.
%
% # Examples of Data
%
% Merge `Dict` with the existing document:
%
% ```swi
% _{doc: Dict}
% ```
%
% Groovy script [?]:
%
% ```swi
% _{script: 'ctx._source.<KEY>+=<INT>'}
% ```
%
% Groovy script with parameters [?]:
%
% ```swi
% _{
%   script: 'ctx._source.<KEY>+=new_tag',
%   params: _{new_tag : Val}
% }
% ```
%
% Dict is the document that is inserted when the document does not yet
% exist:
%
% ```swi
% _{script: ..., upsert: Dict}
% ```

es_update([Index,Type,Id], Data) :-
  es_update([Index,Type,Id], Data, Dict),
  if_debug(es_api, print_dict(Dict)).


es_update([Index,Type,Id], Data, Dict) :-
  es_post0([Index,Type,Id,'_update'], Data, Status, Dict),
  http_status_must_be(Status, [200]).





% HELPERS %

debug_dict(Dict) :-
  debugging(es_api), !,
  dcg_with_output_to(string(Msg), dcg_dict(Dict)),
  debug(es_api, "~s", [Msg]).
debug_dict(_).



%! es_delete0(+PathComps, -Status, -Dict) is det.

es_delete0(PathComps, Status, Dict) :-
  es_iri(PathComps, Iri),
  http_delete(
    Iri,
    {Dict}/[In,Path0,Path0]>>json_read_dict(In, Dict),
    [metadata(Path),request_header('Accept'='application/json')]
  ),
  dicts_getchk(status, Path, Status).



%! es_dict_to_result0(+Dict, -Result) is det.

es_dict_to_result0(Dict, Result) :-
  atom_string(Id, Dict.'_id'),
  dict_tag(Dict.'_source', Id, Result).



%! es_get0(-Status, -Dict) is det.
%! es_get0(+PathComps, -Status, -Dict) is det.
%! es_get0(+PathComps, +QueryComps, -Status, -Dict) is det.
%! es_get_cat0(+PathComps, -Msg) is det.

es_get0(Status, Dict) :-
  es_get0([], Status, Dict).


es_get0(PathComps, Status, Dict) :-
  es_get0(PathComps, [], Status, Dict).


es_get0(PathComps, QueryComps, Status, Dict) :-
  es_get0(
    PathComps,
    QueryComps,
    [request_header('Accept'='application/json')],
    {Dict}/[In,Path,Path]>>json_read_dict(In, Dict),
    Status
  ).


es_get_cat0(PathComps, Msg) :-
  es_get0(
    ['_cat'|PathComps],
    [v(true)],
    [],
    {Msg}/[In,Path,Path]>>read_stream_to_string(In, Msg),
    Status
  ),
  http_status_must_be(Status, [200]).


es_get0(PathComps, QueryComps, T, Goal_3, Status) :-
  es_iri(PathComps, QueryComps, Iri),
  http_get(Iri, Goal_3, [metadata(Path)|T]),
  dicts_getchk(status, Path, Status).



%! es_head0(+PathComps, -Status) is semidet.

es_head0(PathComps, Status) :-
  es_iri(PathComps, Iri),
  http_head(
    Iri,
    [metadata(Path),request_header('Accept'='application/json')]
  ),
  dicts_get(status, Path, Status).



%! es_iri(+PathComps, -Iri) is det.
%! es_iri(+PathComps, +QueryComps, -Iri) is det.

es_iri(PathComps, Iri) :-
  es_iri(PathComps, [], Iri).


es_iri(PathComps, QueryComps, Iri) :-
  setting(endpoint_scheme, Scheme),
  setting(endpoint_host, Host),
  setting(endpoint_port,Port),
  uri_authority_components(Auth, uri_authority(_,_,Host,Port)),
  atomic_list_concat([''|PathComps], /, Path),
  uri_query_components(Query, QueryComps),
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)).



%! es_post0(+PathComps, +Data, -Status, -Dict) is det.
%! es_post0(+PathComps, +QueryComps, +Data, -Status, -Dict) is det.

es_post0(PathComps, Data, Status, Dict) :-
  es_post0(PathComps, [], Data, Status, Dict).


es_post0(PathComps, QueryComps, Data, Status, Dict) :-
  es_iri(PathComps, QueryComps, Iri),
  debug_dict(Data),
  http_post(
    Iri,
    json(Data),
    {Dict}/[In,Path0,Path0]>>json_read_dict(In, Dict),
    [metadata(Path),request_header('Accept'='application/json')]
  ),
  dicts_getchk(status, Path, Status).



%! es_put0(+PathComps, +Data, -Status, -Dict) is det.

es_put0(PathComps, Data, Status, Dict) :-
  es_iri(PathComps, Iri),
  debug_dict(Data),
  http_put(
    Iri,
    json(Data),
    {Dict}/[In,Path0,Path0]>>json_read_dict(In, Dict),
    [metadata(Path),request_header('Accept'='application/json')]
  ),
  dicts_getchk(status, Path, Status).
