:- module(
  es_api,
  [
    es_check/1,       % -Dict
    es_count/1,       % -Dict
    es_count/2,       % +PathComps, -Dict
    es_create/2,      % +PathComps, +Data
    es_create/3,      % +PathComps, +Data, -Dict
    es_delete/1,      % +PathComps
    es_delete/2,      % +PathComps, -Dict
    es_exists/1,      % +PathComps
    es_get/2,         % +PathComps, -Result
    es_get/3,         % +PathComps, +Keys, -Result
    es_health/1,      % -Dict
    es_health_cat/0,
    es_indices_cat/0,
    es_nodes_cat/0,
    es_search/2,      % +PathComps, -Result
    es_search/4,      % +PathComps, +Search, +PageOpts, -Result
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
@version 2016/08-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/json)).
:- use_module(library(http/rest)).
:- use_module(library(io)).
:- use_module(library(lists)).
:- use_module(library(pagination/pagination), []).
:- use_module(library(print_ext)).
:- use_module(library(readutil)).
:- use_module(library(settings)).
:- use_module(library(true)).
:- use_module(library(uri)).
:- use_module(library(yall)).

:- setting(endpoint_host, atom, localhost, "").
:- setting(endpoint_port, positive_integer, 9200, "").
:- setting(endpoint_scheme, oneof([http,https]), http, "").





%! es_check(-Dict) is det.

es_check(Dict) :-
  es_get0(Dict).



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
  es_get0(PathComps2, Dict).



%! es_create(+PathComps, +Data) is det.
%! es_create(+PathComps, +Data, -Dict) is det.
%
% Create a new document.
%
% Succeeds if a document with given Id already exists.

es_create(PathComps, Data) :-
  es_create(PathComps, Data, Dict),
  if_debug(es_api, print_dict(Dict)).


% POST /<INDEX>/<TYPE>
es_create([Index,Type], Data, Dict) :- !,
  % The POST method auto-generated an ElasticSearch ID.
  es_post0([Index,Type], Data, [201], Dict).
% PUT /<INDEX>/<TYPE>/<ID>
es_create([Index,Type,Id], Data, Dict) :-
  es_put0([Index,Type,Id,'_create'], Data, [201], [409], Dict).



%! es_delete(+PathComps) is det.
%! es_delete(+PathComps, -Dict) is det.

es_delete(PathComps) :-
  es_delete(PathComps, Dict),
  if_debug(es_api, print_dict(Dict)).


es_delete(PathComps, Dict) :-
  es_uri(PathComps, Uri),
  call_on_stream(
    uri(Uri),
    rest_reply(Dict, [200], [404]),
    [method(delete),request_header('Accept'='application/json')]
  ).



%! es_exists(+PathComps) is semidet.

es_exists(PathComps) :-
  es_uri(PathComps, Uri),
  call_on_stream(
    uri(Uri),
    rest_reply(_, [200], [404]),
    [method(head),request_header('Accept'='application/json')]
  ).



%! es_get(+PathComps, -Result) is semidet.
%! es_get(+PathComps, +Keys, -Result) is semidet.
%
% PathComps must be of the form [Index,Type,Id].
%
% Result contains the following keys:
%
%   * '_id'(-atom)
%
%   * '_index'(-atom)
%
%   * '_score'(-float)
%
%   * '_source'(-dict)
%
%   * '_type'(-atom)
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
  es_get0(PathComps, QueryComps, Dict),
  _{found: true} :< Dict,
  es_dict_to_result0(Dict, Result).



%! es_health(-Dict) is det.

es_health(Dict) :-
  es_get0(['_cluster',health], Dict).



%! es_health_cat is det.

es_health_cat :-
  es_get_cat0([health], Str),
  writeln(Str).



%! es_indices_cat is det.

es_indices_cat :-
  es_get_cat0([indices], Str),
  writeln(Str).



%! es_nodes_cat is det.

es_nodes_cat :-
  es_get_cat0([nodes], Str),
  writeln(Str).



%! es_search(+PathComps, -Result) is nondet.
%! es_search(+PathComps, +Search, +PageOpts, -Result) is nondet.
%
% Result uses pagination.

es_search(PathComps, Result) :-
  es_search(PathComps, _VAR, _{}, Result).


es_search(PathComps1, Search, PageOpts1, Result2) :-
  pagination:pagination_init_options(PageOpts1, FirstPage, PageSize, PageOpts2),
  % NONDET
  between(FirstPage, inf, Page),
  From is (Page - 1) * PageSize,
  QueryComps1 = [from(From),size(PageSize)],
  append(PathComps1, ['_search'], PathComps2),
  (   % Query DSL
      is_dict(Search)
  ->  es_post0(PathComps2, QueryComps1, Search, [200], Dict)
  ;   % Simple Search
      (   var(Search)
      ->  QueryComps2 = QueryComps1
      ;   format_simple_search_string0(Search, Str),
          QueryComps2 = [q(Str)|QueryComps1]
      ),
      es_get0(PathComps2, QueryComps2, Dict)
  ),
  Hits = Dict.hits,
  maplist(es_dict_to_result0, Hits.hits, Results),
  length(Results, NumResults),
  % Remove choicepoints when there are no more results.
  (NumResults =:= 0 -> !, true ; true),
  Result1 = _{
    number_of_results: NumResults,
    page: Page,
    page_size: PageSize,
    results: Results,
    total_number_of_results: Hits.total
  },
  merge_dicts(PageOpts2, Result1, Result2).

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
  es_put0([Index,'_settings'], Data, [201], Dict),
  Dict.acknowledged == true.



%! es_stat(-Dict) is det.
%! es_stat(+PathComps, -Dict) is det.

es_stat(Dict) :-
  es_stat([], Dict).


es_stat(PathComps1, Dict) :-
  append(PathComps1, ['_stats'], PathComps2),
  es_get0(PathComps2, Dict).



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
  es_post0([Index,Type,Id,'_update'], Data, [200], Dict).





% HELPERS %

debug_dict(Dict) :-
  debugging(es_api), !,
  dcg_with_output_to(string(Str), dcg_dict(Dict)),
  debug(es_api, "~s", [Str]).
debug_dict(_).



%! es_dict_to_result0(+Dict, -Result) is det.

es_dict_to_result0(Dict, Result) :-
  atom_string(Id, Dict.'_id'),
  dict_tag(Dict.'_source', Id, Result).



%! es_get0(-Dict) is det.
%! es_get0(+PathComps, -Dict) is det.
%! es_get0(+PathComps, +QueryComps, -Dict) is det.

es_get0(Dict) :-
  es_get0([], Dict).


es_get0(PathComps, Dict) :-
  es_get0(PathComps, [], Dict).


es_get0(PathComps, QueryComps, Dict) :-
  es_uri(PathComps, QueryComps, Uri),
  call_on_stream(
    Uri,
    rest_reply(Dict, [200], [404]),
    [method(get),request_header('Accept'='application/json')]
  ).



%! es_get_cat0(+PathComps, -Str) is det.

es_get_cat0(PathComps, Str) :-
  es_uri(['_cat'|PathComps], [v(true)], Uri),
  call_on_stream(
    Uri,
    rest_reply(Str, [200], [404]),
    [method(get),request_header('Accept'='text/plain')]
  ).



%! es_uri(+PathComps, -Uri) is det.
%! es_uri(+PathComps, +QueryComps, -Uri) is det.

es_uri(PathComps, Uri) :-
  es_uri(PathComps, [], Uri).


es_uri(PathComps, QueryComps, Uri) :-
  setting(endpoint_scheme, Scheme),
  setting(endpoint_host, Host),
  setting(endpoint_port,Port),
  uri_authority_components(Auth, uri_authority(_,_,Host,Port)),
  atomic_list_concat([''|PathComps], /, Path),
  uri_query_components(Query, QueryComps),
  uri_components(Uri, uri_components(Scheme,Auth,Path,Query,_)).



%! es_post0(+PathComps, +Data, +Success, -Dict) is det.
%! es_post0(+PathComps, +QueryComps, +Data, +Success, -Dict) is det.

es_post0(PathComps, Data, Success, Dict) :-
  es_post0(PathComps, [], Data, Success, Dict).


es_post0(PathComps, QueryComps, Data, Success, Dict) :-
  es_uri(PathComps, QueryComps, Uri),
  debug_dict(Data),
  call_on_stream(
    uri(Uri),
    json(Data),
    rest_reply(Dict, Success),
    [method(post),request_header('Accept'='application/json')]
  ).



%! es_put0(+PathComps, +Data, +Success, -Dict) is det.
%! es_put0(+PathComps, +Data, +Success, +Failure, -Dict) is det.

es_put0(PathComps, Data, Success, Dict) :-
  es_put0(PathComps, Data, Success, [], Dict).


es_put0(PathComps, Data, Success, Failure, Dict) :-
  es_uri(PathComps, Uri),
  debug_dict(Data),
  call_on_stream(
    uri(Uri),
    json(Data),
    rest_reply(Dict, Success, Failure),
    [method(put),request_header('Accept'='application/json')]
  ).
