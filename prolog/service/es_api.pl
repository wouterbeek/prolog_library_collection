:- module(
  es_api,
  [
    es_change/3,    % +PathComps, +Data,             -Dict
    es_change/4,    % +PathComps, +Data,   +Version, -Dict
    es_change_pp/2, % +PathComps, +Data
    es_change_pp/3, % +PathComps, +Data,   +Version
    es_check/1,     %                                -Dict
    es_check_pp/0,
    es_count/1,     % +PathComps
    es_count/2,     % +PathComps,                    -Dict
    es_count_pp/0,
    es_count_pp/1,  % +PathComps
    es_create/3,    % +PathComps, +Data,             -Dict
    es_create_pp/2, % +PathComps, +Data
    es_exists/1,    % +PathComps
    es_get/2,       % +PathComps,                    -Dict
    es_get/3,       % +PathComps, +Keys,             -Dict
    es_get_pp/1,    % +PathComps
    es_get_pp/2,    % +PathComps, +Keys
    es_health/1,    %                                -Dict
    es_health_pp/0,
    es_rm/1,        % +PathComps
    es_search/3,    % +PathComps, +Search,           -Dict
    es_search_pp/2, % +PathComps, +Search
    es_update/3,    % +PathComps, +Data,             -Dict
    es_update_pp/2  % +PathComps, +Data
  ]
).

/** <module> Elastic Search API

A typical use of PathComps is [<INDEX>,<TYPE>,<DOC>].

# Settings

```swi
_{
  settings : _{
    number_of_shards : 3,
    number_of_replicas : 1
  }
}
```

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
    fields: {
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
    filtered: _{
      filter: {
        range: {
          <KEY>: {
            gt: <NONNEG>
          }
        }
      },
      query: {
        match_phrase: {
          <KEY>: "<X> <Y>"
        }
      }
    }
  }
}
```

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
:- use_module(library(true)).
:- use_module(library(uri)).
:- use_module(library(yall)).

:- debug(http(error)).
:- debug(http(receive_reply)).
:- debug(http(send_request)).

:- setting(endpoint_host, atom, localhost, "").
:- setting(endpoint_port, positive_integer, 9200, "").
:- setting(endpoint_scheme, oneof([http,https]), http, "").





%! es_change(+PathComps, +Data, -Dict) is det.
%! es_change(+PathComps, +Data, +Version, -Dict) is det.
%! es_change_pp(+PathComps, +Data) is det.
%! es_change_pp(+PathComps, +Data, +Version) is det.
%
% Dict contains the following keys:
%
%   - '_id'(-atom)
%
%   - '_index'(-atom)
%
%   - '_type'(-atom)
%
%   - '_version'(-positive_integer)
%
%   - created(-boolean)
%
% Create a new document or change an existing document.

es_change([Index,Type,Id], Data, Dict):-
  es_put0([Index,Type,Id], Data, Status, Dict),
  http_status_must_be(Status, 200).


es_change([Index,Type,Id], Data, Version, Dict):-
  es_put0([Index,Type,Id], Data, [version(Version)], Status, Dict),
  http_status_must_be(Status, 200).


es_change_pp(PathComps, Data):-
  es_change(PathComps, Data, Dict),
  print_dict(Dict).


es_change_pp(PathComps, Data, Version):-
  es_change(PathComps, Data, Version, Dict),
  print_dict(Dict).



%! es_check(-Dict) is det.
%! es_check_pp is det.

es_check(Dict) :-
  es_get0([], Status, Dict),
  http_status_must_be(Status, 200).


es_check_pp :-
  es_check(Dict),
  print_dict(Dict).



%! es_count(+PathComps) is det.
%! es_count(+PathComps, -Dict) is det.
%! es_count_pp is det.
%! es_count_pp(+PathComps) is det.
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
  http_status_must_be(Status, 200).
  

es_count_pp :-
  es_count(Dict),
  print_dict(Dict).


es_count_pp(PathComps) :-
  es_count(PathComps, Dict),
  print_dict(Dict).



%! es_create(+PathComps, +Data, -Dict) is det.
%! es_create_pp(+PathComps, +Data) is det.
%
% Create a new document.

es_create([Index,Type], Data, Dict) :- !,
  es_post0([Index,Type], Data, Status, Dict),
  http_status_must_be(Status, 201).
es_create([Index,Type,Id], Data, Dict) :-
  es_put0([Index,Type,Id,'_create'], Data, Status, Dict),
  http_status_must_be(Status, 201).


es_create_pp(PathComps, Data) :-
  es_create(PathComps, Data, Dict),
  print_dict(Dict).



%! es_exists(+PathComps) is semidet.

es_exists(PathComps) :-
  es_head0(PathComps, Status),
  http_status_must_be(Status, 200).



%! es_get(+PathComps, -Dict) is det.
%! es_get(+PathComps, +Keys, -Dict) is det.
%! es_get_pp(+PathComps) is det.
%! es_get_pp(+PathComps, +Keys) is det.
%
% Dict contains the following keys:
%
%   - '_id'(-atom)
%
%   - '_index'(-atom)
%
%   - '_source'(-dict)
%
%   - '_type'(-atom)
%
%   - '_version'(-positive_integer)
%
%   - found(-boolean)
%
% Keys, if present, filters the keys returned in '_source'.

es_get(PathComps, Dict) :-
  es_get0(PathComps, Status, Dict),
  http_status_must_be(Status, 200).


es_get(PathComps, Keys, Dict) :-
  atomic_list_concat(Keys, ',', Search),
  es_get0(PathComps, ['_source'(Search)], Status, Dict),
  http_status_must_be(Status, 200).


es_get_pp(PathComps) :-
  es_get(PathComps, Dict),
  print_dict(Dict).


es_get_pp(PathComps, Keys) :-
  es_get(PathComps, Keys, Dict),
  print_dict(Dict).



%! es_health(-Dict) is det.
%! es_health_pp is det.

es_health(Dict) :-
  es_get0(['_cluster',health], Status, Dict),
  http_status_must_be(Status, 200).


es_health_pp :-
  es_health(Dict),
  print_dict(Dict).



%! es_rm(+PathComps) is det.

es_rm(PathComps) :-
  es_delete0(PathComps, Status),
  http_status_must_be(Status, 200).



%! es_search(+PathComps, +Search, -Dict) is nondet.
%! es_search_pp(+PathComps, +Search) is nondet.

es_search(PathComps1, Search, Dict) :-
  append(PathComps1, ['_search'], PathComps2),
  (   % Query DSL
      is_dict(Search)
  ->  es_post0(PathComps2, Search, Status, Dict)
  ;   % Simple Search
      format_simple_search_string(Search, Str),
      es_get0(PathComps2, [q(Str)], Status, Dict)
  ),
  http_status_must_be(Status, 200).


format_simple_search_string(Comp, Str) :-
  compound(Comp), !,
  Comp =.. [Key,Val],
  format(string(Str), "~a:~s", [Key,Val]).
format_simple_search_string(Str, Str).


es_search_pp(PathComps, Search) :-
  es_search(PathComps, Search, Dict),
  print_dict(Dict).



%! es_update(+PathComps, +Data, -Dict) is det.
%! es_update_pp(+PathComps, +Data) is det.
%
% Data can be:
%
%  - _{doc: <DICT>}
%
%  - _{script: 'ctx._source.<KEY>+=<INT>'}
%
%  - _{
%      script: 'ctx._source.<KEY>+=new_tag',
%      params: _{new_tag : <VAL>}
%    }
%
%  - _{upsert: <DICT>, ...}

es_update([Index,Type,Id], Data, Dict) :-
  es_post0([Index,Type,Id,'_update'], Data, Status, Dict),
  http_status_must_be(Status, 200).


es_update_pp([Index,Type,Id], Data) :-
  es_update([Index,Type,Id], Data, Dict),
  print_dict(Dict).





% HELPERS %

%! es_delete0(+PathComps, -Status) is det.

es_delete0(PathComps, Status) :-
  es_iri0(PathComps, Iri),
  http_delete(Iri, true, [metadata([H|T])]),
  maplist(print_dict, [H|T]),
  Status = H.status.



%! es_get0(+PathComps, -Status, -Dict) is det.
%! es_get0(+PathComps, +QueryComps, -Status, -Dict) is det.

es_get0(PathComps, Status, Dict) :-
  es_get0(PathComps, [], Status, Dict).


es_get0(PathComps, QueryComps, Status, Dict) :-
  es_iri0(PathComps, QueryComps, Iri),
  call_or_fail(
    http_get(
      Iri,
      {Dict}/[In,M,M]>>json_read_dict(In, Dict),
      [metadata([H|T])]
    )
  ),
  maplist(print_dict, [H|T]),
  Status = H.status.



%! es_head0(+PathComps, -Status) is semidet.

es_head0(PathComps, Status) :-
  es_iri0(PathComps, Iri),
  call_or_fail(http_head(Iri, [metadata([H|T])])),
  maplist(print_dict, [H|T]),
  Status = H.status.



%! es_iri0(+PathComps, -Iri) is det.
%! es_iri0(+PathComps, +QueryComps, -Iri) is det.

es_iri0(PathComps, Iri) :-
  es_iri0(PathComps, [], Iri).


es_iri0(PathComps, QueryComps, Iri) :-
  setting(endpoint_scheme, Scheme),
  setting(endpoint_host, Host),
  setting(endpoint_port,Port),
  uri_authority_components(Auth, uri_authority(_,_,Host,Port)),
  atomic_list_concat([''|PathComps], /, Path),
  uri_query_components(Query, QueryComps),
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)).



%! es_post0(+PathComps, +Data, -Status, -Dict) is det.

es_post0(PathComps, Data, Status, Dict) :-
  es_iri0(PathComps, Iri),
  call_or_fail(
    http_post(
      Iri,
      json(Data),
      {Dict}/[In,M,M]>>json_read_dict(In, Dict),
      [metadata([H|T]),request_header('Accept'='application/json')]
    )
  ),
  maplist(print_dict, [H|T]),
  Status = H.status.



%! es_put0(+PathComps, +Data, -Status, -Dict) is det.
%! es_put0(+PathComps, +Data, +QueryComps, -Status, -Dict) is det.

es_put0(PathComps, Data, Status, Dict) :-
  es_put0(PathComps, Data, [], Status, Dict).


es_put0(PathComps, Data, QueryComps, Status, Dict) :-
  es_iri0(PathComps, QueryComps, Iri),
  call_or_fail(
    http_put(
      Iri,
      json(Data),
      {Dict}/[In,M,M]>>json_read_dict(In, Dict),
      [metadata([H|T]),request_header('Accept'='application/json')]
    )
  ),
  maplist(print_dict, [H|T]),
  Status = H.status.
