:- module(
  es_api,
  [
    es_count/1,       % -Dict
    es_count/2,       % +Segments, -Dict
    es_delete/1,      % +Segments
    es_delete/2,      % +Segments, -Dict
    es_setting/3,     % +Index, +Key, ?Val
    es_update/2,      % +Segments, +Data
    es_update/3       % +Segments, +Data, -Dict
  ]
).

/** <module> Elastic Search API

A typical use of Segments is [<INDEX>,<TYPE>,<DOC>].

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
*/



%! es_count(-Dict) is det.
%! es_count(+Segments, -Dict) is det.
%
% ```bash
% curl -XGET 'http://localhost:9200/_count?pretty' -d '
% {
%   "query": {
%     "match_all": {}
%   }
% }
%
% Succeeds if the HTTP status code is ‘200’; throws an exception
% otherwise.

es_count(Dict) :-
  es_count([], Dict).


es_count(Segments1, Dict) :-
  append(Segments1, ['_count'], Segments2),
  es_get0(Segments2, [200], [], Dict).



%! es_delete(+Segments) is det.
%! es_delete(+Segments, -Dict) is det.
%
% Succeeds if the HTTP status code is ‘200’ or ‘404’.

es_delete(Segments) :-
  es_delete(Segments, Dict),
  if_debug(es_api, print_dict(Dict)).


es_delete(Segments, Dict) :-
  es_uri(Segments, Uri),
  call_on_stream(
    uri(Uri),
    rest_reply(Dict, [200,404], []),
    [method(delete),request_header('Accept'='application/json')]
  ).



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
%
% Succeeds if the HTTP status code is ‘200’ or ‘201 (Created)’; throws
% an exception otherwise.

es_setting(Index, Key, Val) :-
  ground(Val), !,
  dict_pairs(Data, [Key-Val]),
  es_put0([Index,'_settings'], Data, [201], [], Dict),
  Dict.acknowledged == true.
es_setting(Index, Key, Val) :-
  es_get0([Index,'_settings'], Dict),
  get_dict(Key, Dict, Val).



%! es_update(+Segments, +Data) is semidet.
%! es_update(+Segments, +Data, -Dict) is semidet.
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
%
% Succeeds if the HTTP status code is ‘200’; fails if the HTTP status
% code is ‘409 (Conflict)’.

es_update([Index,Type,Id], Data) :-
  es_update([Index,Type,Id], Data, Dict),
  if_debug(es_api, print_dict(Dict)).


es_update([Index,Type,Id], Data, Dict) :-
  es_post0([Index,Type,Id,'_update'], [], Data, [200], [409], Dict).
