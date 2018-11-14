:- encoding(utf8).
:- module(
  elasticsearch,
  [
    es/2,                % +Segments, -Result
    es/3,                % +Segments, -Result, +Options
    es_add_document/3,   % +Segments, +Document, -Status
    es_add_index/2,      % +Index, -Status
    es_count/2,          % +Segments, -Count
    es_delete/2,         % +Segments, -Status
    es_get/2,            % +Segments, -Result
    es_health/0,
    es_health/1,         % -Status
    es_indices/0,
    es_nodes/0,
    es_setting/3,        % +Index, ?Key, ?Value
    es_statistics/2,     % +Segments, -Status
    es_update_document/3 % +Segments, +Document, -Status
  ]
).
:- reexport(library(pp)).

/** <module> ElasticSearch API

@author Wouter Beek
@version 2017/09, 2017/12
*/

:- use_module(library(call_ext)).
:- use_module(library(dict)).
:- use_module(library(error)).
:- use_module(library(http/json)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_client2)).
:- use_module(library(lists)).
:- use_module(library(media_type)).
:- use_module(library(option)).
:- use_module(library(pagination), []).
:- use_module(library(settings)).
:- use_module(library(readutil)).
:- use_module(library(uri_ext)).

:- setting(host, atom, localhost,
           "The host name of the ElasticSearch endpoint.").
:- setting(port, positive_integer, 9200,
           "The port of the ElasticSearch endpoint.").
:- setting(scheme, oneof([http,https]), http,
           "The scheme of the ElasticSearch endpoint.").





%! es(+Segments:list(atom), -Result:dict) is nondet.
%! es(+Segments:list(atom), -Result:dict, +Options:list(compound)) is nondet.
%
% The following options are supported:
%
%   * page_number(+positive_integer)
%
%     The number of the page that is initially retreived.  Default is
%     1.
%
%   * page_size(+positive_integer)
%
%     The number of results per request.  Default is 10.
%

es(Segments, Result) :-
  es(Segments, Result, []).


es(Segments1, Result, Options) :-
  option(page_number(FirstPage), Options, 1),
  option(page_size(PageSize), Options, 10),
  between(FirstPage, inf, Page), % NONDET
  From is (Page - 1) * PageSize,
  Query1 = [from(From),size(PageSize)],
  append(Segments1, ['_search'], Segments2),
  (   option(query_dsl(Search), Options)
  ->  must_be(dict, Search),
      es_request_(
        Segments2,
        Query1,
        [accept(json),post(json(Search))],
        200,
        Status
      )
  ;   (   option(simple_search(Search), Options)
      ->  must_be(ground, Search),
          simple_search_string(Search, String),
          Query2 = [q(String)|Query1]
      ;   Query2 = Query1
      ),
      es_request_(
        Segments2,
        Query2,
        [accept(media(text/plain,[]))],
        200,
        Status
      )
  ),
  Hits = Status.hits.hits,
  % Remove choicepoints when there are no more results.
  length(Hits, NumberOfHits),
  (NumberOfHits < PageSize -> ! ; true),
  member(Hit, Hits),
  _{'_id': Id, '_source': Source} :< Hit,
  dict_tag(Source, Id, Result).

simple_search_string(Term, String) :-
  compound(Term), !,
  Term =.. [Key,Val],
  format(string(String), "~a:~w", [Key,Val]).
simple_search_string(String, String).



%! es_add_document(+Segments:list(atom), +Document:dict,
%!                 -Status:dict) is semidet.
%
% Add a new document.
%
% Succeeds if the HTTP status code is ‘201 (Created)’.  Fails if the
% HTTP status code is ‘409 (Conflict)’.

% POST /<INDEX>/<TYPE>
es_add_document([Index,Type], Document, Status) :- !,
  % The POST method automatically generates an ElasticSearch ID.
  es_request_(
    [Index,Type],
    [],
    [accept(json),post(json(Document))],
    201,
    Status
  ).
% PUT /<INDEX>/<TYPE>/<ID>
es_add_document([Index,Type,Id], Document, Status) :-
  es_request_(
    [Index,Type,Id,'_create'],
    [],
    [accept(json),method(put),post(json(Document))],
    201-409,
    Status
  ).



%! es_add_index(+Index:atom, -Status:dict) is det.

es_add_index(Index, Status) :-
  es_request_(
    [Index],
    [pretty(true)],
    [accept(json),method(put)],
    200-400,
    Status
  ).



%! es_count(+Segments:list(atom), -Status:dict) is det.
%
% ```bash
% curl -XGET 'http://localhost:9200/_count?pretty' -d '
% {
%   "query": {
%     "match_all": {}
%   }
% }
% ```

es_count(Segments1, Status) :-
  append(Segments1, ['_count'], Segments2),
  es_request_(
    Segments2,
    [],
    [accept(json)],
    200-404,
    Status
  ).



%! es_delete(+Segments:list(atom), -Status:dict) is det.

es_delete(Segments, Status) :-
  es_request_(
    Segments,
    [],
    [accept(json),method(delete)],
    200-404,
    Status
  ).



%! es_get(+Segments:list(atom), -Result:dict) is semidet.
%
% Segments must be of the form [Index,Type,Id].
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

es_get(Segments, Result) :-
  assertion(Segments = [_Index,_Type,_Id]),
  es_request_(
    Segments,
    [],
    [accept(json)],
    200-404,
    Status
  ),
  _{'_id': Id, '_source': Source, found: true} :< Status,
  dict_tag(Source, Id, Result).

es_get_key('_id').
es_get_key('_index').
es_get_key('_score').
es_get_key('_source').
es_get_key('_type').



%! es_health is det.

es_health :-
  es_request_(
    ['_cat',health],
    [v(true)],
    [accept(media(text/plain,[]))],
    200-404,
    Codes
  ),
  format("~s\n", [Codes]).


%! es_health(-Dict:dict) is det.

es_health(Dict) :-
  es_request_(
    ['_cluster',health],
    [],
    [accept(media(text/plain,[]))],
    200-404,
    Dict
  ).



%! es_indices is det.

es_indices :-
  es_request_(
    ['_cat',indices],
    [v(true)],
    [accpept(media(text/plain,[]))],
    200-404,
    Codes
  ),
  format("~s\n", [Codes]).



%! es_nodes is det.

es_nodes :-
  es_request_(
    ['_cat',nodes],
    [v(true)],
    [accept(media(text/plain,[]))],
    200-404,
    Codes
  ),
  format("~s\n", [Codes]).



%! es_statistics(+Segments:list(atom), -Status:dict) is det.

es_statistics(Segments1, Status) :-
  append(Segments1, ['_stats'], Segments2),
  es_request_(
    Segments2,
    [],
    [accept(media(text/plain,[]))],
    200-404,
    Status
  ).



%! es_setting(+Index:atom, +Key:atom, +Value:nonneg) is det.
%! es_setting(+Index:atom, +Key:atom, -Value:nonneg) is det.
%! es_setting(+Index:atom, -Key:atom, -Value:nonneg) is det.
%
% Get and set settings.
%
% The following keys are supported:
%
%   * number_of_replicas(?nonneg)
%
%   * number_of_shards(?nonneg)

es_setting(Index, Key, Value) :-
  call_must_be(es_setting_key, Key),
  (   ground(Value)
  ->  dict_pairs(Data, [Key-Value]),
      es_request_(
        [Index,'_settings'],
        [],
        [accept(media(json)),method(put),post(json(Data))],
        201-409,
        Status
      ),
      assertion(Status.acknowledged == true)
  ;   es_request_(
        [Index,'_settings'],
        [],
        [accept(json)],
        200-404,
        Status
      ),
      Value = Status.Key
  ).

es_setting_key(number_of_replicas).
es_setting_key(number_of_shards).



%! es_update_document(+Segments:list(atom), +Document:dict,
%!                    -Status:dict) is det.
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

es_update_document([Index,Type,Id], Document, Status) :-
  es_request_(
    [Index,Type,Id,'_update'],
    [],
    [accept(json),post(json(Document))],
    200-409,
    Status
  ).





% HELPERS %

%! es_request_(+Segments:list(atom), +Query:list(compound),
%!             +Options:list(compound),
%!             +StatusCodes:pair(between(200,299),between(400,499)),
%!             -Result:term) is det.

es_request_(Segments, Query, Options1, Success-Failure, Result) :-
  es_uri(Segments, Query, Uri),
  merge_options(
    [failure(Failure),metadata(Metas),success(Success)],
    Options1,
    Options2
  ),
  http_open2(Uri, In, Options2),
  call_cleanup(
    (
      http_metadata_content_type(Metas, MediaType),
      (   media_type_comps(MediaType, application, json, _)
      ->  json_read_dict(In, Result)
      ;   media_type_comps(MediaType, text, plain, _)
      ->  read_stream_to_codes(In, Result)
      ;   domain_error(media_type, MediaType)
      )
    ),
    close(In)
  ).



%! es_uri(+Segments:list(atom), -Uri:atom) is det.
%! es_uri(+Segments:list(atom), +Query:list(compound), -Uri:atom) is det.

es_uri(Segments, Uri) :-
  es_uri(Segments, [], Uri).


es_uri(Segments, Query, Uri) :-
  setting(scheme, Scheme),
  setting(host, Host),
  setting(port,Port),
  uri_comps(Uri, uri(Scheme,auth(_,_,Host,Port),Segments,Query,_)).



kibana_uri(Uri) :-
  uri_comps(Uri, uri(http,auth(_,_,localhost,5601),[],[],_)).
