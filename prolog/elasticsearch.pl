:- module(
  elasticsearch,
  [
    es/2,           % +Segments, -Result
    es/4,           % +Segments, +Search, +Options, -Result
    es_add/3,       % +Segments, +Document, -Status
    es_get/2,       % +Segments, -Result
    es_health/0,
    es_health/1,    % -Status
    es_indices/0,
    es_nodes/0,
    es_statistics/2 % +Segments, -Status
  ]
).
:- reexport(library(pp)).

/** <module> ElasticSearch API

@author Wouter Beek
@version 2017/09
*/

:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/json)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pagination), []).
:- use_module(library(settings)).
:- use_module(library(readutil)).
:- use_module(library(uri/uri_ext)).

:- setting(host, atom, localhost,
           "The host name of the ElasticSearch endpoint.").
:- setting(port, positive_integer, 9200,
           "The port of the ElasticSearch endpoint.").
:- setting(scheme, oneof([http,https]), http,
           "The scheme of the ElasticSearch endpoint.").





%! es(+Segments:list(atom), -Result:dict) is nondet.
%! es(+Segments:list(atom), +Search, +Options, -Result:dict) is nondet.
%
% Result contains pagination keys.

es(Segments, Result) :-
  es(Segments, _VAR, _{}, Result).


es(Segments1, Search, Options1, Result2) :-
  pagination:pagination_options(Options1, FirstPage, PageSize, Options2),
  % NONDET
  between(FirstPage, inf, Page),
  From is (Page - 1) * PageSize,
  Query1 = [from(From),size(PageSize)],
  append(Segments1, ['_search'], Segments2),
  (   % Query DSL
      is_dict(Search)
  ->  es_request_(
        Segments2,
        Query1,
        [post(json(Search)),request_header('Accept'='application/json')],
        200,
        Status
      )
  ;   % Simple Search
      (   var(Search)
      ->  Query2 = Query1
      ;   simple_search_string(Search, String),
          Query2 = [q(String)|Query1]
      ),
      es_request_(
        Segments2,
        Query2,
        [request_header('Accept'='text/plain')],
        200,
        Status
      )
  ),
  Hits = Status.hits,
  maplist(es_hit, Hits.hits, Hits),
  length(Hits, NumberOfHits),
  % Remove choicepoints when there are no more results.
  (NumberOfHits =:= 0 -> !, true ; true),
  Result1 = _{
    number_of_results: NumberOfHits,
    page: Page,
    page_size: PageSize,
    results: Hits,
    total_number_of_results: Hits.total
  },
  merge_dicts(Options2, Result1, Result2).

es_hit(Dict1, Dict3) :-
  _{'_id': Id, '_source': Dict2} :< Dict1,
  dict_tag(Dict2, Id, Dict3).

simple_search_string(Term, String) :-
  compound(Term), !,
  Term =.. [Key,Val],
  format(string(String), "~a:~w", [Key,Val]).
simple_search_string(String, String).



%! es_add(+Segments:list(atom), +Document:dict, -Status:dict) is semidet.
%
% Add a new document.
%
% Succeeds if the HTTP status code is ‘201 (Created)’.  Fails if the
% HTTP status code is ‘409 (Conflict)’.

% POST /<INDEX>/<TYPE>
es_add([Index,Type], Document, Status) :- !,
  % The POST method automatically generates an ElasticSearch ID.
  es_request_(
    [Index,Type],
    [],
    [post(json(Document)),request_header('Accept'='application/json')],
    201,
    Status
  ).
% PUT /<INDEX>/<TYPE>/<ID>
es_add([Index,Type,Id], Document, Status) :-
  es_request_(
    [Index,Type,Id,'_create'],
    [],
    [
      method(put),
      post(json(Document)),
      request_header('Accept'='application/json')
    ],
    201,
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
    [request_header('Accept'='application/json')],
    200,
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
    [request_header('Accept'='text/plain')],
    200,
    Codes
  ),
  format("~s\n", [Codes]).


%! es_health(-Dict:dict) is det.

es_health(Dict) :-
  es_request_(
    ['_cluster',health],
    [],
    [request_header('Accept'='text/plain')],
    200,
    Dict
  ).



%! es_indices is det.

es_indices :-
  es_request_(
    ['_cat',indices],
    [v(true)],
    [request_header('Accept'='text/plain')],
    200,
    Codes
  ),
  format("~s\n", [Codes]).



%! es_nodes is det.

es_nodes :-
  es_request_(
    ['_cat',nodes],
    [v(true)],
    [request_header('Accept'='text/plain')],
    200,
    Codes
  ),
  format("~s\n", [Codes]).



%! es_statistics(+Segments:list(atom), -Status:dict) is det.

es_statistics(Segments1, Status) :-
  append(Segments1, ['_stats'], Segments2),
  es_request_(
    Segments2,
    [],
    [request_header('Accept'='text/plain')],
    200,
    Status
  ).





% HELPERS %

%! es_request_(+Segments:list(atom), +Query:list(compound),
%!             +Options:list(compound), +Success:between(200,299),
%!             -Result:term) is det.

es_request_(Segments, Query, Options1, Success, Result) :-
  es_uri(Segments, Query, Uri),
  merge_options(
    [header(content_type,ContentType),status_code(Status)],
    Options1,
    Options2
  ),
  setup_call_cleanup(
    http_open(Uri, In, Options2),
    (
      must_be(oneof([Success]), Status),
      http_parse_header_value(content_type, ContentType, media(MediaType,_)),
      (   MediaType = application/json
      ->  json_read_dict(In, Result)
      ;   MediaType = text/plain
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
