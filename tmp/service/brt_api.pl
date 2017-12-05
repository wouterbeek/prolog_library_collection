:- module(
  brt_api,
  [
    brt_building/1, %                            -Buiding
    brt_building/2, % +UriQuery,                 -Buiding
    brt_building/3  % +Id:atom,       +UriQuery, -Buiding
                    % +GeoQuery:dict, +UriQuery, -Buiding
  ]
).
:- reexport(library(print_ext)).

/** <module> BRT API

@author Wouter Beek
@version 2017/02
*/

:- use_module(library(http/rest)).
:- use_module(library(lists)).
:- use_module(library(pagination_client)).
:- use_module(library(settings)).
:- use_module(library(uri/uri_ext)).

:- meta_predicate
    brt_request(+, 3),
    brt_request(+, 3, +).

:- setting(
     host,
     atom,
     'brt.basisregistraties.overheid.nl',
     "The host of the BRT API."
   ).
:- setting(
     scheme,
     oneof([http,https]),
     https,
     "The URI scheme of the BRT API."
   ).





%! brt_building(                           -Building) is nondet.
%! brt_building(+UriQuery,                 -Building) is nondet.
%! brt_building(+Id:atom, +UriQuery,       -Building) is det.
%! brt_building(+GeoQuery:dict, +UriQuery, -Building) is det.
%
% @arg Id
%
% @arg GeoQuery GeoJSON object supporting ‘within’, ‘contains’,
%      ‘disjoint’ and ‘intersects’ in combination with ‘point’,
%      ‘polygon’ or ‘bbox’ coordinates.
%
% @arg UriQuery The following options are supported:
%
%   * brontype(+string)
%
%     Waardelijst voor het type van de gebruikte brongegevens.
%
%   * identificatie(+string)
%
%     Unieke identificatie van het object binnen het domein van NEN
%     3610.
%
%   * page(+positive_integer)
%
%     Default is 1.
%
%   * type(+string)
%
%     De klasse waartoe het object behoort.

brt_building(Building) :-
  brt_building([], Building).


brt_building(UriQuery, Building) :-
  brt_uri([gebouw], UriQuery, Uri),
  brt_request(Uri, rest_reply(Building, [200], [])).


brt_building(Id, UriQuery, Building) :-
  atom(Id), !,
  brt_uri([gebouw,Id], UriQuery, Uri),
  brt_request(Uri, rest_reply(Building, [200], [404])).
brt_building(GeoQuery, UriQuery, Building) :-
  brt_uri([gebouw], UriQuery, Uri),
  brt_request(
    Uri,
    rest_reply(Building, [200], []),
    [method(put),post(json(GeoQuery))]
  ).



/*
%! brt_registratief_gebied(-Registratiefgebied) is nondet.
%! brt_registratief_gebied(+UriQuery, -Registratiefgebied) is nondet.
%! brt_registratief_gebied(+Id:atom, +UriQuery, -RegistratiefGebied) is det.
%! brt_registratief_gebied(+GeoQuery:dict, +UriQuery, -RegistratiefGebied) is det.
%
% @arg Id
%
% @arg Query GeoJSON object supporting ‘within’, ‘contains’,
%      ‘disjoint’ and ‘intersects’ in combination with ‘point’,
%      ‘polygon’ or ‘bbox’ coordinates.
%
% @arg Opts The following options are supported:
%
%   * brontype(+string)
%
%     Waardelijst voor het type van de gebruikte brongegevens.
%
%   * identificatie(+string)
%
%     Unieke identificatie van het object binnen het domein van NEN
%     3610.
%
%   * naamFries(+string)
%
%     De Friese naam.
%
%   * naamNL(+string)
%
%     De Nederlandse naam.
%
%   * naamOfficieel(+string)
%
%     De officiële naam.
%
%   * page(+positive_integer)
%
%     Default is 1.
%
%   * type(+string)
%
%     De klasse waartoe het object behoort.

brt_registratief_gebied(RegistratiefGebied) :-
  brt_registratief_gebied(RegistratiefGebied, []).


brt_registratief_gebied(RegistratiefGebied, Opts) :-
  brt_uri(['registratief-gebied'], Opts, Uri),
  brt_request(Uri, rest_reply(RegistratiefGebied, [200], [])).


brt_registratief_gebied(Id, RegistratiefGebied, Opts) :-
  atom(Id), !,
  brt_uri(['registratief-gebied',Id], Opts, Uri),
  brt_request(Uri, rest_reply(RegistratiefGebied, [200], [404])).
brt_registratief_gebied(Query, RegistratiefGebied, Opts) :-
  brt_uri(['registratief-gebied'], Opts, Uri),
  brt_request(
    Uri,
    rest_reply(RegistratiefGebied, [200], []),
    [method(put),post(json(Query))]
  ).
*/





% HELPERS %

%! brt_request(+Uri, :Goal_3) is det.
%! brt_request(+Uri, :Goal_3, +Opts) is det.

brt_request(Uri, Goal_3) :-
  brt_request(Uri, Goal_3, []).


brt_request(Uri, Goal_3, Opts1) :-
  merge_options(Opts1, [accept(json),method(get),verbose(all)], Opts2),
  client_pagination(Uri, Goal_3, Opts2).



%! brt_uri(-Uri) is det.

brt_uri(Uri) :-
  brt_uri([], Uri).


brt_uri(Segments1, Uri) :-
  brt_uri(Segments1, _, Uri).


brt_uri(Segments1, Query, Uri) :-
  setting(scheme, Scheme),
  setting(host, Host),
  append([api,v1], Segments1, Segments2),
  uri_comps(Uri, uri(Scheme,Host,Segments2,Query,_)).
