:- module(
  geonames,
  [
    query_geonames/4 % +Service:atom
                     % +Format:atom
                     % +Query:atom
                     % +Results:atom
  ]
).

/** <module> GeoNames

GeoNames predicates.

@author Wouter Beek
@version 2013/01, 2013/06
*/

:- use_module(generics(file_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(http/http_open)).



% ATTRIBUTES %

attribute(lang, atom).
attribute(lat, number).
attribute(lng, number).
attribute(q, atom).
% Numbers represent kilometers.
attribute(radius, number).
attribute(type, oneof([csv,json,kml,rdf,rss,txt,xml])).
attribute(username, atom).


% QUERY FORMULATION AND PARSING %

parse_attribute(AttributeNames, Attribute):-
  Attribute =.. [Name, Value],
  memberchk(Name, AttributeNames),
  attribute(Name, Type),
  typecheck(Type, Value).

parse_attributes(AttributeNames, Attributes):-
  maplist(parse_attribute(AttributeNames), Attributes).



% QUERY EXECUTION %

query_geonames(Service, Format, Attributes, Result):-
  % Check whether the service with the given name exists.
  service(Service, Formats, AttributeNames),

  % Check whether the given format is supported fot this service.
  memberchk(Format, Formats),

  % Use the list of attribute names to check whether an attribute is defined
  % for the specific service that is used.
  % Add the type attribute to specifiy the format in which the result should
  % be formatted.
  parse_attributes(AttributeNames, Attributes),

  geonames_protocol(Protocol),
  host(Host),
  port(Port),
  service_path(Service, Path),
  user_name(UserName),
  http_open(
    [
      % Note that http_open/3 and parse_url/2 use different attribute names
      % for the same thing, namely scheme/1 and protocol/1 respectively.
      scheme(Protocol),
      host(Host),
      port(Port),
      path(Path),
      search([type(Format), username(UserName) | Attributes])
    ],
    Stream,
    []
  ),
  stream_to_atom(Stream, Result).



% SETTINGS %

host('api.geonames.org').

geonames_protocol(http).

port(80).

%! service(
%!   ?Service:atom,
%!   -Format:oneof([csv,json,kml,rdf,rss,txt,xml])
%! ) is nondet.
%
% @arg Name The atomic name of a GeoNames Web service.
% @arg Format The atomic name of the data exchange format that
%        is supported for the Web serive. One of the following:
%        1. =csv=
%        2. =json=
%        3. =kml=
%        4. =rdf=
%        5. =rss=
%        6. =txt=
%        7. =xml=
% @arg Attributes A list of atomic names of attributes defined for a
%        specific Web service.

service(Service, Formats, Attributes):-
  service0(Service, Formats, Attributes).

service0(astergdem, [json,txt,xml], []).
service0(children, [json,xml], []).
service0(cities, [json,xml], []).
service0(countryCode, [json,txt,xml], [lang,lat,lng,radius,type]).
service0(countryInfo, [csv,json,xml], []).
service0(countrySubdivision, [json,xml], []).
service0(earthquakes, [json,xml], []).
service0(extendedFindNearby, [xml], []).
service0(findNearby, [json,xml], []).
service0(findNearbyPlaceName, [json,xml], []).
service0(findNearbyPostalCodes, [json,xml], []).
service0(findNearbyStreets, [json,xml], []).
service0(findNearbyStreetsOSM, [json,xml], []).
service0(findNearByWeather, [json,xml], []).
service0(findNearbyWikipedia, [json,rss,xml], []).
service0(findNearestAddress, [json,xml], []).
service0(findNearestIntersection, [json,xml], []).
service0(findNearestIntersectionOSM, [json,xml], []).
service0(findNearbyPOIsOSM, [json,xml], []).
service0(get, [json,xml], []).
service0(gtopo30, [json,txt,xml], []).
service0(hierarchy, [json,xml], []).
service0(neighbourhood, [json,xml], []).
service0(neighbours, [json,xml], []).
service0(ocean, [json,xml], []).
service0(postalCodeCountryInfo, [json,xml], []).
service0(postalCodeLookup, [json], []).
service0(postalCodeSearch, [json,xml], []).
service0(rssToGeo, [kml,rss], []).
service0(search, [json,rdf,xml], [q]).
service0(siblings, [json,xml], []).
service0(srtm3, [json,txt,xml], []).
service0(timezone, [json,xml], []).
service0(weather, [json,xml], []).
service0(weatherIcao, [json,xml], []).
service0(wikipediaBoundingBox, [json,xml], []).
service0(wikipediaSearch, [json,xml], []).

% The services are (almost) the paths.
service_path(Service, Path):-
  service(Service, _Formats, _Attributes),
  format(atom(Path), '/~w', [Service]).

user_name(wouterbeek).

