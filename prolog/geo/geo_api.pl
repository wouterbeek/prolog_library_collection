:- module(
  geo_api,
  [
    geo_area/2 % +Shape, -Area
  ]
).

/** <module> GEO API

@author Wouter Beek
@version 2017/04
*/

:- use_module(library(apply)).
:- use_module(library(lists)).





geo_area('Line'(_), 0.0) :- !.
geo_area('MultiLine'(_), 0.0) :- !.
geo_area('MultiPoint'(_), 0.0) :- !.
geo_area('MultiPolygon'(Polygons), Area) :- !,
  maplist(polygon_area, Polygons, Areas),
  sum_list(Areas, Area).
geo_area('Point'(_), 0.0) :- !.
geo_area('Polygon'(Coords), Area) :-
  polygon_area(Coords, Area).

polygon_area([Coords], Area) :- !,
  polygon_area(Coords, 0.0, Area).
polygon_area([Coords1,Coords2], Area) :-
  polygon_area(Coords1, Area1),
  polygon_area(Coords2, Area2),
  Area is abs(Area1 - Area2).

polygon_area([[X1,Y1|_],[X2,Y2|T2]|Coords], Sum1, Area) :- !,
  Sum2 is Sum1 + (X1 * Y2) - (Y1 * X2),
  polygon_area([[X2,Y2|T2]|Coords], Sum2, Area).
polygon_area(_, Sum, Area) :-
  Area is abs(Sum / 2.0).
