:- module(
  gml,
  [
    gml_shape/2 % +Dom, -Shape
  ]
).

/** <module> GML

@author Wouter Beek
@version 2017/02-2017/04
*/

:- use_module(library(apply)).
:- use_module(library(dcg)).





%! gml_shape(+Dom, -Shape) is det.

gml_shape(element('gml:coordinates',_,[Coords]), Line) :- !,
  atom_phrase(gml_coords(Line), Coords).
gml_shape(element('gml:exterior',_,[Dom]), Shape) :- !,
  gml_shape(Dom, Shape).
gml_shape(element('gml:innerBoundaryIs',_,[Dom]), Line) :- !,
  gml_shape(Dom, Line).
gml_shape(element('gml:interior',_,[Dom]), Shape) :- !,
  gml_shape(Dom, Shape).
gml_shape(element('gml:LinearRing',_,[Dom]), Line) :- !,
  gml_shape(Dom, Line).
gml_shape(element('gml:MultiPolygon',_,Dom), 'MultiPolygon'(Polygons2)) :- !,
  maplist(gml_shape, Dom, Polygons1),
  maplist(polygon_in_multipolygon, Polygons1, Polygons2).
gml_shape(element('gml:MultiSurface',_,Dom), 'MultiSurface'(Surfaces2)) :- !,
  maplist(gml_shape, Dom, Surfaces1),
  maplist(polygon_in_multipolygon, Surfaces1, Surfaces2).
gml_shape(element('gml:outerBoundaryIs',_, [Dom]), Line) :- !,
  gml_shape(Dom, Line).
gml_shape(element('gml:Polygon',_,Dom), 'Polygon'(Lines)) :- !,
  % Outer boundary and -- possibly -- inner boundary.
  maplist(gml_shape, Dom, Lines).
gml_shape(element('gml:polygonMember',_,[Dom]), Polygon) :- !,
  gml_shape(Dom, Polygon).
gml_shape(element('gml:posList',_,[Coords]), Line) :- !,
  atom_phrase(gml_poslist(Line), Coords).
gml_shape(element('gml:surfaceMember',_,[Dom]), Surface) :-
  gml_shape(Dom, Surface).

gml_coords([[X,Y]|T]) -->
  number(X), !,
  blanks, ",", blanks,
  number(Y),
  blanks,
  gml_coords(T).
gml_coords([]) --> "".

gml_poslist([[X,Y]|T]) -->
  number(X), !,
  blank, blanks,
  number(Y),
  blanks,
  gml_poslist(T).
gml_poslist([]) --> "".

polygon_in_multipolygon('Polygon'(Polygon), Polygon).
