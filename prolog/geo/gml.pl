:- module(
  gml,
  [
    gml_shape/2    % +Dom, -Shape
  ]
).

/** <module> GML

@author Wouter Beek
@version 2017/02-2017/04
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).





%! gml_shape(+Dom, -Shape) is det.

gml_shape(element('gml:coordinates',_,[Coords]), Line) :- !,
  atom_phrase(gml_coords(Line), Coords).
gml_shape(element('gml:innerBoundaryIs',_,[Dom]), Line) :- !,
  gml_shape(Dom, Line).
gml_shape(element('gml:LinearRing',_,[Dom]), Line) :- !,
  gml_shape(Dom, Line).
gml_shape(element('gml:MultiPolygon',_,Dom), 'MultiPolygon'(Polygons2)) :- !,
  maplist(gml_shape, Dom, Polygons1),
  maplist(polygon_in_multipolygon, Polygons1, Polygons2).
gml_shape(element('gml:outerBoundaryIs',_, [Dom]), Line) :- !,
  gml_shape(Dom, Line).
gml_shape(element('gml:Polygon',_,Dom), 'Polygon'(Lines)) :- !,
  % Outer boundary and -- possibly -- inner boundary.
  maplist(gml_shape, Dom, Lines).
gml_shape(element('gml:polygonMember',_,[Dom]), Polygon) :-
  gml_shape(Dom, Polygon).

gml_coords([[X,Y]|T]) -->
  number(X), !,
  ",",
  number(Y),
  blanks,
  gml_coords(T).
gml_coords([]) --> "".

polygon_in_multipolygon('Polygon'(Polygon), Polygon).
