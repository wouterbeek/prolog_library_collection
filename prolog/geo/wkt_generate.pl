:- module(
  wkt_gen,
  [
    wkt_gen//1 % +Shape
  ]
).

/** <module> Well-Known Text (WKT): Generator

@version 2016/11
*/

:- use_module(library(dcg/dcg_ext)).





circularstring_text(_, []) --> !,
  "EMPTY".
circularstring_text(ZM, Points) -->
  "(",
  're+'(point(ZM), Points),
  ")".


circularstring_text_representation(ZM1, circularString(Points)) -->
  "CIRCULARSTRING",
  z_m(ZM1, ZM2),
  circularstring_text(ZM2, Points).



collection_text_representation(ZM, Multipoint) -->
  multipoint_text_representation(ZM, Multipoint), !.
collection_text_representation(ZM, Shape) -->
  multicurve_text_representation(ZM, Shape), !.
collection_text_representation(ZM, Shape) -->
  multisurface_text_representation(ZM, Shape), !.
collection_text_representation(ZM, GeometryCollection) -->
  geometrycollection_text_representation(ZM, GeometryCollection).



compoundcurve_text(_, []) --> !,
  "EMPTY".
compoundcurve_text(ZM, L) -->
  "(",
  're+'(single_curve_text(ZM), L),
  ")".



compoundcurve_text_representation(ZM1, compoundCurve(L)) -->
  "COMPOUNDCURVE",
  z_m(ZM1, ZM2),
  compoundcurve_text(ZM2, L).



curve_text(ZM, Points) -->
  linestring_text_body(ZM, Points), !.
curve_text(ZM, CircularString) -->
  circularstring_text_representation(ZM, CircularString), !.
curve_text(ZM, CompoundCurve) -->
  compoundcurve_text_representation(ZM, CompoundCurve).


curve_text_representation(ZM, Shape) -->
  linestring_text_representation(ZM, Shape), !.
curve_text_representation(ZM, CircularString) -->
  circularstring_text_representation(ZM, CircularString), !.
curve_text_representation(ZM, CompoundCurve) -->
  compoundcurve_text_representation(ZM, CompoundCurve).



curvepolygon_text(_, []) --> !,
  "EMPTY".
curvepolygon_text(ZM, L) -->
  "(",
  're+'(ring_text(ZM), L),
  ")".


curvepolygon_text_body(ZM, L) -->
  curvepolygon_text(ZM, L).


curvepolygon_text_representation(ZM1, curvePolygon(L)) -->
  "CURVEPOLYGON",
  z_m(ZM1, ZM2),
  curvepolygon_text_body(ZM2, L), !.
curvepolygon_text_representation(ZM, Shape) -->
  polygon_text_representation(ZM, Shape), !.
curvepolygon_text_representation(ZM, Shape) -->
  triangle_text_representation(ZM, Shape).



geometrycollection_text(_, []) --> !,
  "EMPTY".
geometrycollection_text(ZM, Shapes) -->
  "(",
  're+'(wkt_representation(ZM), Shapes),
  ")".


geometrycollection_text_representation(ZM1, geometryCollection(Shapes)) -->
  "GEOMETRYCOLLECTION",
  z_m(ZM1, ZM2),
  geometrycollection_text(ZM2, Shapes).



linestring_text(_, []) --> !,
  "EMPTY".
linestring_text(ZM, Points) -->
  "(",
  're+'(point(ZM), Points),
  ")".


linestring_text_body(ZM, Points) -->
  linestring_text(ZM, Points).


linestring_text_representation(ZM1, lineString(Points)) -->
  "LINESTRING",
  z_m(ZM1, ZM2),
  linestring_text_body(ZM2, Points).



multicurve_text(_, []) --> !,
  "EMPTY".
multicurve_text(ZM, L) -->
  "(",
  're+'(curve_text(ZM), L),
  ")".


multicurve_text_representation(ZM1, multiCurve(L)) -->
  "MULTICURVE",
  z_m(ZM1, ZM2),
  multicurve_text(ZM2, L), !.
multicurve_text_representation(ZM, Shape) -->
  multilinestring_text_representation(ZM, Shape).



multilinestring_text(_, []) --> !,
  "EMPTY".
multilinestring_text(ZM, Pointss) -->
  "(",
  're+'(linestring_text_body(ZM), Pointss),
  ")".


multilinestring_text_representation(ZM1, multiLineString(Pointss)) -->
  "MULTILINESTRING",
  z_m(ZM1, ZM2),
  multilinestring_text(ZM2, Pointss).



multipoint_text(_, []) --> !,
  "EMPTY".
multipoint_text(ZM, Points) -->
  "(",
  're+'(point_text(ZM), Points),
  ")".


multipoint_text_representation(ZM1, multiPoint(Points)) -->
  "MULTIPOINT",
  z_m(ZM1, ZM2),
  multipoint_text(ZM2, Points).



multipolygon_text(_, []) --> !,
  "EMPTY".
multipolygon_text(ZM, L) -->
  "(",
  're+'(polygon_text_body(ZM), L),
  ")".


multipolygon_text_representation(ZM1, multiPolygon(L)) -->
  "MULTIPOLYGON",
  z_m(ZM1, ZM2),
  multipolygon_text(ZM2, L).



multisurface_text(_, []) --> !,
  "EMPTY".
multisurface_text(ZM, L) -->
  "(",
  're+'(surface_text(ZM), L),
  ")".


multisurface_text_representation(ZM1, multiSurface(L)) -->
  "MULTISURFACE",
  z_m(ZM1, ZM2),
  multisurface_text(ZM2, L), !.
multisurface_text_representation(ZM, Shape) -->
  multipolygon_text_representation(ZM, Shape), !.
multisurface_text_representation(ZM, PolyhedralSurface) -->
  polyhedralsurface_text_representation(ZM, PolyhedralSurface), !.
multisurface_text_representation(ZM, Tin) -->
  tin_text_representation(ZM, Tin).



point_text(_, []) --> !,
  "EMPTY".
point_text(ZM, [Point]) -->
  "(",
  point(ZM, Point),
  ")".


point_text_representation(ZM1, point(Point)) -->
  "POINT",
  z_m(ZM1, ZM2),
  point_text(ZM2, [Point]).



polygon_text(_, []) --> !,
  "EMPTY".
polygon_text(ZM, Pointss) -->
  "(",
  're+'(linestring_text(ZM), Pointss),
  ")".


polygon_text_body(ZM, Pointss) -->
  polygon_text(ZM, Pointss).


polygon_text_representation(ZM1, polygon(Pointss)) -->
  "POLYGON",
  z_m(ZM1, ZM2),
  polygon_text_body(ZM2, Pointss).


polyhedralsurface_text(_, []) --> !,
  "EMPTY".
polyhedralsurface_text(ZM, Pointsss) -->
  "(",
  're+'(polygon_text_body(ZM), Pointsss),
  ")".


polyhedralsurface_text_representation(ZM1, polyhedralSurface(Pointsss)) -->
  "POLYHEDRALSURFACE",
  z_m(ZM1, ZM2),
  polyhedralsurface_text(ZM2, Pointsss).



ring_text(ZM, Points) -->
  linestring_text_body(ZM, Points), !.
ring_text(ZM, CircularString) -->
  circularstring_text_representation(ZM, CircularString), !.
ring_text(ZM, CompoundCurve) -->
  compoundcurve_text_representation(ZM, CompoundCurve).



single_curve_text(ZM, Points) -->
  linestring_text_body(ZM, Points), !.
single_curve_text(ZM, CircularString) -->
  circularstring_text_representation(ZM, CircularString).



surface_text(ZM, curvePolygon(L)) -->
  "CURVEPOLYGON",
  curvepolygon_text_body(ZM, L), !.
surface_text(ZM, Pointss) -->
  polygon_text_body(ZM, Pointss).


surface_text_representation(ZM, Shape) -->
  curvepolygon_text_representation(ZM, Shape).



tin_text(_, []) --> !,
  "EMPTY".
tin_text(ZM, Pointss) -->
  "(",
  're+'(triangle_text_body(ZM), Pointss),
  ")".


tin_text_representation(ZM1, tin(Pointss)) -->
  "TIN",
  z_m(ZM1, ZM2),
  tin_text(ZM2, Pointss).



triangle_text(_, []) --> !,
  "EMPTY".
triangle_text(ZM, Points) -->
  "(",
  linestring_text(ZM, Points),
  ")".


triangle_text_body(ZM, Points) -->
  triangle_text(ZM, Points).


triangle_text_representation(ZM1, triangle(Points)) -->
  "TRIANGLE",
  z_m(ZM1, ZM2),
  triangle_text_body(ZM2, Points).



wkt_gen(Shape) -->
  wkt_representation(_, Shape).


wkt_representation(ZM, Point) -->
  point_text_representation(ZM, Point), !.
wkt_representation(ZM, Shape) -->
  curve_text_representation(ZM, Shape), !.
wkt_representation(ZM, Shape) -->
  surface_text_representation(ZM, Shape), !.
wkt_representation(ZM, Shape) -->
  collection_text_representation(ZM, Shape).





% HELPERS %

m(N) --> number(N).


%point(zm, [X,Y,Z,M]) --> point(z, [X,Y,Z]), " ", m(M), !.
%point(z, [X,Y,Z]) --> point(none, [X,Y]), " ", 'Z'(Z), !.
%point(m, [X,Y,M]) --> point(none, [X,Y]), " ", m(M), !.
point(_, [X,Y]) -->
  'X'(X),
  " ",
  'Y'(Y).


:- meta_predicate
    're+'(3, -, ?, ?),
    're*'(3, -, ?, ?).

're+'(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H),
  're*'(Dcg_1, T).

're*'(Dcg_1, [H|T]) -->
  ", ", !,
  dcg_call(Dcg_1, H),
  're*'(Dcg_1, T).
're*'(_, []) --> "".


'X'(N) --> number(N).


'Y'(N) --> number(N).


'Z'(N) --> number(N).


z_m(ZM, ZM) --> "".
%z_m(_, zm) --> "ZM", +(ws).
%z_m(_, z) --> "Z", +(ws).
%z_m(_, m) --> "M", +(ws).
