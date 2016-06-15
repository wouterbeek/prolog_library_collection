:- module(
  wkt,
  [
    wkt//1 % +Compound
  ]
).

/** <module> Well-Known Text (WKT)

A **linear ring** is a closed line string of at least four positions,
where the first and the last position are the same (both syntactically
and semantically).

A **polygon** consists of exactly one Exterior linear ring and zero or
more interior linear rings (Interiors).

@author Wouter Beek
@version 2016/05-2016/06
*/

:- use_module(library(dcg/dcg_ext)).





circularstring_text(_, Points) -->
  empty_set(Points), !.
circularstring_text(ZM, Points) -->
  "(", seplist(point(ZM), ",", Points), ")".


circularstring_text_representation(ZM1, circularString(Points)) -->
  "CIRCULARSTRING",
  z_m(ZM1, ZM2),
  circularstring_text(ZM2, Points).



collection_text_representation(ZM, Multipoint) -->
  multipoint_text_representation(ZM, Multipoint), !.
collection_text_representation(ZM, Compound) -->
  multicurve_text_representation(ZM, Compound), !.
collection_text_representation(ZM, Compound) -->
  multisurface_text_representation(ZM, Compound), !.
collection_text_representation(ZM, GeometryCollection) -->
  geometrycollection_text_representation(ZM, GeometryCollection).



compoundcurve_text(_, L) -->
  empty_set(L), !.
compoundcurve_text(ZM, L) -->
  "(", seplist(single_curve_text(ZM), ",", L), ")".



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


curve_text_representation(ZM, Compound) -->
  linestring_text_representation(ZM, Compound), !.
curve_text_representation(ZM, CircularString) -->
  circularstring_text_representation(ZM, CircularString), !.
curve_text_representation(ZM, CompoundCurve) -->
  compoundcurve_text_representation(ZM, CompoundCurve).



curvepolygon_text(_, L) -->
  empty_set(L), !.
curvepolygon_text(ZM, L) -->
  "(", seplist(ring_text(ZM), ",", L), ")".


curvepolygon_text_body(ZM, L) -->
  curvepolygon_text(ZM, L).


curvepolygon_text_representation(ZM1, curvePolygon(L)) -->
  "CURVEPOLYGON",
  z_m(ZM1, ZM2),
  curvepolygon_text_body(ZM2, L), !.
curvepolygon_text_representation(ZM, Compound) -->
  polygon_text_representation(ZM, Compound), !.
curvepolygon_text_representation(ZM, Compound) -->
  triangle_text_representation(ZM, Compound).



geometrycollection_text(_, Compounds) -->
  empty_set(Compounds), !.
geometrycollection_text(ZM, Compounds) -->
  seplist(wkt_representation(ZM), ",", Compounds), ")".


geometrycollection_text_representation(ZM1, geometryCollection(Compounds)) -->
  "GEOMETRYCOLLECTION",
  z_m(ZM1, ZM2),
  geometrycollection_text(ZM2, Compounds).



linestring_text(_, Points) -->
  empty_set(Points), !.
linestring_text(ZM, Points) -->
  "(", seplist(point(ZM), ",", Points), ")".


linestring_text_body(ZM, Points) -->
  linestring_text(ZM, Points).


linestring_text_representation(ZM1, lineString(Points)) -->
  "LINESTRING",
  z_m(ZM1, ZM2),
  linestring_text_body(ZM2, Points).



multicurve_text(_, L) -->
  empty_set(L), !.
multicurve_text(ZM, L) -->
  "(", seplist(curve_text(ZM), ",", L), ")".


multicurve_text_representation(ZM1, multiCurve(L)) -->
  "MULTICURVE",
  z_m(ZM1, ZM2),
  multicurve_text(ZM2, L), !.
multicurve_text_representation(ZM, Compound) -->
  multilinestring_text_representation(ZM, Compound).



multilinestring_text(_, Pointss) -->
  empty_set(Pointss), !.
multilinestring_text(ZM, Pointss) -->
  "(", seplist(linestring_text_body(ZM), ",", Pointss), ")".


multilinestring_text_representation(ZM1, multiLineString(Pointss)) -->
  "MULTILINESTRING",
  z_m(ZM1, ZM2),
  multilinestring_text(ZM2, Pointss).



multipoint_text(_, Points) -->
  empty_set(Points), !.
multipoint_text(ZM, Points) -->
  "(", seplist(point_text(ZM), ",", Points), ")".


multipoint_text_representation(ZM1, multiPoint(Points)) -->
  "MULTIPOINT",
  z_m(ZM1, ZM2),
  multipoint_text(ZM2, Points).



multipolygon_text(_, L) -->
  empty_set(L), !.
multipolygon_text(ZM, L) -->
  "(", seplist(polygon_text_body(ZM), ",", L), ")".


multipolygon_text_representation(ZM1, multiPolygon(L)) -->
  "MULTIPOLYGON",
  z_m(ZM1, ZM2),
  multipolygon_text(ZM2, L).



multisurface_text(_, L) -->
  empty_set(L), !.
multisurface_text(ZM, L) -->
  "(", seplist(surface_text(ZM), ",", L), ")".


multisurface_text_representation(ZM1, multiSurface(L)) -->
 "MULTISURFACE",
 z_m(ZM1, ZM2),
 multisurface_text(ZM2, L), !.
multisurface_text_representation(ZM, Compound) -->
  multipolygon_text_representation(ZM, Compound), !.
multisurface_text_representation(ZM, PolyhedralSurface) -->
  polyhedralsurface_text_representation(ZM, PolyhedralSurface), !.
multisurface_text_representation(ZM, Tin) -->
  tin_text_representation(ZM, Tin).



point_text(_, Points) -->
  empty_set(Points), !.
point_text(ZM, [Point]) -->
  "(", point(ZM, Point), ")".


point_text_representation(ZM1, point(Point)) -->
  "POINT",
  z_m(ZM1, ZM2),
  point_text(ZM2, [Point]).



polygon_text(_, Pointss) -->
  empty_set(Pointss), !.
polygon_text(ZM, Pointss) -->
  "(", seplist(linestring_text(ZM), ",", Pointss), ")".


polygon_text_body(ZM, Pointss) -->
  polygon_text(ZM, Pointss).


polygon_text_representation(ZM1, polygon(Pointss)) -->
  "POLYGON",
  z_m(ZM1, ZM2),
  polygon_text_body(ZM2, Pointss).



polyhedralsurface_text(_, Pointsss) -->
  empty_set(Pointsss), !.
polyhedralsurface_text(ZM, Pointsss) -->
  "(", seplist(polygon_text_body(ZM), ",", Pointsss), ")".


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


surface_text_representation(ZM, Compound) -->
  curvepolygon_text_representation(ZM, Compound).



tin_text(_, Pointss) -->
  empty_set(Pointss), !.
tin_text(ZM, Pointss) -->
  "(", seplist(triangle_text_body(ZM), ",", Pointss), ")".


tin_text_representation(ZM1, tin(Pointss)) -->
  "TIN",
  z_m(ZM1, ZM2),
  tin_text(ZM2, Pointss).



triangle_text(_, Points) -->
  empty_set(Points), !.
triangle_text(ZM, Points) -->
  "(", linestring_text(ZM, Points), ")".


triangle_text_body(ZM, Points) -->
  triangle_text(ZM, Points).


triangle_text_representation(ZM1, triangle(Points)) -->
  "TRIANGLE",
  z_m(ZM1, ZM2),
  triangle_text_body(ZM2, Points).



wkt(Compound) -->
  wkt_representation(_, Compound).


wkt_representation(ZM, Point) -->
  point_text_representation(ZM, Point), !.
wkt_representation(ZM, Compound) -->
  curve_text_representation(ZM, Compound), !.
wkt_representation(ZM, Compound) -->
  surface_text_representation(ZM, Compound), !.
wkt_representation(ZM, Compound) -->
  collection_text_representation(ZM, Compound).





% HELPERS %

empty_set([]) --> "EMPTY".


m(N) --> number(N).


%point(zm, [X,Y,Z,M]) --> point(z, [X,Y,Z]), " ", m(M), !.
%point(z, [X,Y,Z]) --> point(none, [X,Y]), " ", z(Z), !.
%point(m, [X,Y,M]) --> point(none, [X,Y]), " ", m(M), !.
point(_, [X,Y]) --> x(X), " ", y(Y).


x(N) --> number(N).


y(N) --> number(N).


z(N) --> number(N).


z_m(ZM, ZM) --> "".
%z_m(_, zm) --> "ZM", +(ws).
%z_m(_, z) --> "Z", +(ws).
%z_m(_, m) --> "M", +(ws).
