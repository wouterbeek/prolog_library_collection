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
  "(", +(ws), seplist(point(ZM), +(ws), Points), +(ws), ")".


circularstring_text_representation(ZM1, circularstring(Points)) -->
  "CIRCULARSTRING", +(ws),
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
  "(", +(ws), seplist(single_curve_text(ZM), +(ws), L), +(ws), ")".



compoundcurve_text_representation(ZM1, compoundcurve(L)) -->
  "COMPOUNDCURVE", +(ws),
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
  "(", +(ws), seplist(ring_text(ZM), +(ws), L), +(ws), ")".


curvepolygon_text_body(ZM, L) -->
  curvepolygon_text(ZM, L).


curvepolygon_text_representation(ZM1, curvepolygon(L)) -->
  "CURVEPOLYGON", +(ws),
  z_m(ZM1, ZM2),
  curvepolygon_text_body(ZM2, L), !.
curvepolygon_text_representation(ZM, Compound) -->
  polygon_text_representation(ZM, Compound), !.
curvepolygon_text_representation(ZM, Compound) -->
  triangle_text_representation(ZM, Compound).



geometrycollection_text(_, Compounds) -->
  empty_set(Compounds), !.
geometrycollection_text(ZM, Compounds) -->
  seplist(wkt_representation(ZM), +(ws), Compounds), +(ws), ")".


geometrycollection_text_representation(ZM1, geometrycollection(Compounds)) -->
  "GEOMETRYCOLLECTION", +(ws),
  z_m(ZM1, ZM2),
  geometrycollection_text(ZM2, Compounds).



linestring_text(_, Points) -->
  empty_set(Points), !.
linestring_text(ZM, Points) -->
  "(", +(ws), seplist(point(ZM), +(ws), Points), +(ws), ")".


linestring_text_body(ZM, Points) -->
  linestring_text(ZM, Points).


linestring_text_representation(ZM1, linestring(Points)) -->
  "LINESTRING", +(ws),
  z_m(ZM1, ZM2),
  linestring_text_body(ZM2, Points).



multicurve_text(_, L) -->
  empty_set(L), !.
multicurve_text(ZM, L) -->
  "(", +(ws), seplist(curve_text(ZM), +(ws), L), +(ws), ")".


multicurve_text_representation(ZM1, multicurve(L)) -->
  "MULTICURVE", +(ws),
  z_m(ZM1, ZM2),
  multicurve_text(ZM2, L), !.
multicurve_text_representation(ZM, Compound) -->
  multilinestring_text_representation(ZM, Compound).



multilinestring_text(_, Pointss) -->
  empty_set(Pointss), !.
multilinestring_text(ZM, Pointss) -->
  "(", +(ws), seplist(linestring_text_body(ZM), +(ws), Pointss), +(ws), ")".


multilinestring_text_representation(ZM1, multilinestring(Pointss)) -->
  "MULTILINESTRING", +(ws),
  z_m(ZM1, ZM2),
  multilinestring_text(ZM2, Pointss).



multipoint_text(_, Points) -->
  empty_set(Points), !.
multipoint_text(ZM, Points) -->
  "(", +(ws), seplist(point_text(ZM), +(ws), Points), +(ws), ")".


multipoint_text_representation(ZM1, multipoint(Points)) -->
  "MULTIPOINT", +(ws),
  z_m(ZM1, ZM2),
  multipoint_text(ZM2, Points).



multipolygon_text(_, L) -->
  empty_set(L), !.
multipolygon_text(ZM, L) -->
  "(", +(ws), seplist(polygon_text_body(ZM), +(ws), L), +(ws), ")".


multipolygon_text_representation(ZM1, multipolygon(L)) -->
  "MULTIPOLYGON", +(ws),
  z_m(ZM1, ZM2),
  multipolygon_text(ZM2, L).



multisurface_text(_, L) -->
  empty_set(L), !.
multisurface_text(ZM, L) -->
  "(", +(ws), seplist(surface_text(ZM), +(ws), L), +(ws), ")".


multisurface_text_representation(ZM1, multisurface(L)) -->
 "MULTISURFACE", +(ws),
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
  "(", +(ws), point(ZM, Point), +(ws), ")".


point_text_representation(ZM1, Point) -->
  "POINT", +(ws),
  z_m(ZM1, ZM2),
  point_text(ZM2, [Point]).



polygon_text(_, Pointss) -->
  empty_set(Pointss), !.
polygon_text(ZM, Pointss) -->
  "(", +(ws), seplist(linestring_text(ZM), +(ws), Pointss), +(ws), ")".


polygon_text_body(ZM, Pointss) -->
  polygon_text(ZM, Pointss).


polygon_text_representation(ZM1, polygon(Pointss)) -->
  "POLYGON", +(ws),
  z_m(ZM1, ZM2),
  polygon_text_body(ZM2, Pointss).



polyhedralsurface_text(_, Pointsss) -->
  empty_set(Pointsss), !.
polyhedralsurface_text(ZM, Pointsss) -->
  "(", +(ws), seplist(polygon_text_body(ZM), +(ws), Pointsss), +(ws), ")".


polyhedralsurface_text_representation(ZM1, polyhedralsurface(Pointsss)) -->
  "POLYHEDRALSURFACE", +(ws),
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



surface_text(ZM, curvepolygon(L)) -->
  "CURVEPOLYGON",
  curvepolygon_text_body(ZM, L), !.
surface_text(ZM, Pointss) -->
  polygon_text_body(ZM, Pointss).


surface_text_representation(ZM, Compound) -->
  curvepolygon_text_representation(ZM, Compound).



tin_text(_, Pointss) -->
  empty_set(Pointss), !.
tin_text(ZM, Pointss) -->
  "(", +(ws), seplist(triangle_text_body(ZM), +(ws), Pointss), +(ws), ")".


tin_text_representation(ZM1, tin(Pointss)) -->
  "TIN", +(ws),
  z_m(ZM1, ZM2),
  tin_text(ZM2, Pointss).



triangle_text(_, Points) -->
  empty_set(Points), !.
triangle_text(ZM, Points) -->
  "(", +(ws), linestring_text(ZM, Points), +(ws), ")".


triangle_text_body(ZM, Points) -->
  triangle_text(ZM, Points).


triangle_text_representation(ZM1, triangle(Points)) -->
  "TRIANGLE", +(ws),
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


%point(zm, [X,Y,Z,M]) --> point(z, [X,Y,Z]), +(ws), m(M), !.
%point(z, [X,Y,Z]) --> point(none, [X,Y]), +(ws), z(Z), !.
%point(m, [X,Y,M]) --> point(none, [X,Y]), +(ws), m(M), !.
point(_, point(X,Y)) --> x(X), +(ws), y(Y).


x(N) --> number(N).


y(N) --> number(N).


z(N) --> number(N).


z_m(ZM, ZM) --> "".
z_m(_, zm) --> "ZM", +(ws).
z_m(_, z) --> "Z", +(ws).
z_m(_, m) --> "M", +(ws).
