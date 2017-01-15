:- module(
  wkt_parse,
  [
    wkt//1 % -Shape
  ]
).

/** <module> Well-Known Text (WKT): Parser

A **linear ring** is a closed line string of at least four positions,
where the first and the last position are the same (both syntactically
and semantically).

A **polygon** consists of exactly one Exterior linear ring and zero or
more interior linear rings (Interiors).

@author Wouter Beek
@version 2016/05-2016/06, 2016/11
*/

:- use_module(library(dcg/dcg_ext)).





circularstring_text(_, Points) -->
  empty_set(Points), !.
circularstring_text(ZM, Points) -->
  must_see_code(0'(),
  're+'(point(ZM), Points),
  must_see_code(0')).


circularstring_text_representation(ZM1, circularString(Points)) -->
  keyword("circularstring"),
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
  must_see_code(0'(),
  're+'(single_curve_text(ZM), L),
  must_see_code(0')).



compoundcurve_text_representation(ZM1, compoundCurve(L)) -->
  keyword("compoundcurve"),
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
  must_see_code(0'(),
  're+'(ring_text(ZM), L),
  must_see_code(0')).


curvepolygon_text_body(ZM, L) -->
  curvepolygon_text(ZM, L).


curvepolygon_text_representation(ZM1, curvePolygon(L)) -->
  keyword("curvepolygon"),
  z_m(ZM1, ZM2),
  curvepolygon_text_body(ZM2, L), !.
curvepolygon_text_representation(ZM, Compound) -->
  polygon_text_representation(ZM, Compound), !.
curvepolygon_text_representation(ZM, Compound) -->
  triangle_text_representation(ZM, Compound).



geometrycollection_text(_, Compounds) -->
  empty_set(Compounds), !.
geometrycollection_text(ZM, Compounds) -->
  're+'(wkt_representation(ZM), Compounds),
  must_see_code(0')).


geometrycollection_text_representation(ZM1, geometryCollection(Compounds)) -->
  keyword("geometrycollection"),
  z_m(ZM1, ZM2),
  geometrycollection_text(ZM2, Compounds).



linestring_text(_, Points) -->
  empty_set(Points), !.
linestring_text(ZM, Points) -->
  must_see_code(0'(),
  're+'(point(ZM), Points),
  must_see_code(0')).


linestring_text_body(ZM, Points) -->
  linestring_text(ZM, Points).


linestring_text_representation(ZM1, lineString(Points)) -->
  keyword("linestring"),
  z_m(ZM1, ZM2),
  linestring_text_body(ZM2, Points).



multicurve_text(_, L) -->
  empty_set(L), !.
multicurve_text(ZM, L) -->
  must_see_code(0'(),
  're+'(curve_text(ZM), L),
  must_see_code(0')).


multicurve_text_representation(ZM1, multiCurve(L)) -->
  keyword("multicurve"),
  z_m(ZM1, ZM2),
  multicurve_text(ZM2, L), !.
multicurve_text_representation(ZM, Compound) -->
  multilinestring_text_representation(ZM, Compound).



multilinestring_text(_, Pointss) -->
  empty_set(Pointss), !.
multilinestring_text(ZM, Pointss) -->
  must_see_code(0'(),
  're+'(linestring_text_body(ZM), Pointss),
  must_see_code(0')).


multilinestring_text_representation(ZM1, multiLineString(Pointss)) -->
  keyword("multilinestring"),
  z_m(ZM1, ZM2),
  multilinestring_text(ZM2, Pointss).



multipoint_text(_, Points) -->
  empty_set(Points), !.
multipoint_text(ZM, Points) -->
  must_see_code(0'(),
  're+'(point_text(ZM), Points),
  must_see_code(0')).


multipoint_text_representation(ZM1, multiPoint(Points)) -->
  keyword("multipoint"),
  z_m(ZM1, ZM2),
  multipoint_text(ZM2, Points).



multipolygon_text(_, L) -->
  empty_set(L), !.
multipolygon_text(ZM, L) -->
  must_see_code(0'(),
  're+'(polygon_text_body(ZM), L),
  must_see_code(0')).


multipolygon_text_representation(ZM1, multiPolygon(L)) -->
  keyword("multipolygon"),
  z_m(ZM1, ZM2),
  multipolygon_text(ZM2, L).



multisurface_text(_, L) -->
  empty_set(L), !.
multisurface_text(ZM, L) -->
  must_see_code(0'(),
  're+'(surface_text(ZM), L),
  must_see_code(0')).


multisurface_text_representation(ZM1, multiSurface(L)) -->
  keyword("multisurface"),
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
  must_see_code(0'(),
  point(ZM, Point),
  must_see_code(0')).


point_text_representation(ZM1, point(Point)) -->
  keyword("point"),
  z_m(ZM1, ZM2),
  point_text(ZM2, [Point]).



polygon_text(_, Pointss) -->
  empty_set(Pointss), !.
polygon_text(ZM, Pointss) -->
  must_see_code(0'(),
  're+'(linestring_text(ZM), Pointss),
  must_see_code(0')).


polygon_text_body(ZM, Pointss) -->
  polygon_text(ZM, Pointss).


polygon_text_representation(ZM1, polygon(Pointss)) -->
  keyword("polygon"),
  z_m(ZM1, ZM2),
  polygon_text_body(ZM2, Pointss).


polyhedralsurface_text(_, Pointsss) -->
  empty_set(Pointsss), !.
polyhedralsurface_text(ZM, Pointsss) -->
  must_see_code(0'(),
  're+'(polygon_text_body(ZM), Pointsss),
  must_see_code(0')).


polyhedralsurface_text_representation(ZM1, polyhedralSurface(Pointsss)) -->
  keyword("polyhedralsurface"),
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
  keyword("curvepolygon"),
  curvepolygon_text_body(ZM, L), !.
surface_text(ZM, Pointss) -->
  polygon_text_body(ZM, Pointss).


surface_text_representation(ZM, Compound) -->
  curvepolygon_text_representation(ZM, Compound).



tin_text(_, Pointss) -->
  empty_set(Pointss), !.
tin_text(ZM, Pointss) -->
  must_see_code(0'(),
  're+'(triangle_text_body(ZM), Pointss),
  must_see_code(0')).


tin_text_representation(ZM1, tin(Pointss)) -->
  keyword("tin"),
  z_m(ZM1, ZM2),
  tin_text(ZM2, Pointss).



triangle_text(_, Points) -->
  empty_set(Points), !.
triangle_text(ZM, Points) -->
  must_see_code(0'(),
  linestring_text(ZM, Points),
  must_see_code(0')).


triangle_text_body(ZM, Points) -->
  triangle_text(ZM, Points).


triangle_text_representation(ZM1, triangle(Points)) -->
  keyword("triangle"),
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

empty_set([]) -->
  keyword("empty").


keyword(Str) -->
  {string_upper(Str, Str0)},
  Str0,
  skip_ws.


m(N) --> number(N).


must_see_code(C) -->
  must_see_code(C, skip_ws).


%point(zm, [X,Y,Z,M]) --> point(z, [X,Y,Z]), " ", m(M), !.
%point(z, [X,Y,Z]) --> point(none, [X,Y]), " ", 'Z'(Z), !.
%point(m, [X,Y,M]) --> point(none, [X,Y]), " ", m(M), !.
point(_, [X,Y]) -->
  'X'(X),
  must_see_code(0' ),
  'Y'(Y),
  skip_ws.


:- meta_predicate
    're+'(3, -, ?, ?),
    're*'(3, -, ?, ?).

're+'(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H),
  're*'(Dcg_1, T).

're*'(Dcg_1, [H|T]) -->
  ",", !,
  skip_ws,
  dcg_call(Dcg_1, H),
  're*'(Dcg_1, T).
're*'(_, []) --> "".


skip_ws -->
  'WS', !,
  skip_ws.
skip_ws --> "".


'WS' --> [0x20].
'WS' --> [0x09].
'WS' --> [0x0D].
'WS' --> [0x0A].


'X'(N) --> number(N).


'Y'(N) --> number(N).


'Z'(N) --> number(N).


z_m(ZM, ZM) --> "".
%z_m(_, zm) --> "ZM", +(ws).
%z_m(_, z) --> "Z", +(ws).
%z_m(_, m) --> "M", +(ws).
