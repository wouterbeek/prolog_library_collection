:- module(
  wkt_parse,
  [
    wkt//2 % -Crs:atom, -Shape:compound
  ]
).

/** <module> Well-Known Text (WKT): Parser

A **linear ring** is a closed line string of at least four positions,
where the first and the last position are the same (both syntactically
and semantically).

A **polygon** consists of exactly one Exterior linear ring and zero or
more interior linear rings (Interiors).

@author Wouter Beek
@version 2016/05-2016/06, 2016/11, 2017/02
*/

:- use_module(library(dcg/dcg_ascii), [space//0]).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(uri/rfc3986)).

:- meta_predicate
    're+'(3, -, ?, ?),
    're*'(3, -, ?, ?).





circularstring_text(_, Points) -->
  empty_set(Points), !.
circularstring_text(ZM, Points) -->
  must_see_code(0'(),
  're+'(point(ZM), Points),
  must_see_code(0')).


circularstring_text_representation(ZM1, circularString(Points)) -->
  keyword(`circularstring`),
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



compoundcurve_text(_, L) -->
  empty_set(L), !.
compoundcurve_text(ZM, L) -->
  must_see_code(0'(),
  're+'(single_curve_text(ZM), L),
  must_see_code(0')).



compoundcurve_text_representation(ZM1, compoundCurve(L)) -->
  keyword(`compoundcurve`),
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



curvepolygon_text(_, L) -->
  empty_set(L), !.
curvepolygon_text(ZM, L) -->
  must_see_code(0'(),
  're+'(ring_text(ZM), L),
  must_see_code(0')).


curvepolygon_text_body(ZM, L) -->
  curvepolygon_text(ZM, L).


curvepolygon_text_representation(ZM1, curvePolygon(L)) -->
  keyword(`curvepolygon`),
  z_m(ZM1, ZM2),
  curvepolygon_text_body(ZM2, L), !.
curvepolygon_text_representation(ZM, Shape) -->
  polygon_text_representation(ZM, Shape), !.
curvepolygon_text_representation(ZM, Shape) -->
  triangle_text_representation(ZM, Shape).



geometrycollection_text(_, Shapes) -->
  empty_set(Shapes), !.
geometrycollection_text(ZM, Shapes) -->
  're+'(wkt_representation(ZM), Shapes),
  must_see_code(0')).


geometrycollection_text_representation(ZM1, geometryCollection(Shapes)) -->
  keyword(`geometrycollection`),
  z_m(ZM1, ZM2),
  geometrycollection_text(ZM2, Shapes).



linestring_text(_, Points) -->
  empty_set(Points), !.
linestring_text(ZM, Points) -->
  must_see_code(0'(),
  're+'(point(ZM), Points),
  must_see_code(0')).


linestring_text_body(ZM, Points) -->
  linestring_text(ZM, Points).


linestring_text_representation(ZM1, lineString(Points)) -->
  keyword(`linestring`),
  z_m(ZM1, ZM2),
  linestring_text_body(ZM2, Points).



multicurve_text(_, L) -->
  empty_set(L), !.
multicurve_text(ZM, L) -->
  must_see_code(0'(),
  're+'(curve_text(ZM), L),
  must_see_code(0')).


multicurve_text_representation(ZM1, multiCurve(L)) -->
  keyword(`multicurve`),
  z_m(ZM1, ZM2),
  multicurve_text(ZM2, L), !.
multicurve_text_representation(ZM, Shape) -->
  multilinestring_text_representation(ZM, Shape).



multilinestring_text(_, Pointss) -->
  empty_set(Pointss), !.
multilinestring_text(ZM, Pointss) -->
  must_see_code(0'(),
  're+'(linestring_text_body(ZM), Pointss),
  must_see_code(0')).


multilinestring_text_representation(ZM1, multiLineString(Pointss)) -->
  keyword(`multilinestring`),
  z_m(ZM1, ZM2),
  multilinestring_text(ZM2, Pointss).



multipoint_text(_, Points) -->
  empty_set(Points), !.
multipoint_text(ZM, Points) -->
  must_see_code(0'(),
  're+'(point_text(ZM), Points),
  must_see_code(0')).


multipoint_text_representation(ZM1, multiPoint(Points)) -->
  keyword(`multipoint`),
  z_m(ZM1, ZM2),
  multipoint_text(ZM2, Points).



multipolygon_text(_, L) -->
  empty_set(L), !.
multipolygon_text(ZM, L) -->
  must_see_code(0'(),
  're+'(polygon_text_body(ZM), L),
  must_see_code(0')).


multipolygon_text_representation(ZM1, multiPolygon(L)) -->
  keyword(`multipolygon`),
  z_m(ZM1, ZM2),
  multipolygon_text(ZM2, L).



multisurface_text(_, L) -->
  empty_set(L), !.
multisurface_text(ZM, L) -->
  must_see_code(0'(),
  're+'(surface_text(ZM), L),
  must_see_code(0')).


multisurface_text_representation(ZM1, multiSurface(L)) -->
  keyword(`multisurface`),
  z_m(ZM1, ZM2),
  multisurface_text(ZM2, L), !.
multisurface_text_representation(ZM, Shape) -->
  multipolygon_text_representation(ZM, Shape), !.
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
  keyword(`point`),
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
  keyword(`polygon`),
  z_m(ZM1, ZM2),
  polygon_text_body(ZM2, Pointss).


polyhedralsurface_text(_, Pointsss) -->
  empty_set(Pointsss), !.
polyhedralsurface_text(ZM, Pointsss) -->
  must_see_code(0'(),
  're+'(polygon_text_body(ZM), Pointsss),
  must_see_code(0')).


polyhedralsurface_text_representation(ZM1, polyhedralSurface(Pointsss)) -->
  keyword(`polyhedralsurface`),
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
  keyword(`curvepolygon`),
  curvepolygon_text_body(ZM, L), !.
surface_text(ZM, Pointss) -->
  polygon_text_body(ZM, Pointss).


surface_text_representation(ZM, Shape) -->
  curvepolygon_text_representation(ZM, Shape).



tin_text(_, Pointss) -->
  empty_set(Pointss), !.
tin_text(ZM, Pointss) -->
  must_see_code(0'(),
  're+'(triangle_text_body(ZM), Pointss),
  must_see_code(0')).


tin_text_representation(ZM1, tin(Pointss)) -->
  keyword(`tin`),
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
  keyword(`triangle`),
  z_m(ZM1, ZM2),
  triangle_text_body(ZM2, Points).



%! wkt(-Crs:atom, -Shape:compound)// is det.

wkt(Crs, Shape) -->
  (   "<", 'URI'(Crs), ">"
  ->  +(space), !
  ;   {Crs = 'http://www.opengis.net/def/crs/OGC/1.3/CRS84'}
  ),
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

empty_set([]) -->
  keyword(`empty`).



keyword([H|T]) -->
  '[a-zA-Z]'(C),
  {code_type(H, to_lower(C))},
  keyword(T).
keyword([]) -->
  ('[a-zA-Z]'(_) -> !, {fail} ; ""),
  skip_ws.

'[a-zA-Z]'(C) -->
  [C],
  { between(0'a, 0'z, C)
  ; between(0'A, 0'Z, C)
  }, !.



m(N) -->
  number(N).



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



%! 're+'(:Dcg_1, -L)// is det.

're+'(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H),
  're*'(Dcg_1, T).



%! 're*'(:Dcg_1, -L)// is det.

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



% @tbd
z_m(ZM, ZM) --> "".
%z_m(_, zm) --> "ZM", +(ws).
%z_m(_, z) --> "Z", +(ws).
%z_m(_, m) --> "M", +(ws).
