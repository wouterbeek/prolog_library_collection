:- module(
  wkt_parse,
  [
    wkt_parse//1, % -Shape:compound
    wkt_parse//4  % -Z:boolean, -LRS:boolean, -Crs:atom, -Shape:compound
  ]
).

/** <module> Well-Known Text (WKT): Parser

@author Wouter Beek
@version 2016/05-2016/06, 2016/11, 2017/02, 2017/04-2017/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(uri/rfc3986)).

:- meta_predicate
    'wkt+'(3, -, ?, ?),
    'wkt*'(3, -, ?, ?).





% CircularString

circularstring_text(Z, LRS, Points) -->
  "(", !,
  'wkt+'(point(Z, LRS), Points),
  must_see_code(0')).
circularstring_text(_, _, []) -->
  empty_set.

circularstring_text_representation(Z, LRS, 'CircularString'(Points)) -->
  keyword(`circularstring`),
  z_m(Z, LRS),
  circularstring_text(Z, LRS, Points).



collection_text_representation(Z, LRS, Multipoint) -->
  multipoint_text_representation(Z, LRS, Multipoint), !.
collection_text_representation(Z, LRS, Shape) -->
  multicurve_text_representation(Z, LRS, Shape), !.
collection_text_representation(Z, LRS, Shape) -->
  multisurface_text_representation(Z, LRS, Shape), !.
collection_text_representation(Z, LRS, GeometryCollection) -->
  geometrycollection_text_representation(Z, LRS, GeometryCollection).



% CompoundCurve

compoundcurve_text(Z, LRS, L) -->
  "(", !,
  'wkt+'(single_curve_text(Z, LRS), L),
  must_see_code(0')).
compoundcurve_text(_, _, []) -->
  empty_set.

compoundcurve_text_representation(Z, LRS, 'CompoundCurve'(L)) -->
  keyword(`compoundcurve`),
  z_m(Z, LRS),
  compoundcurve_text(Z, LRS, L).



curve_text(Z, LRS, Points) -->
  linestring_text_body(Z, LRS, Points), !.
curve_text(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString), !.
curve_text(Z, LRS, CompoundCurve) -->
  compoundcurve_text_representation(Z, LRS, CompoundCurve).


curve_text_representation(Z, LRS, Shape) -->
  linestring_text_representation(Z, LRS, Shape), !.
curve_text_representation(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString), !.
curve_text_representation(Z, LRS, CompoundCurve) -->
  compoundcurve_text_representation(Z, LRS, CompoundCurve).



% CurvePolygon

curvepolygon_text(Z, LRS, L) -->
  "(", !,
  'wkt+'(ring_text(Z, LRS), L),
  must_see_code(0')).
curvepolygon_text(_, _, []) -->
  empty_set.

curvepolygon_text_body(Z, LRS, L) -->
  curvepolygon_text(Z, LRS, L).

curvepolygon_text_representation(Z, LRS, 'CurvePolygon'(L)) -->
  keyword(`curvepolygon`),
  z_m(Z, LRS),
  curvepolygon_text_body(Z, LRS, L), !.
curvepolygon_text_representation(Z, LRS, Shape) -->
  polygon_text_representation(Z, LRS, Shape), !.
curvepolygon_text_representation(Z, LRS, Shape) -->
  triangle_text_representation(Z, LRS, Shape).



% GeometryCollection

geometrycollection_text(Z, LRS, Shapes) -->
  "(", !,
  'wkt+'(wkt_representation(Z, LRS), Shapes),
  must_see_code(0')).
geometrycollection_text(_, _, []) -->
  empty_set.

geometrycollection_text_representation(Z, LRS, 'GeometryCollection'(Shapes)) -->
  keyword(`geometrycollection`),
  z_m(Z, LRS),
  geometrycollection_text(Z, LRS, Shapes).



% LineString

linestring_text(Z, LRS, Points) -->
  "(", !,
  'wkt+'(point(Z, LRS), Points),
  must_see_code(0')).
linestring_text(_, _, []) -->
  empty_set.

linestring_text_body(Z, LRS, Points) -->
  linestring_text(Z, LRS, Points).

linestring_text_representation(Z, LRS, 'LineString'(Points)) -->
  keyword(`linestring`),
  z_m(Z, LRS),
  linestring_text_body(Z, LRS, Points).



% MultiCurve

multicurve_text(Z, LRS, L) -->
  "(", !,
  'wkt+'(curve_text(Z, LRS), L),
  must_see_code(0')).
multicurve_text(_, _, []) -->
  empty_set.

multicurve_text_representation(Z, LRS, 'MultiCurve'(L)) -->
  keyword(`multicurve`),
  z_m(Z, LRS),
  multicurve_text(Z, LRS, L), !.
multicurve_text_representation(Z, LRS, Shape) -->
  multilinestring_text_representation(Z, LRS, Shape).



% MultiLineString

multilinestring_text(Z, LRS, Pointss) -->
  "(", !,
  'wkt+'(linestring_text_body(Z, LRS), Pointss),
  must_see_code(0')).
multilinestring_text(_, _, []) -->
  empty_set.

multilinestring_text_representation(Z, LRS, 'MultiLineString'(Pointss)) -->
  keyword(`multilinestring`),
  z_m(Z, LRS),
  multilinestring_text(Z, LRS, Pointss).



% MultiPoint

multipoint_text(Z, LRS, Points) -->
  "(", !,
  'wkt+'(point_text(Z, LRS), Points),
  must_see_code(0')).
multipoint_text(_, _, []) -->
  empty_set.

multipoint_text_representation(Z, LRS, 'MultiPoint'(Points)) -->
  keyword(`multipoint`),
  z_m(Z, LRS),
  multipoint_text(Z, LRS, Points).



% MultiPolygon

multipolygon_text(Z, LRS, L) -->
  "(", !,
  'wkt+'(polygon_text_body(Z, LRS), L),
  must_see_code(0')).
multipolygon_text(_, _, []) -->
  empty_set.

multipolygon_text_representation(Z, LRS, 'MultiPolygon'(L)) -->
  keyword(`multipolygon`),
  z_m(Z, LRS),
  multipolygon_text(Z, LRS, L).



% MultiSurface

multisurface_text(Z, LRS, L) -->
  "(", !,
  'wkt+'(surface_text(Z, LRS), L),
  must_see_code(0')).
multisurface_text(_, _, []) -->
  empty_set.

multisurface_text_representation(Z, LRS, 'MultiSurface'(L)) -->
  keyword(`multisurface`),
  z_m(Z, LRS),
  multisurface_text(Z, LRS, L), !.
multisurface_text_representation(Z, LRS, Shape) -->
  multipolygon_text_representation(Z, LRS, Shape), !.
multisurface_text_representation(Z, LRS, PolyhedralSurface) -->
  polyhedralsurface_text_representation(Z, LRS, PolyhedralSurface), !.
multisurface_text_representation(Z, LRS, Tin) -->
  tin_text_representation(Z, LRS, Tin).



% Point

point_text(Z, LRS, Point) -->
  "(", !,
  point(Z, LRS, Point),
  must_see_code(0')).
point_text(_, _, []) -->
  empty_set.

point_text_representation(Z, LRS, 'Point'(Coords)) -->
  keyword(`point`),
  z_m(Z, LRS),
  point_text(Z, LRS, Coords).



% Polygon

polygon_text(Z, LRS, Pointss) -->
  "(", !,
  'wkt+'(linestring_text(Z, LRS), Pointss),
  must_see_code(0')).
polygon_text(_, _, []) -->
  empty_set.

polygon_text_body(Z, LRS, Pointss) -->
  polygon_text(Z, LRS, Pointss).

polygon_text_representation(Z, LRS, 'Polygon'(Pointss)) -->
  keyword(`polygon`),
  z_m(Z, LRS),
  polygon_text_body(Z, LRS, Pointss).



% PolyhedralSurface

polyhedralsurface_text(Z, LRS, Pointsss) -->
  "(", !,
  'wkt+'(polygon_text_body(Z, LRS), Pointsss),
  must_see_code(0')).
polyhedralsurface_text(_, _, []) -->
  empty_set.

polyhedralsurface_text_representation(Z, LRS, 'PolyhedralSurface'(Pointsss)) -->
  keyword(`polyhedralsurface`),
  z_m(Z, LRS),
  polyhedralsurface_text(Z, LRS, Pointsss).



ring_text(Z, LRS, Points) -->
  linestring_text_body(Z, LRS, Points), !.
ring_text(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString), !.
ring_text(Z, LRS, CompoundCurve) -->
  compoundcurve_text_representation(Z, LRS, CompoundCurve).



single_curve_text(Z, LRS, Points) -->
  linestring_text_body(Z, LRS, Points), !.
single_curve_text(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString).



surface_text(Z, LRS, curvepolygon(L)) -->
  keyword(`curvepolygon`),
  curvepolygon_text_body(Z, LRS, L), !.
surface_text(Z, LRS, Pointss) -->
  polygon_text_body(Z, LRS, Pointss).

surface_text_representation(Z, LRS, Shape) -->
  curvepolygon_text_representation(Z, LRS, Shape).



% TIN

tin_text(Z, LRS, Pointss) -->
  "(", !,
  'wkt+'(triangle_text_body(Z, LRS), Pointss),
  must_see_code(0')).
tin_text(_, _, []) -->
  empty_set.

tin_text_representation(Z, LRS, 'TIN'(Pointss)) -->
  keyword(`tin`),
  z_m(Z, LRS),
  tin_text(Z, LRS, Pointss).



% Triangle

triangle_text(Z, LRS, Points) -->
  "(", !,
  linestring_text(Z, LRS, Points),
  must_see_code(0')).
triangle_text(_, _, []) -->
  empty_set.

triangle_text_body(Z, LRS, Points) -->
  triangle_text(Z, LRS, Points).

triangle_text_representation(Z, LRS, 'Triangle'(Points)) -->
  keyword(`triangle`),
  z_m(Z, LRS),
  triangle_text_body(Z, LRS, Points).



%! wkt_parse(-Shape:compound)// is det.
%! wkt_parse(-Z:boolean, -LRS:boolean, -Crs:atom, -Shape:compound)// is det.

wkt_parse(Shape) -->
  wkt_parse(_, _, _, Shape).


wkt_parse(Z, LRS, Crs, Shape) -->
  (   "<", 'URI'(Crs), ">"
  ->  +('WS'), !
  ;   {Crs = 'http://www.opengis.net/def/crs/OGC/1.3/CRS84'}
  ),
  wkt_representation(Z, LRS, Shape).

wkt_representation(Z, LRS, Point) -->
  point_text_representation(Z, LRS, Point), !.
wkt_representation(Z, LRS, Shape) -->
  curve_text_representation(Z, LRS, Shape), !.
wkt_representation(Z, LRS, Shape) -->
  surface_text_representation(Z, LRS, Shape), !.
wkt_representation(Z, LRS, Shape) -->
  collection_text_representation(Z, LRS, Shape).





% HELPERS %

%! empty_set// .

empty_set -->
  keyword(`empty`).



%! keyword(+Cs)// .

keyword([H|T]) -->
  alpha(C),
  {code_type(H, to_lower(C))},
  keyword(T).
keyword([]) -->
  (alpha(_) -> !, {fail} ; ""),
  *('WS').



%! m(-N)// ,

m(N) -->
  number(N).



%! must_see_code(-Code)// .

must_see_code(C) -->
  must_see_code(C, *('WS')).



%! point(+Z:boolean, +LRS:boolean, -Point:compound)// is det.

point(false, false, [X,Y]) -->
  'X'(X),
  must_see_code(0' ),
  'Y'(Y),
  *('WS').
point(false, true, [X,Y,LRS]) -->
  point(false, false, [X,Y]),
  " ",
  m(LRS).
point(true, false, [X,Y,Z]) -->
  point(false, false, [X,Y]),
  " ",
  'Z'(Z).
point(true, true, [X,Y,Z,LRS]) -->
  point(true, false, [X,Y,Z]),
  " ",
  m(LRS).



%! 'wkt+'(:Dcg_1, -L)// is det.

'wkt+'(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H),
  'wkt*'(Dcg_1, T).



%! 'wkt*'(:Dcg_1, -L)// is det.

'wkt*'(Dcg_1, [H|T]) -->
  ",", !,
  *('WS'),
  dcg_call(Dcg_1, H),
  'wkt*'(Dcg_1, T).
'wkt*'(_, []) --> "".



%! 'X'(-N)// .

'X'(N) -->
  number(N).



%! 'Y'(-N)// .

'Y'(N) -->
  number(N).



%! 'Z'(-N)// .

'Z'(N) -->
  number(N).



%! z_m(-Z:boolean, -LRS:boolean)// is det.

z_m(Z, LRS) -->
  ("Z" -> {Z = true} ; {Z = false}),
  ("M" -> {LRS = true} ; {LRS = false}).
