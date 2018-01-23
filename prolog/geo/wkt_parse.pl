:- module(
  wkt_parse,
  [
    wkt_parse//1, % -Shape:compound
    wkt_parse//4  % -Z:boolean, -LRS:boolean, -Crs:atom, -Shape:compound
  ]
).

/** <module> Well-Known Text (WKT): Parser

@author Wouter Beek
@version 2016/05-2018/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(uri/rfc3986)).

:- meta_predicate
    'wkt+'(3, -, ?, ?),
    'wkt*'(3, -, ?, ?).





% CircularString

circularstring_text(Z, LRS, 'CircularString'(Points)) -->
  (   "("
  ->  'wkt+'(point(Z, LRS), Points),
      must_see_code(0'))
  ;   empty(Points)
  ).

circularstring_text_representation(Z, LRS, CircularString) -->
  keyword(`circularstring`),
  z_m(Z, LRS),
  circularstring_text(Z, LRS, CircularString).



collection_text_representation(Z, LRS, Multipoint) -->
  multipoint_text_representation(Z, LRS, Multipoint), !.
collection_text_representation(Z, LRS, MultiCurve) -->
  multicurve_text_representation(Z, LRS, MultiCurve), !.
collection_text_representation(Z, LRS, MultiSurface) -->
  multisurface_text_representation(Z, LRS, MultiSurface), !.
collection_text_representation(Z, LRS, GeometryCollection) -->
  geometrycollection_text_representation(Z, LRS, GeometryCollection).



% CompoundCurve

compoundcurve_text(Z, LRS, 'CompoundCurve'(SingleCurve)) -->
  (   "("
  ->  'wkt+'(single_curve_text(Z, LRS), SingleCurve),
      must_see_code(0'))
  ;   empty(SingleCurve)
  ).

compoundcurve_text_representation(Z, LRS, CompoundCurve) -->
  keyword(`compoundcurve`),
  z_m(Z, LRS),
  compoundcurve_text(Z, LRS, CompoundCurve).



curve_text(Z, LRS, LineString) -->
  linestring_text_body(Z, LRS, LineString), !.
curve_text(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString), !.
curve_text(Z, LRS, CompoundCurve) -->
  compoundcurve_text_representation(Z, LRS, CompoundCurve).


curve_text_representation(Z, LRS, LineString) -->
  linestring_text_representation(Z, LRS, LineString), !.
curve_text_representation(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString), !.
curve_text_representation(Z, LRS, CompoundCurve) -->
  compoundcurve_text_representation(Z, LRS, CompoundCurve).



% CurvePolygon

curvepolygon_text(Z, LRS, 'CurvePolygon'(Rings)) -->
  (   "("
  ->  'wkt+'(ring_text(Z, LRS), Rings),
      must_see_code(0'))
  ;   empty(Rings)
  ).

curvepolygon_text_body(Z, LRS, CurvePolygon) -->
  curvepolygon_text(Z, LRS, CurvePolygon).

curvepolygon_text_representation(Z, LRS, CurvePolygon) -->
  keyword(`curvepolygon`),
  z_m(Z, LRS),
  curvepolygon_text_body(Z, LRS, CurvePolygon), !.
curvepolygon_text_representation(Z, LRS, Polygon) -->
  polygon_text_representation(Z, LRS, Polygon), !.
curvepolygon_text_representation(Z, LRS, Triangle) -->
  triangle_text_representation(Z, LRS, Triangle).



% GeometryCollection

geometrycollection_text(Z, LRS, 'GeometryCollection'(Shapes)) -->
  (   "("
  ->  'wkt+'(wkt_representation(Z, LRS), Shapes),
      must_see_code(0'))
  ;   empty(Shapes)
  ).

geometrycollection_text_representation(Z, LRS, GeometryCollection) -->
  keyword(`geometrycollection`),
  z_m(Z, LRS),
  geometrycollection_text(Z, LRS, GeometryCollection).



% LineString

linestring_text(Z, LRS, 'LineString'(Points)) -->
  (   "("
  ->  'wkt+'(point(Z, LRS), Points),
      must_see_code(0'))
  ;   empty(Points)
  ).

linestring_text_body(Z, LRS, LineString) -->
  linestring_text(Z, LRS, LineString).

linestring_text_representation(Z, LRS, LineString) -->
  keyword(`linestring`),
  z_m(Z, LRS),
  linestring_text_body(Z, LRS, LineString).



% MultiCurve

multicurve_text(Z, LRS, 'MultiCurve'(Curves)) -->
  (   "("
  ->  'wkt+'(curve_text(Z, LRS), Curves),
      must_see_code(0'))
  ;   empty(Curves)
  ).

multicurve_text_representation(Z, LRS, MultiCurve) -->
  keyword(`multicurve`),
  z_m(Z, LRS),
  multicurve_text(Z, LRS, MultiCurve), !.
multicurve_text_representation(Z, LRS, MultiLineString) -->
  multilinestring_text_representation(Z, LRS, MultiLineString).



% MultiLineString

multilinestring_text(Z, LRS, 'MultiLineString'(LineStrings)) -->
  (   "("
  ->  'wkt+'(linestring_text_body(Z, LRS), LineStrings),
      must_see_code(0'))
  ;   empty(LineStrings)
  ).

multilinestring_text_representation(Z, LRS, MultiLineString) -->
  keyword(`multilinestring`),
  z_m(Z, LRS),
  multilinestring_text(Z, LRS, MultiLineString).



% MultiPoint

multipoint_text(Z, LRS, 'MultiPoint'(Points)) -->
  (   "("
  ->  'wkt+'(point_text(Z, LRS), Points),
      must_see_code(0'))
  ;   empty(Points)
  ).

multipoint_text_representation(Z, LRS, MultiPoint) -->
  keyword(`multipoint`),
  z_m(Z, LRS),
  multipoint_text(Z, LRS, MultiPoint).



% MultiPolygon

multipolygon_text(Z, LRS, 'MultiPolygon'(Polygons)) -->
  (   "("
  ->  'wkt+'(polygon_text_body(Z, LRS), Polygons),
      must_see_code(0'))
  ;   empty(Polygons)
  ).

multipolygon_text_representation(Z, LRS, MultiPolygon) -->
  keyword(`multipolygon`),
  z_m(Z, LRS),
  multipolygon_text(Z, LRS, MultiPolygon).



% MultiSurface

multisurface_text(Z, LRS, 'MultiSurface'(Surfaces)) -->
  (   "("
  ->  'wkt+'(surface_text(Z, LRS), Surfaces),
      must_see_code(0'))
  ;   empty(Surfaces)
  ).

multisurface_text_representation(Z, LRS, MultiSurface) -->
  keyword(`multisurface`),
  z_m(Z, LRS),
  multisurface_text(Z, LRS, MultiSurface), !.
multisurface_text_representation(Z, LRS, MultiPolygon) -->
  multipolygon_text_representation(Z, LRS, MultiPolygon), !.
multisurface_text_representation(Z, LRS, PolyhedralSurface) -->
  polyhedralsurface_text_representation(Z, LRS, PolyhedralSurface), !.
multisurface_text_representation(Z, LRS, Tin) -->
  tin_text_representation(Z, LRS, Tin).



% Point

point_text(Z, LRS, Point) -->
  "(",
  point(Z, LRS, Point),
  must_see_code(0')).

point_text_representation(Z, LRS, Point) -->
  keyword(`point`),
  z_m(Z, LRS),
  point_text(Z, LRS, Point).



% Polygon

polygon_text(Z, LRS, 'Polygon'(LineStrings)) -->
  (   "("
  ->  'wkt+'(linestring_text(Z, LRS), LineStrings),
      must_see_code(0'))
  ;   empty(LineStrings)
  ).

polygon_text_body(Z, LRS, Polygon) -->
  polygon_text(Z, LRS, Polygon).

polygon_text_representation(Z, LRS, Polygon) -->
  keyword(`polygon`),
  z_m(Z, LRS),
  polygon_text_body(Z, LRS, Polygon).



% PolyhedralSurface

polyhedralsurface_text(Z, LRS, 'PolyhedralSurface'(Polygons)) -->
  (   "("
  ->  'wkt+'(polygon_text_body(Z, LRS), Polygons),
      must_see_code(0'))
  ;   empty(Polygons)
  ).

polyhedralsurface_text_representation(Z, LRS, PolyhedralSurface) -->
  keyword(`polyhedralsurface`),
  z_m(Z, LRS),
  polyhedralsurface_text(Z, LRS, PolyhedralSurface).



ring_text(Z, LRS, LineString) -->
  linestring_text_body(Z, LRS, LineString), !.
ring_text(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString), !.
ring_text(Z, LRS, CompoundCurve) -->
  compoundcurve_text_representation(Z, LRS, CompoundCurve).



single_curve_text(Z, LRS, LineString) -->
  linestring_text_body(Z, LRS, LineString), !.
single_curve_text(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString).



surface_text(Z, LRS, CurvePolygon) -->
  keyword(`curvepolygon`),
  curvepolygon_text_body(Z, LRS, CurvePolygon), !.
surface_text(Z, LRS, Polygon) -->
  polygon_text_body(Z, LRS, Polygon).

surface_text_representation(Z, LRS, CurvePolygon) -->
  curvepolygon_text_representation(Z, LRS, CurvePolygon).



% TIN

tin_text(Z, LRS, 'TIN'(Triangles)) -->
  (   "("
  ->  'wkt+'(triangle_text_body(Z, LRS), Triangles),
      must_see_code(0'))
  ;   empty(Triangles)
  ).

tin_text_representation(Z, LRS, Tin) -->
  keyword(`tin`),
  z_m(Z, LRS),
  tin_text(Z, LRS, Tin).



% Triangle

triangle_text(Z, LRS, 'Triangle'(Points)) -->
  (   "("
  ->  linestring_text(Z, LRS, 'LineString'(Points)),
      must_see_code(0'))
  ;   empty(Points)
  ).

triangle_text_body(Z, LRS, Triangle) -->
  triangle_text(Z, LRS, Triangle).

triangle_text_representation(Z, LRS, Triangle) -->
  keyword(`triangle`),
  z_m(Z, LRS),
  triangle_text_body(Z, LRS, Triangle).



%! wkt_parse(-Shape:compound)// is det.
%! wkt_parse(-Z:boolean, -LRS:boolean, -Crs:atom, -Shape:compound)// is det.

wkt_parse(Shape) -->
  wkt_parse(_, _, _, Shape).


wkt_parse(Z, LRS, Crs, Shape) -->
  (   "<", 'URI'(Crs), ">"
  ->  blank,
      blanks, !
  ;   {Crs = 'http://www.opengis.net/def/crs/OGC/1.3/CRS84'}
  ),
  wkt_representation(Z, LRS, Shape).

wkt_representation(Z, LRS, Point) -->
  point_text_representation(Z, LRS, Point), !.
wkt_representation(Z, LRS, Curve) -->
  curve_text_representation(Z, LRS, Curve), !.
wkt_representation(Z, LRS, Surface) -->
  surface_text_representation(Z, LRS, Surface), !.
wkt_representation(Z, LRS, Collection) -->
  collection_text_representation(Z, LRS, Collection).





% HELPERS %

%! empty(-Shapes:list:compound)// .

empty([]) -->
  keyword(`empty`).



%! keyword(+Cs)// .

keyword([H|T]) -->
  alpha(C),
  {code_type(H, to_lower(C))},
  keyword(T).
keyword([]) -->
  (alpha(_) -> !, {fail} ; ""),
  blanks.



%! m(-N)// ,

m(N) -->
  number(N).



%! must_see_code(-Code)// .

must_see_code(C) -->
  must_see_code(C, blanks).



%! point(+Z:boolean, +LRS:boolean, -Point:compound)// is det.

point(true, true, 'Point'(X,Y,Z,LRS)) --> !,
  'X'(X),
  must_see_code(0' ),
  blanks,
  'Y'(Y),
  must_see_code(0' ),
  blanks,
  'Z'(Z),
  must_see_code(0' ),
  blanks,
  m(LRS).
point(true, false, 'Point'(X,Y,Z)) --> !,
  'X'(X),
  must_see_code(0' ),
  blanks,
  'Y'(Y),
  must_see_code(0' ),
  blanks,
  'Z'(Z),
  blanks.
point(false, true, 'Point'(X,Y,LRS)) --> !,
  'X'(X),
  must_see_code(0' ),
  blanks,
  'Y'(Y),
  must_see_code(0' ),
  blanks,
  'm'(LRS),
  blanks.
point(false, false, 'Point'(X,Y)) -->
  'X'(X),
  must_see_code(0' ),
  blanks,
  'Y'(Y),
  blanks.



%! 'wkt+'(:Dcg_1, -L)// is det.

'wkt+'(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H),
  'wkt*'(Dcg_1, T).



%! 'wkt*'(:Dcg_1, -L)// is det.

'wkt*'(Dcg_1, [H|T]) -->
  ",", !,
  blanks,
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
