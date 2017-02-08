:- module(
  wkt_generate,
  [
    wkt_generate//1, % +Shape:compound
    wkt_generate//4  % +Z:boolean, +LRS:boolean, +Crs:atom, +Shape:compound
  ]
).

/** <module> Well-Known Text (WKT): Generator

@version 2016/11, 2017/02
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(uri/rfc3986)).

:- meta_predicate
    're+'(3, -, ?, ?),
    're*'(3, -, ?, ?).





% CircularString

circularstring_text(_, _, []) --> !,
  "Empty".
circularstring_text(Z, LRS, Points) -->
  "(",
  're+'(point(Z, LRS), Points),
  ")".

circularstring_text_representation(Z, LRS, circularstring(Points)) -->
  "CircularString",
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

compoundcurve_text(_, _, []) --> !,
  "Empty".
compoundcurve_text(Z, LRS, L) -->
  "(",
  're+'(single_curve_text(Z, LRS), L),
  ")".

compoundcurve_text_representation(Z, LRS, compoundcurve(L)) -->
  "CompoundCurve",
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

curvepolygon_text(_, _, []) --> !,
  "Empty".
curvepolygon_text(Z, LRS, L) -->
  "(",
  're+'(ring_text(Z, LRS), L),
  ")".

curvepolygon_text_body(Z, LRS, L) -->
  curvepolygon_text(Z, LRS, L).

curvepolygon_text_representation(Z, LRS, curvepolygon(L)) -->
  "CurvePolygon",
  z_m(Z, LRS),
  curvepolygon_text_body(Z, LRS, L), !.
curvepolygon_text_representation(Z, LRS, Shape) -->
  polygon_text_representation(Z, LRS, Shape), !.
curvepolygon_text_representation(Z, LRS, Shape) -->
  triangle_text_representation(Z, LRS, Shape).



% GeometryCollection

geometrycollection_text(_, _, []) --> !,
  "Empty".
geometrycollection_text(Z, LRS, Shapes) -->
  "(",
  're+'(wkt_representation(Z, LRS), Shapes),
  ")".

geometrycollection_text_representation(Z, LRS, geometrycollection(Shapes)) -->
  "GeometryCollection",
  z_m(Z, LRS),
  geometrycollection_text(Z, LRS, Shapes).



% LineString

linestring_text(_, _, []) --> !,
  "Empty".
linestring_text(Z, LRS, Points) -->
  "(",
  're+'(point(Z, LRS), Points),
  ")".

linestring_text_body(Z, LRS, Points) -->
  linestring_text(Z, LRS, Points).

linestring_text_representation(Z, LRS, linestring(Points)) -->
  "LineString",
  z_m(Z, LRS),
  linestring_text_body(Z, LRS, Points).



% MultiCurve

multicurve_text(_, _, []) --> !,
  "Empty".
multicurve_text(Z, LRS, L) -->
  "(",
  're+'(curve_text(Z, LRS), L),
  ")".

multicurve_text_representation(Z, LRS, multicurve(L)) -->
  "MultiCurve",
  z_m(Z, LRS),
  multicurve_text(Z, LRS, L), !.
multicurve_text_representation(Z, LRS, Shape) -->
  multilinestring_text_representation(Z, LRS, Shape).



% MultiLineString

multilinestring_text(_, _, []) --> !,
  "Empty".
multilinestring_text(Z, LRS, Pointss) -->
  "(",
  're+'(linestring_text_body(Z, LRS), Pointss),
  ")".

multilinestring_text_representation(Z, LRS, multilinestring(Pointss)) -->
  "MultiLineString",
  z_m(Z, LRS),
  multilinestring_text(Z, LRS, Pointss).



% MultiPoint

multipoint_text(_, _, []) --> !,
  "Empty".
multipoint_text(Z, LRS, Points) -->
  "(",
  're+'(point_text(Z, LRS), Points),
  ")".

multipoint_text_representation(Z, LRS, multipoint(Points)) -->
  "MultiPoint",
  z_m(Z, LRS),
  multipoint_text(Z, LRS, Points).



% MultiPolygon

multipolygon_text(_, _, []) --> !,
  "Empty".
multipolygon_text(Z, LRS, L) -->
  "(",
  're+'(polygon_text_body(Z, LRS), L),
  ")".

multipolygon_text_representation(Z, LRS, multipolygon(L)) -->
  "MultiPolygon",
  z_m(Z, LRS),
  multipolygon_text(Z, LRS, L).



% MultiSurface

multisurface_text(_, _, []) --> !,
  "Empty".
multisurface_text(Z, LRS, L) -->
  "(",
  're+'(surface_text(Z, LRS), L),
  ")".

multisurface_text_representation(Z, LRS, multisurface(L)) -->
  "MultiSurface",
  z_m(Z, LRS),
  multisurface_text(Z, LRS, L), !.
multisurface_text_representation(Z, LRS, Shape) -->
  multipolygon_text_representation(Z, LRS, Shape), !.
multisurface_text_representation(Z, LRS, PolyhedralSurface) -->
  polyhedralsurface_text_representation(Z, LRS, PolyhedralSurface), !.
multisurface_text_representation(Z, LRS, Tin) -->
  tin_text_representation(Z, LRS, Tin).



% Point

point_text(_, _, []) --> !,
  "Empty".
point_text(Z, LRS, [Point]) -->
  "(",
  point(Z, LRS, Point),
  ")".

point_text_representation(Z, LRS, Point) -->
  {Point =.. [point|_]},
  "Point",
  z_m(Z, LRS),
  point_text(Z, LRS, [Point]).



% Polygon

polygon_text(_, _, []) --> !,
  "Empty".
polygon_text(Z, LRS, Pointss) -->
  "(",
  're+'(linestring_text(Z, LRS), Pointss),
  ")".

polygon_text_body(Z, LRS, Pointss) -->
  polygon_text(Z, LRS, Pointss).

polygon_text_representation(Z, LRS, polygon(Pointss)) -->
  "Polygon",
  z_m(Z, LRS),
  polygon_text_body(Z, LRS, Pointss).



% PolyhedralSurface

polyhedralsurface_text(_, _, []) --> !,
  "Empty".
polyhedralsurface_text(Z, LRS, Pointsss) -->
  "(",
  're+'(polygon_text_body(Z, LRS), Pointsss),
  ")".

polyhedralsurface_text_representation(Z, LRS, polyhedralsurface(Pointsss)) -->
  "PolyhedralSurface",
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



% CurvePolygon

surface_text(Z, LRS, curvepolygon(L)) -->
  "CurvePolygon",
  curvepolygon_text_body(Z, LRS, L), !.
surface_text(Z, LRS, Pointss) -->
  polygon_text_body(Z, LRS, Pointss).

surface_text_representation(Z, LRS, Shape) -->
  curvepolygon_text_representation(Z, LRS, Shape).



% TIN

tin_text(_, _, []) --> !,
  "Empty".
tin_text(Z, LRS, Pointss) -->
  "(",
  're+'(triangle_text_body(Z, LRS), Pointss),
  ")".

tin_text_representation(Z, LRS, tin(Pointss)) -->
  "TIN",
  z_m(Z, LRS),
  tin_text(Z, LRS, Pointss).



% Triangle

triangle_text(_, _, []) --> !,
  "Empty".
triangle_text(Z, LRS, Points) -->
  "(",
  linestring_text(Z, LRS, Points),
  ")".

triangle_text_body(Z, LRS, Points) -->
  triangle_text(Z, LRS, Points).

triangle_text_representation(Z, LRS, triangle(Points)) -->
  "Triangle",
  z_m(Z, LRS),
  triangle_text_body(Z, LRS, Points).



%! wkt_generate(+Shape:compound)// is det.
%! wkt_generate(+Z:boolean, +LRS:boolean, +Crs:atom, +Shape:compound)// is det.

wkt_generate(Shape) -->
  wkt_generate(
    false,
    false,
    'http://www.opengis.net/def/crs/OGC/1.3/CRS84',
    Shape
  ).

wkt_generate(Z, LRS, Crs, Shape) -->
  (   {Crs = 'http://www.opengis.net/def/crs/OGC/1.3/CRS84'}
  ->  ""
  ;   "<", 'URI'(Crs), "> "
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

%! m(+Number)// is det.

m(N) -->
  number(N).



%! point(+Z:boolean, +LRS:boolean, +Point:compound)// is det.

point(false, false, point(X,Y)) -->
  'X'(X),
  " ",
  'Y'(Y).
point(false, true, point(X,Y,LRS)) -->
  point(false, false, point(X,Y)),
  " ",
  m(LRS), !.
point(true, false, point(X,Y,Z)) -->
  point(false, false, point(X,Y)),
  " ",
  'Z'(Z), !.
point(true, true, point(X,Y,Z,LRS)) -->
  point(true, false, point(X,Y,Z)),
  " ",
  m(LRS), !.



're+'(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H),
  're*'(Dcg_1, T).

're*'(Dcg_1, [H|T]) -->
  ",", !,
  dcg_call(Dcg_1, H),
  're*'(Dcg_1, T).
're*'(_, []) --> "".



%! 'X'(+Number)// is det.

'X'(N) -->
  number(N).



%! 'Y'(+Number)// is det.

'Y'(N) -->
  number(N).



%! 'Z'(+Number)// is det.

'Z'(N) -->
  number(N).



%! z_m(+Z:boolean, +LRS:boolean)// is det.

z_m(false, false) --> "".
z_m(false, true)  --> "M",  +(ws).
z_m(true, false)  --> "Z",  +(ws).
z_m(true, true)   --> "ZM", +(ws).
