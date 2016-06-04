:- module(
  wkt,
  [
    wkt//2,    % +Name, +Array
    wkt_name/1 % ?Name
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





circularstring_text(_, L) -->
  empty_set(L), !.
circularstring_text(ZM, L) -->
  "( ", seplist(point(ZM), +(blank), L), " )".


circularstring_text_representation(ZM1, circularstring(L)) -->
  "CIRCULARSTRING ",
  z_m(ZM1, ZM2),
  circularstring_text(ZM2, L).



collection_text_representation(ZM, L) -->
  multipoint_text_representation(ZM, L), !.
collection_text_representation(ZM, L) -->
  multicurve_text_representation(ZM, L), !.
collection_text_representation(ZM, L) -->
  multisurface_text_representation(ZM, L), !.
collection_text_representation(ZM, L) -->
  geometrycollection_text_representation(ZM, L).



compoundcurve_text(_, L) -->
  empty_set(L), !.
compoundcurve_text(ZM, L) -->
  "( ", seplist('single curve text'(ZM), +(blank), L), " )".



compoundcurve_text_representation(ZM1, compoundcurve(L)) -->
  "COMPOUNDCURVE ",
  z_m(ZM1, ZM2),
  compoundcurve_text(ZM2, L).



curve_text(ZM, L) -->
  linestring_text_body(ZM, L), !.
curve_text(ZM, L) -->
  circularstring_text_representation(ZM, L), !.
curve_text(ZM, L) -->
  compoundcurve_text_representation(ZM, L).


curve_text_representation(ZM, L) -->
  linestring_text_representation(ZM, L), !.
curve_text_representation(ZM, L) -->
  circularstring_text_representation(ZM, L), !.
curve_text_representation(ZM, L) -->
  compoundcurve_text_representation(ZM, L).



curvepolygon_text(_, L) -->
  empty_set(L), !.
curvepolygon_text(ZM, L) -->
  "( ", seplist(ring_text(ZM), +(blank), L), " )".


curvepolygon_text_body(ZM, L) -->
  curvepolygon_text(ZM, L).


curvepolygon_text_representation(ZM1, curvepolygon(L)) -->
  "CURVEPOLYGON ",
  z_m(ZM1, ZM2),
  curvepolygon_text_body(ZM2, L), !.
curvepolygon_text_representation(ZM, L) -->
  polygon_text_representation(ZM, L), !.
curvepolygon_text_representation(ZM, L) -->
  triangle_text_representation(ZM, L).



geometrycollection_text(_, L) -->
  empty_set(L), !.
geometrycollection_text(ZM, L) -->
  seplist(wkt_representation(ZM), +(blank), L), " )".


geometrycollection_text_representation(ZM1, geometrycollection(L)) -->
  "GEOMETRYCOLLECTION ",
  z_m(ZM1, ZM2),
  geometrycollection_text(ZM2, L).



linestring_text(_, L) -->
  empty_set(L), !.
linestring_text(ZM, L) -->
  "( ", seplist(point(ZM), +(blank), L), " )".


linestring_text_body(ZM, L) -->
  linestring_text(ZM, L).


linestring_text_representation(ZM1, linestring(L)) -->
  "LINESTRING ",
  z_m(ZM1, ZM2),
  linestring text body(ZM2, L).



multicurve_text(_, L) -->
  empty_set(L), !.
multicurve_text(ZM, L) -->
  "( ", seplist(curve_text(ZM), +(blank), L), " )".


multicurve_text_representation(ZM1, multicurve(L)) -->
  "MULTICURVE ",
  z_m(ZM1, ZM2),
  multicurve_text(ZM2, L), !.
multicurve_text_representation(ZM, L) -->
  multilinestring_text_representation(ZM, L).



multilinestring_text(_, L) -->
  empty_set(L), !.
multilinestring_text(ZM, L) -->
  "( ", seplist('linestring text body'(ZM), +(blank), L), " )".


multilinestring_text_representation(ZM1, multilinestring(L)) -->
  "MULTILINESTRING ",
  z_m(ZM1, ZM2),
  multilinestring_text(ZM2, L).



multipoint_text(_, L) -->
  empty_set(L), !.
multipoint_text(ZM, L) -->
  "( ", seplist(point_text(ZM), +(blank), L), " )".


multipoint_text_representation(ZM1, multipoint(L)) -->
  "MULTIPOINT ",
  z_m(ZM1, ZM2),
  multipoint_text(ZM2, L).



multipolygon_text(_, L) -->
  empty_set(L), !.
multipolygon_text(ZM, L) -->
  "( ", seplist(polygon_text_body(ZM), +(blank), L), " )".


multipolygon_text_representation(ZM1, multipolygon(L)) -->
  "MULTIPOLYGON ",
  z_m(ZM1, ZM2),
  multipolygon_text(ZM2, L).



multisurface_text(_, L) -->
  empty_set(L), !.
multisurface_text(ZM, L) -->
  "( ", seplist(surface_text(ZM), +(blank), L), " )".


multisurface_text_representation(ZM1, multisurface(L)) -->
 "MULTISURFACE ",
 z_m(ZM1, ZM2),
 multisurface_text(ZM2, L), !.
multisurface_text_representation(ZM, L) -->
  multipolygon_text_representation(ZM, L), !.
multisurface_text_representation(ZM, L) -->
  polyhedralsurface_text_representation(ZM, L), !.
multisurface_text_representation(ZM, L) -->
  tin_text_representation(ZM, L).



point_text(_, L) -->
  empty_set(L), !.
point_text(ZM, L) -->
  "( ", point(ZM, L), " )".


point_text_representation(ZM1, point(L)) -->
  "POINT ",
  z_m(ZM1, ZM2),
  point_text(ZM2, L).



polygon_text(_, L) -->
  empty_set(L), !.
polygon_text(ZM, L) -->
  "( ", seplist(linestring_text(ZM), +(blank), L), " )".


polygon_text_body(ZM, L) -->
  polygon_text(ZM, L).


polygon_text_representation(ZM1, polygon(L)) -->
  "POLYGON ",
  z_m(ZM1, ZM2),
  polygon_text_body(ZM2, L).



polyhedralsurface_text(_, L) -->
  empty_set(L), !.
polyhedralsurface_text(ZM, L) -->
  "( ", seplist(polygon_text_body(ZM), +(blank), L), " )".


polyhedralsurface_text_representation(ZM1, polyhedralsurface(L)) -->
  "POLYHEDRALSURFACE ",
  z_m(ZM1, ZM2),
  polyhedralsurface_text(ZM2, L).



ring_text(ZM, L) --> linestring_text_body(ZM, L), !.
ring_text(ZM, L) --> circularstring_text_representation(ZM, L), !.
ring_text(ZM, L) --> compoundcurve_text_representation(ZM, L).



single_curve_text(ZM, L) -->
  linestring_text_body(ZM, L), !.
single_curve_text(ZM, L) -->
  circularstring_text_representation(ZM, L).



surface_text(ZM, curvepolygon(L)) -->
  "CURVEPOLYGON",
  curvepolygon_text_body(ZM, L), !.
surface_text(ZM, L) -->
  polygon_text_body(ZM, L).


surface_text_representation(ZM, L) -->
  curvepolygon_text_representation(ZM, L).



tin_text(_, L) -->
  empty_set(L), !.
tin_text(ZM, L) -->
  "( ", seplist(triangle_text_body(ZM), +(blank), L), " )".


tin_text_representation(ZM1, tin(L)) -->
  "TIN ",
  z_m(ZM1, ZM2),
  tin_text(ZM2, L).



triangle_text(_, L) -->
  empty_set(L), !.
triangle_text(ZM, L) -->
  "( ", linestring_text(ZM, L), " )".


triangle_text_body(ZM, L) -->
  triangle_text(ZM, L).


triangle_text_representation(ZM1, triangle(L)) -->
  "TRIANGLE ",
  z_m(ZM1, ZM2),
  triangle_text_body(ZM2, L).



wkt(Name, Array) -->
  {Comp =.. [Name,Array]},
  wkt_representation(_, Comp).


wkt_name(circularstring).
wkt_name(compoundcurve).
wkt_name(curvepolygon).
wkt_name(geometrycollection).
wkt_name(linestring).
wkt_name(multicurve).
wkt_name(multilinestring).
wkt_name(multipoint).
wkt_name(multipolygon).
wkt_name(multisurface).
wkt_name(point).
wkt_name(polygon).
wkt_name(polyhedralsurface).
wkt_name(tin).
wkt_name(triangle).


wkt_representation(ZM, L) -->
  point_text_representation(ZM, L), !.
wkt_representation(ZM, L) -->
  curve_text_representation(ZM, L), !.
wkt_representation(ZM, L) -->
  surface_text_representation(ZM, L), !.
wkt_representation(ZM, L) -->
  collection_text_representation(ZM, L).





% HELPERS %

empty_set([]) --> "EMPTY".


m(N) --> number(N).


point(zm, [X,Y,Z,M]) --> point(z, [X,Y,Z]), +(blank), m(M), !.
point(z, [X,Y,Z]) --> point(none, [X,Y]), +(blank), z(Z), !.
point(m, [X,Y,M]) --> point(none, [X,Y]), +(blank), m(M), !.
point(_, [X,Y]) --> x(X), +(blank), y(Y).


x(N) --> number(N).


y(N) --> number(N).


z(N) --> number(N).


z_m(ZM, ZM) --> "".
z_m(_, zm) --> "ZM ".
z_m(_, z) --> "Z ".
z_m(_, m) --> "M ".
