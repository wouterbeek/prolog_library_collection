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
  "(", +(ws), seplist(point(ZM), +(ws), L), +(ws), ")".


circularstring_text_representation(ZM1, circularstring(L)) -->
  "CIRCULARSTRING", +(ws),
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
  "(", +(ws), seplist('single curve text'(ZM), +(ws), L), +(ws), ")".



compoundcurve_text_representation(ZM1, compoundcurve(L)) -->
  "COMPOUNDCURVE", +(ws),
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
  "(", +(ws), seplist(ring_text(ZM), +(ws), L), +(ws), ")".


curvepolygon_text_body(ZM, L) -->
  curvepolygon_text(ZM, L).


curvepolygon_text_representation(ZM1, curvepolygon(L)) -->
  "CURVEPOLYGON", +(ws),
  z_m(ZM1, ZM2),
  curvepolygon_text_body(ZM2, L), !.
curvepolygon_text_representation(ZM, L) -->
  polygon_text_representation(ZM, L), !.
curvepolygon_text_representation(ZM, L) -->
  triangle_text_representation(ZM, L).



geometrycollection_text(_, L) -->
  empty_set(L), !.
geometrycollection_text(ZM, L) -->
  seplist(wkt_representation(ZM), +(ws), L), +(ws), ")".


geometrycollection_text_representation(ZM1, geometrycollection(L)) -->
  "GEOMETRYCOLLECTION", +(ws),
  z_m(ZM1, ZM2),
  geometrycollection_text(ZM2, L).



linestring_text(_, L) -->
  empty_set(L), !.
linestring_text(ZM, L) -->
  "(", +(ws), seplist(point(ZM), +(ws), L), +(ws), ")".


linestring_text_body(ZM, L) -->
  linestring_text(ZM, L).


linestring_text_representation(ZM1, linestring(L)) -->
  "LINESTRING", +(ws),
  z_m(ZM1, ZM2),
  linestring_text_body(ZM2, L).



multicurve_text(_, L) -->
  empty_set(L), !.
multicurve_text(ZM, L) -->
  "(", +(ws), seplist(curve_text(ZM), +(ws), L), +(ws), ")".


multicurve_text_representation(ZM1, multicurve(L)) -->
  "MULTICURVE", +(ws),
  z_m(ZM1, ZM2),
  multicurve_text(ZM2, L), !.
multicurve_text_representation(ZM, L) -->
  multilinestring_text_representation(ZM, L).



multilinestring_text(_, L) -->
  empty_set(L), !.
multilinestring_text(ZM, L) -->
  "(", +(ws), seplist('linestring text body'(ZM), +(ws), L), +(ws), ")".


multilinestring_text_representation(ZM1, multilinestring(L)) -->
  "MULTILINESTRING", +(ws),
  z_m(ZM1, ZM2),
  multilinestring_text(ZM2, L).



multipoint_text(_, L) -->
  empty_set(L), !.
multipoint_text(ZM, L) -->
  "(", +(ws), seplist(point_text(ZM), +(ws), L), +(ws), ")".


multipoint_text_representation(ZM1, multipoint(L)) -->
  "MULTIPOINT", +(ws),
  z_m(ZM1, ZM2),
  multipoint_text(ZM2, L).



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
multisurface_text_representation(ZM, L) -->
  multipolygon_text_representation(ZM, L), !.
multisurface_text_representation(ZM, L) -->
  polyhedralsurface_text_representation(ZM, L), !.
multisurface_text_representation(ZM, L) -->
  tin_text_representation(ZM, L).



point_text(_, L) -->
  empty_set(L), !.
point_text(ZM, L) -->
  "(", +(ws), point(ZM, L), +(ws), ")".


point_text_representation(ZM1, point(L)) -->
  "POINT", +(ws),
  z_m(ZM1, ZM2),
  point_text(ZM2, L).



polygon_text(_, L) -->
  empty_set(L), !.
polygon_text(ZM, L) -->
  "(", +(ws), seplist(linestring_text(ZM), +(ws), L), +(ws), ")".


polygon_text_body(ZM, L) -->
  polygon_text(ZM, L).


polygon_text_representation(ZM1, polygon(L)) -->
  "POLYGON", +(ws),
  z_m(ZM1, ZM2),
  polygon_text_body(ZM2, L).



polyhedralsurface_text(_, L) -->
  empty_set(L), !.
polyhedralsurface_text(ZM, L) -->
  "(", +(ws), seplist(polygon_text_body(ZM), +(ws), L), +(ws), ")".


polyhedralsurface_text_representation(ZM1, polyhedralsurface(L)) -->
  "POLYHEDRALSURFACE", +(ws),
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
  "(", +(ws), seplist(triangle_text_body(ZM), +(ws), L), +(ws), ")".


tin_text_representation(ZM1, tin(L)) -->
  "TIN", +(ws),
  z_m(ZM1, ZM2),
  tin_text(ZM2, L).



triangle_text(_, L) -->
  empty_set(L), !.
triangle_text(ZM, L) -->
  "(", +(ws), linestring_text(ZM, L), +(ws), ")".


triangle_text_body(ZM, L) -->
  triangle_text(ZM, L).


triangle_text_representation(ZM1, triangle(L)) -->
  "TRIANGLE", +(ws),
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


point(zm, [X,Y,Z,M]) --> point(z, [X,Y,Z]), +(ws), m(M), !.
point(z, [X,Y,Z]) --> point(none, [X,Y]), +(ws), z(Z), !.
point(m, [X,Y,M]) --> point(none, [X,Y]), +(ws), m(M), !.
point(_, [X,Y]) --> x(X), +(ws), y(Y).


x(N) --> number(N).


y(N) --> number(N).


z(N) --> number(N).


z_m(ZM, ZM) --> "".
z_m(_, zm) --> "ZM", +(ws).
z_m(_, z) --> "Z", +(ws).
z_m(_, m) --> "M", +(ws).
