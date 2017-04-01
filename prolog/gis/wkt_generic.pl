:- module(
  wkt_generic,
  [
    wkt_shape_name/1 % ?Name
  ]
).

/** <module> WKT generic

@author Wouter Beek
@version 2017/04
*/





%! wkt_shape_name(+Name) is semidet.
%! wkt_shape_name(-Name) is multi.

wkt_shape_name('CircularString').
wkt_shape_name('CompoundCurve').
wkt_shape_name('CurvePolygon').
wkt_shape_name('GeometryCollection').
wkt_shape_name('LineString').
wkt_shape_name('MultiCurve').
wkt_shape_name('MultiLineString').
wkt_shape_name('MultiPoint').
wkt_shape_name('MultiPolygon').
wkt_shape_name('MultiSurface').
wkt_shape_name('Point').
wkt_shape_name('Polygon').
wkt_shape_name('PolyhedralSurface').
wkt_shape_name('TIN').
wkt_shape_name('Triangle').
