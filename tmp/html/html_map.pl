:- module(
  html_map,
  [
    map//5,     % +QueryEndpoint
                % +BrowserEndpoint
                % ?CenterPoint
                % ?Zoom
                % ?FeatureCollection
    map_param/2 % ?Key, ?Spec
  ]
).

/** <module> HTML map

@author Wouter Beek
@version 2016/05, 2016/09, 2016/11
*/

:- use_module(library(debug)).
:- use_module(library(default)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_resource)).
:- use_module(library(http/js_write)).
:- use_module(library(settings)).

:- html_resource(
     css(map),
     [requires([css('leaflet-1.0.3.css')]),virtual(true)]
   ).

:- if(debugging(js(map))).
  :- html_resource(
       js(map),
       [requires([js('leaflet-1.0.3.js')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       js(map),
       [requires([js('leaflet-1.0.3.min.js')]),virtual(true)]
     ).
:- endif.

:- html_resource(map, [requires([css(map),js(map)]),virtual(true)]).

:- setting(
     default_center_lat,
     float,
     51.35,
     "The latitude of the default map center."
   ).
:- setting(
     default_center_lng,
     float,
     5.47,
     "The longitude of the default map center."
   ).
:- setting(
     default_zoom,
     between(1,20),
     15,
     "The default zoom level."
   ).

:- multifile
    http:http_param/3.

http:http_param(
  Mod,
  lat,
  [
    default(DefLat),
    description("The latitude of the default center of the map."),
    float
  ]
) :-
  setting(Mod:default_center_lat, DefLat).
http:http_param(
  Mod,
  lng,
  [
    default(DefLng),
    description("The longitude of the default center of the map."),
    float
  ]
) :-
  setting(Mod:default_center_lng, DefLng).
http:http_param(
  Mod,
  zoom,
  [
    between(1, 20),
    default(DefZoom),
    description("The default zoom level of the map.")
  ]
) :-
  setting(Mod:default_zoom, DefZoom).





%! map(
%!   +QueryEndpoint,
%!   +BrowserEndpoint,
%!   ?CenterPoint,
%!   ?Zoom,
%!   ?FeatureCollection
%! )// is det.

map(
  QueryEndpoint,
  BrowserEndpoint,
  point(Lng,Lat),
  Zoom,
  FeatureCollection
) -->
  {
    defsetting(default_center_lng, Lng),
    defsetting(default_center_lat, Lat),
    defsetting(default_zoom, Zoom),
    % @note The order for the central point in Leaflet is flipped.
    CenterPoint = [Lat,Lng],
    defval(_{features: [], type: "FeatureCollection"}, FeatureCollection)
  },
  html(div(id=mapid, [])),
  js_script({|javascript(
    QueryEndpoint,
    BrowserEndpoint,
    CenterPoint,
    Zoom,
    FeatureCollection
  )||
window.onload = function() {
  var x = $("#mapid");
  var map = L.map("mapid").setView(CenterPoint, Zoom);
  var OpenStreetMap_Mapnik =
    L.tileLayer(
      'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
      {maxZoom: 19}
    ).addTo(map);
  function onMapClick(e) {
    $.ajax({
      "accepts": {"json": "application/vnd.geo+json"},
      "data": {
        "graph": $("#graph-menu").find(":selected").val(),
        "lng": e.latlng.lng,
        "lat": e.latlng.lat
      },
      "dataType": "json",
      "success": function(data) {
        var layer = L.geoJson(data.features, {
          "onEachFeature": function (feature, layer) {
            // This part populates the popup content with the info from GeoJSON.
            if (feature.properties) {
              if (feature.properties.popupContent) {
                layer.bindPopup(feature.properties.popupContent, {"maxWidth": 1000});
              } else if (feature.properties['@id']) {
                // No popup content.  Just show a ‘more info’ link.
                var link = BrowserEndpoint + '?subject=' + encodeURIComponent(feature.properties['@id']);
                layer.bindPopup('<a href="' + link + '" target="_self">Link to graph</a>');
              }
            }
            // Avoid event bubbling that executes another map click.
            layer.on("click", function(levent) {
              L.DomEvent.stopPropagation(levent);
            })
          },
          "style": function(feature) {
            if (feature.properties.color) {
              return {
                color: feature.properties.color,
                fillColor: feature.properties.color
              };
            }
          }
        }).addTo(map);

        // Animate markers a bit to distinguish them when zoomed out.
        layer.eachLayer(function(marker) {
          L.setOptions(marker, {riseOnHover: true});
        });
        map.fitBounds(layer.getBounds().pad(0.5), {animate:true});
      },
      "url": QueryEndpoint
    });
  }
  map.on('click', onMapClick);
  var layer = L.geoJson().addTo(map);
  layer.addData(FeatureCollection);
}
  |}).



%! map_param(?Key, ?Spec) is nondet.

map_param(Key, Spec) :-
  http_param(Key),
  http:http_param(html_map, Key, Spec).


http_param(lat).
http_param(lng).
http_param(zoom).
