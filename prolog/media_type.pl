:- module(
  media_type,
  [
    extension_label/2,      % ?Extension, ?Label
    media_type//1,          % +MediaType
    media_type_comps/4,     % ?MediaType, ?Supertype, ?Subtype, ?Params
    media_type_extension/2, % ?MediaType, ?Extension
    media_type_label/2,     % ?MediaType, ?Label
    media_type_program/2    % ?MediaType, ?Program
  ]
).

/** <module> Media Types

Design goal: maintain a one-to-one mapping between Media Types and
file name extensions.

@author Wouter Beek
@version 2017/12
*/

:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(error)).





%! extension_label(+Extension:atom, -Label:string) is det.
%! extension_label(-Extension:atom, -Label:string) is multi.

extension_label(Ext, Label) :-
  call_det_when(ground(Ext), media_type_(Ext, _, _, Label)).



%! media_type(+MediaType:compound)// is det.

media_type(media(Super/Sub,Params)) -->
  atom(Super),
  "/",
  atom(Sub),
  params_(Params).

params_([]) --> !, "".
params_([H|T]) -->
  ";",
  param_(H),
  params_(T).

param_(Param) -->
  {
    Param =.. [Key,Value],
    format(atom(Atom), "~a(~w)", [Key,Value])
  },
  atom(Atom).



%! media_type_comps(?MediaType:compound, ?Supertype:atom, ?Subtype:atom,
%!                  ?Params:list(compound)) is det.

media_type_comps(media(Supertype/Subtype,Params), Supertype, Subtype, Params).



%! media_type_extension(+MediaType:compound, +Extension:atom) is semidet.
%! media_type_extension(+MediaType:compound, -Extension:atom) is det.
%! media_type_extension(-MediaType:compound, +Extension:atom) is det.
%! media_type_extension(-MediaType:compound, -Extension:atom) is multi.

media_type_extension(MediaType, Ext) :-
  call_det_when(
    (ground(MediaType) ; ground(Ext)),
    media_type_(Ext, MediaType, _, _)
  ).



%! media_type_label(+MediaType:compound, -Label:string) is det.
%! media_type_label(-MediaType:compound, -Label:string) is multi.

media_type_label(MediaType, Label) :-
  call_det_when_ground(MediaType, media_type_(_, MediaType, _, Label)).



%! media_type_program(+MediaType:compound, +Program:atom) is nondet.
%! media_type_program(+MediaType:compound, -Program:atom) is nondet.
%! media_type_program(-MediaType:compound, +Program:atom) is nondet.
%! media_type_program(-MediaType:compound, -Program:atom) is multi.

media_type_program(MediaType, Program) :-
  media_type_(_, MediaType, Programs, _),
  member(Program, Programs).





% GENERICS %

media_type_(bmp, media(image/bmp,[]), [eog], "Windows Bitmap (BMP)").
media_type_(csv, media(text/csv,[]), [gedit], "Comma-separated values (CSV)").
media_type_(dot, media(text/'vnd.graphviz',[]), [gedit], "GraphViz DOT").
media_type_(geojson, media(application/'vnd.geo+json',[]), [gedit], "GeoJSON").
media_type_(gif, media(image/gif,[]), [eog], "Graphics Interchange Format (GIF)").
media_type_(hdt, media(application/'vnd.hdt',[]), [], "Header Dictionary Triples (HDT)").
media_type_(html, media(text/html,[]), [firefox], "Hyper Text Markup Language (HTML)").
% Microsoft uses Media Type ‘image/x-icon’.
media_type_(ico, media(image/'vnd.microsoft.icon',[]), [eog], "Windows Icon").
media_type_(jgf, media(application/'vnd.jgf+json',[]), [gedit], "JSON Graph Format (JGF)").
media_type_(jpeg, media(image/jpeg,[]), [eog], "Joint Photographic Experts Group (JPEG)").
media_type_(json, media(application/json,[]), [gedit], "JavaScript Object Notation (JSON)").
media_type_(jsonld, media(application/'ld+json',[]), [gedit], "JSON-LD 1.0").
media_type_(gml, media(text/'x-gml',[]), [gephi], "Graph Markup Language (GML)").
media_type_(nq, media(application/'n-quads',[]), [gedit], "N-Quads 1.1").
media_type_(nt, media(application/'n-triples',[]), [gedit], "N-Triples 1.1").
media_type_(pbm, media(image/'x-portable-bitmap',[]), [], "Portable Bitmap Format (PBM)").
% Native file format of PC Paintbrush.
media_type_(pcx, media(image/'vnd.zbrush.pcx',[]), [eog], "PiCture EXchange").
media_type_(pdf, media(application/pdf,[]), [evince,xpdf], "Portable Document Format (PDF)").
media_type_(pgm, media(image/'x-portable-graymap',[]), [], "Portable Graymap Format (PGM)").
media_type_(pl, media(application/'x-prolog',[]), [gedit], "Prolog").
media_type_(png, media(image/png,[]), [eog], "Portable Network Graphics (PNG)").
media_type_(pnm, media(image/'x-portable-anymap',[]), [eog], "Portable Anymap Format (PNM)").
media_type_(ppm, media(image/'x-portable-pixmap',[]), [], "Portable Pixmap Format (PPM)").
media_type_(ps, media(application/postscript,[]), [evince,xpdf], "PostScript (PS)").
media_type_(rdf, media(application/'rdf+xml',[]), [gedit], "RDF/XML 1.1").
media_type_(rq, media(application/'sparql-query',[]), [gedit], "SPARQL 1.1 Query").
media_type_(ru, media(application/'sparql-update',[]), [gedit], "SPARQL 1.1 Update").
media_type_(svg, media(image/'svg+xml',[]), [firefox,eog], "Scalable Vector Graphics (SVG)").
media_type_(tga, media(image/'x-targa',[]), [eog], "Truevision Advanced Raster Graphics Adapter (TARGA)").
media_type_(tiff, media(image/tiff,[]), [eog], "Tagged Image File Format (TIFF)").
media_type_(trig, media(application/trig,[]), [gedit], "TriG 1.1").
media_type_(tsv, media(text/'tab-separated-values',[]), [gedit], "Tag-separated values (TSV)").
media_type_(ttl, media(text/turtle,[]), [gedit], "Turtle 1.1").
media_type_(wbmp, media(image/'vnd.wap.bmp',[]), [eog], "Wireless Application Protocol Bitmap Format (Wireless Bitmap)").
media_type_(xbm, media(image/'x-bitmap',[]), [eog], "X BitMap (XBM)").
media_type_(xhtml, media(application/'xhtml+xml',[]), [gedit], "XHTML").
media_type_(xpm, media(image/'x-xpixmap',[]), [eog], "X PixMap (XPM)").
media_type_(webp, media(image/webp,[]), [], "WebP").

no_media_type_(ras, _, [eog], "Sun Raster").
