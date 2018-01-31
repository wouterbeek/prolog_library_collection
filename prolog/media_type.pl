:- encoding(utf8).
:- module(
  media_type,
  [
    extension_label/2,      % ?Extension, ?Label
    media_type//1,          % +MediaType
    media_type/1,           % ?MediaType
    media_type_comps/4,     % ?MediaType, ?Supertype, ?Subtype, ?Params
    media_type_encoding/2,  % ?MediaType, ?Encoding
    media_type_extension/2, % ?MediaType, ?Extension
    media_type_label/2,     % ?MediaType, ?Label
    media_type_parameter/3, % +MediaType, ?Key, ?Value
    media_type_program/3    % ?MediaType, ?Program, -Args
  ]
).

/** <module> Media Types

Design goal: maintain a one-to-one mapping between Media Types and
file name extensions.

@author Wouter Beek
@version 2017/12-2018/01
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(stream_ext)).





%! extension_label(+Extension:atom, -Label:string) is det.
%! extension_label(-Extension:atom, -Label:string) is multi.

extension_label(Ext, Label) :-
  call_det_when(ground(Ext), media_type_(Ext, _, _, Label)).



%! media_type(+MediaType:compound)// is det.

media_type(MediaType) -->
  {ground(MediaType)}, !,
  media_type_generate(MediaType).
media_type(MediaType) -->
  media_type_parse(MediaType).

media_type_generate(media(Super/Sub,Params)) -->
  atom(Super),
  "/",
  atom(Sub),
  params_generate(Params).

params_generate([]) --> !, "".
params_generate([H|T]) -->
  ";",
  param_generate(H),
  params_generate(T).

param_generate(Key=Value) -->
  {format(atom(Atom), "~a=~w", [Key,Value])},
  atom(Atom).

media_type_parse(media(Super/Sub,Params)) -->
  ...(SuperCodes),
  "/",
  ...(SubCodes),
  (";" -> whites, params_parse(Params) ; eos),
  {maplist(atom_codes, [Super,Sub], [SuperCodes,SubCodes])}.

params_parse([H|T]) -->
  param_parse(H), !,
  params_parse(T).
params_parse([]) --> "".

param_parse(Key-Value) -->
  ...(KeyCodes),
  "=",
  ...(ValueCodes),
  ";",
  {maplist(atom_codes, [Key,Value], [KeyCodes,ValueCodes])},
  whites.



%! media_type(+MediaType:compound) is semidet.
%! media_type(-MediaType:compound) is multi.

media_type(MediaType) :-
  media_type_(_, MediaType, _, _).



%! media_type_comps(?MediaType:compound, ?Supertype:atom, ?Subtype:atom,
%!                  ?Params:list(compound)) is det.

media_type_comps(media(Supertype/Subtype,Params), Supertype, Subtype, Params).



%! media_type_encoding(+MediaType:compound, +Encoding:atom) is det.
%! media_type_encoding(+MediaType:compound, -Encoding:atom) is det.

% A parameter `charset'.
% TBD: Are values to the `charset' parameter case-insensitive?
media_type_encoding(MediaType, Encoding) :-
  media_type_parameter(MediaType, charset, Encoding), !.
% TBD: Integrate this with media_type_/5.
media_type_encoding(media(application/json,_), utf8).
media_type_encoding(media(application/'n-quads',_), utf8).
media_type_encoding(media(application/'n-triples',_), utf8).
media_type_encoding(media(application/'sparql-query',_), utf8).
media_type_encoding(media(application/'x-prolog',_), utf8).
media_type_encoding(media(image/jpeg,_), octet).
media_type_encoding(media(image/png,_), octet).
media_type_encoding(media(text/turtle,_), utf8).



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



%! media_type_parameter(+MediaType:compound, +Key:atom, +Value:atom) is semidet.
%! media_type_parameter(+MediaType:compound, +Key:atom, -Value:atom) is semidet.
%! media_type_parameter(+MediaType:compound, -Key:atom, -Value:atom) is nondet.

media_type_parameter(media(_/_,Parameters), Key, Value) :-
  member(Key=Value, Parameters).



%! media_type_program(+MediaType:compound, +Program:atom, -Args:list) is nondet.
%! media_type_program(+MediaType:compound, -Program:atom, -Args:list) is nondet.
%! media_type_program(-MediaType:compound, +Program:atom, -Args:list) is nondet.
%! media_type_program(-MediaType:compound, -Program:atom, -Args:list) is multi.

media_type_program(MediaType, Program, Args) :-
  media_type_(_, MediaType, Programs, _),
  member(Program0, Programs),
  (Program0 = program(Program,Args) ; Program = Program0).





% GENERICS %

media_type_(atom, media(application/'atom+x',[]), [gedit], "Atom XML").
media_type_(bmp, media(image/bmp,[]), [eog], "Windows Bitmap (BMP)").
media_type_(bz2, media(application/'x-bzip2',[]), [program(bzip2,['-d'])], "bzip2").
media_type_(csv, media(text/csv,[]), [gedit], "Comma-separated values (CSV)").
media_type_(doc, media(application/msword,[]), [program(libreoffice,'--writer')], "Microsoft Word Document").
media_type_(docm, media(application/'vnd.ms-word.document.macroenabled.12',[]), [program(libreoffice,'--writer')], "Microsoft Word Document").
media_type_(docx, media(application/'vnd.openxmlformats-officedocument.wordprocessingml.document',[]), [program(libreoffice,'--writer')], "OpenOffice Wordprocessing Document").
media_type_(dot, media(text/'vnd.graphviz',[]), [gedit], "GraphViz DOT").
media_type_(dotm, media(application/'vnd.ms-word.template.macroenabled.12',[]), [program(libreoffice,'--writer')], "Microsoft Word Template").
media_type_(dotx, media(application/'vnd.openxmlformats-officedocument.wordprocessingml.template',[]), [program(libreoffice,'--writer')], "OpenOffice Wordprocessing Template").
media_type_(epub, media(application/'epub+zip',[]), [], "ePub").
media_type_(geojson, media(application/'vnd.geo+json',[]), [gedit], "GeoJSON").
media_type_(gif, media(image/gif,[]), [eog], "Graphics Interchange Format (GIF)").
media_type_(gml, media(application/'gml+xml',[]), [gedit], "GML").
media_type_(gz, media(application/gzip,[]), [], "GNU Zip").
media_type_(hdt, media(application/'vnd.hdt',[]), [], "Header Dictionary Triples (HDT)").
media_type_(html, media(text/html,[]), [firefox], "Hyper Text Markup Language (HTML)").
% Microsoft uses Media Type ‘image/x-icon’.
media_type_(ico, media(image/'vnd.microsoft.icon',[]), [eog], "Windows Icon").
media_type_(jgf, media(application/'vnd.jgf+json',[]), [gedit], "JSON Graph Format (JGF)").
media_type_(jpeg, media(image/jpeg,[]), [eog], "Joint Photographic Experts Group (JPEG)").
media_type_(js, media(application/javascript,[]), [gedit], "JavaScript (JS)").
media_type_(json, media(application/json,[]), [gedit], "JavaScript Object Notation (JSON)").
media_type_(jsonld, media(application/'ld+json',[]), [gedit], "JSON-LD 1.0").
media_type_(jsp, media(application/jsp,[]), [], "Java Server Pages (JSP)").
media_type_(gml, media(text/'x-gml',[]), [gephi], "Graph Markup Language (GML)").
media_type_(kml, media(application/'vnd.google-earth.kml+xml',[]), [], "KML").
media_type_(kmz, media(application/'vnd.google-earth.kmz',[]), [], "KMZ").
media_type_(mdb, media(application/'vnd.ms-access',[]), [program(libreoffice,['--base'])], "Microsoft Access Database").
media_type_(mobi, media(application/'vnd.amazon.mobi8-ebook',[]), [], "Mobi").
media_type_(n3, media(text/n3,[]), [gedit], "Notation 3 (N3)").
media_type_(nq, media(application/'n-quads',[]), [gedit], "N-Quads 1.1").
media_type_(nt, media(application/'n-triples',[]), [gedit], "N-Triples 1.1").
media_type_(ods, media(application/'vnd.oasis.opendocument.spreadsheet',[]), [program(libreoffice,['--calc'])], "OpenDocument Spreadsheet").
media_type_(odt, media(application/'vnd.oasis.opendocument.text',[]), [program(libreoffice,['--writer'])], "OpenDocument Text").
media_type_(pbm, media(image/'x-portable-bitmap',[]), [], "Portable Bitmap Format (PBM)").
% Native file format of PC Paintbrush.
media_type_(pcx, media(image/'vnd.zbrush.pcx',[]), [eog], "PiCture EXchange").
media_type_(pdf, media(application/pdf,[]), [evince,xpdf], "Portable Document Format (PDF)").
media_type_(pgm, media(image/'x-portable-graymap',[]), [], "Portable Graymap Format (PGM)").
media_type_(pl, media(application/'x-prolog',[]), [gedit], "Prolog").
media_type_(png, media(image/png,[]), [eog], "Portable Network Graphics (PNG)").
media_type_(pnm, media(image/'x-portable-anymap',[]), [eog], "Portable Anymap Format (PNM)").
media_type_(pot, media(application/'vnd.ms-powerpoint',[]), [program(libreoffice,['--impress'])], "Microsoft PowerPoint").
media_type_(potm, media(application/'vnd.ms-powerpoint.template.macroenabled.12',[]), [program(libreoffice,['--impress'])], "Microsoft PowerPoint Template").
media_type_(potx, media(application/'vnd.openxmlformats-officedocument.presentationml.template',[]), [program(libreoffice,['--impress'])], "OpenOffice Presentation Template").
media_type_(ppa, media(application/'vnd.ms-powerpoint',[]), [program(libreoffice,['--impress'])], "Microsoft PowerPoint").
media_type_(ppam, media(application/'vnd.ms-powerpoint.addin.macroenabled.12',[]), [program(libreoffice,['--impress'])], "Microsoft PowerPoint Add-in").
media_type_(ppm, media(image/'x-portable-pixmap',[]), [], "Portable Pixmap Format (PPM)").
media_type_(pps, media(application/'vnd.ms-powerpoint',[]), [program(libreoffice,['--impress'])], "Microsoft PowerPoint").
media_type_(ppsm, media(application/'vnd.ms-powerpoint.slideshow.macroenabled.12',[]), [program(libreoffice,['--impress'])], "Microsoft PowerPoint Slideshow").
media_type_(ppsx, media(application/'vnd.openxmlformats-officedocument.presentationml.slideshow',[]), [program(libreoffice,['--impress'])], "OpenOffice Presentation Slideshow").
media_type_(ppt, media(application/'vnd.ms-powerpoint',[]), [program(libreoffice,['--impress'])], "Microsoft PowerPoint").
media_type_(pptm, media(application/'vnd.ms-powerpoint.presentation.macroenabled.12',[]), [program(libreoffice,['--impress'])], "Microsoft PowerPoint Presentation").
media_type_(pptx, media(application/'vnd.openxmlformats-officedocument.presentationml.presentation',[]), [program(libreoffice,['--impress'])], "OpenOffice Presentation").
media_type_(ps, media(application/postscript,[]), [evince,xpdf], "PostScript (PS)").
media_type_(rar, media(application/'vnd.rar',[]), [], "Roshal Archive (RAR)").
media_type_(rdf, media(application/'rdf+xml',[]), [gedit], "RDF/XML 1.1").
media_type_(rq, media(application/'sparql-query',[]), [gedit], "SPARQL 1.1 Query").
media_type_(rss, media(application/'rss+xml',[]), [gedit], "Rich Site Summary (RSS)").
media_type_(rtf, media(application/rtf,[]), [], "Rich Text Format (RTF)").
media_type_(ru, media(application/'sparql-update',[]), [gedit], "SPARQL 1.1 Update").
media_type_(svg, media(image/'svg+xml',[]), [firefox,eog], "Scalable Vector Graphics (SVG)").
media_type_(tga, media(image/'x-targa',[]), [eog], "Truevision Advanced Raster Graphics Adapter (TARGA)").
media_type_(tiff, media(image/tiff,[]), [eog], "Tagged Image File Format (TIFF)").
media_type_(torrent, media(application/'x-bittorrent',[]), ['transmission-gtk'], "BitTorrent").
media_type_(trig, media(application/trig,[]), [gedit], "TriG 1.1").
media_type_(tsv, media(text/'tab-separated-values',[]), [gedit], "Tag-separated values (TSV)").
media_type_(ttl, media(text/turtle,[]), [gedit], "Turtle 1.1").
media_type_(wbmp, media(image/'vnd.wap.bmp',[]), [eog], "Wireless Application Protocol Bitmap Format (Wireless Bitmap)").
media_type_(xbm, media(image/'x-bitmap',[]), [eog], "X BitMap (XBM)").
media_type_(xhtml, media(application/'xhtml+xml',[]), [gedit], "XHTML").
media_type_(xla, media(application/'vnd.ms-excel',[]), [program(libreoffice,['--calc'])], "Microsoft Excel").
media_type_(xlam, media(application/'vnd.ms-excel.addin.macroenabled.12',[]), [program(libreoffice,['--calc'])], "Microsoft Excel Add-in").
media_type_(xls, media(application/'vnd.ms-excel',[]), [program(libreoffice,['--calc'])], "Microsoft Excel").
media_type_(xlsb, media(application/'vnd.ms-excel.sheet.binary.macroenabled.12',[]), [program(libreoffice,['--calc'])], "Microsoft Excel Spreadsheet").
media_type_(xlsm, media(application/'vnd.ms-excel.sheet.macroenabled.12',[]), [program(libreoffice,['--calc'])], "Microsoft Excel Spreadsheet").
media_type_(xlsx, media(application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet',[]), [program(libreoffice,['--calc'])], "OpenOffice Spreadsheet").
media_type_(xlt, media(application/'vnd.ms-excel',[]), [program(libreoffice,['--calc'])], "Microsoft Excel").
media_type_(xltm, media(application/'vnd.ms-excel.template.macroenabled.12',[]), [program(libreoffice,['--calc'])], "Microsoft Excel Template").
media_type_(xltx, media(application/'vnd.openxmlformats-officedocument.spreadsheetml.template',[]), [program(libreoffice,['--calc'])], "OpenOffice Spreadsheet Template").
media_type_(xml, media(text/xml,[]), [gedit], "Extended Markup Language (XML)").
media_type_(xpm, media(image/'x-xpixmap',[]), [eog], "X PixMap (XPM)").
media_type_(webp, media(image/webp,[]), [], "WebP").
media_type_(zip, media(application/zip,[]), [], "ZIP").

no_media_type_(ras, _, [eog], "Sun Raster").
no_media_type_(trix, _, [gedit], "Triples in XML (TriX)").
no_media_type_(yml, _, [gedit], "YAML Ain't Markup Language").
