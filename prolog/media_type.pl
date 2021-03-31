:- encoding(utf8).
:- module(
  media_type,
  [
    media_type//1,          % ?MediaType
    media_type/1,           % ?MediaType
    media_type_encoding/2,  % ?MediaType, ?Encoding
    media_type_extension/2, % ?MediaType, ?Extension
    media_type_family/2,    % ?MediaType, ?Family
    media_type_label/2,     % ?MediaType, ?Label
    media_type_parameter/2, % +MediaType, ?Parameter
    media_type_program/3,   % ?MediaType, -Program, -Args
    must_be_media_type/2    % +MediaTypes, +MediaType
  ]
).

/** <module> Media Types library

Library of Media Types, based on the official Media Types as
maintained by IANA, and de facto Media Types as they are used in the
wild.

Media Types are a common way of denoting binary and textual content.
As such, this library is useful to base sensible defaults for
processing input files on, and can be used to simplify HTTP requests
and aspects of serving content over HTTP.

Also tries to maintain a one-to-one mapping between Media Types and
the most commonly used file name extension for files containing
content in that Media Type.

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(pair_ext), []).
:- use_module(library(settings)).

:- use_module(library(call_ext)).
:- use_module(library(dcg)).
:- use_module(library(stream_ext)).

:- discontiguous
    encoding_/2,
    extension_/2,
    family_/2,
    label_/2,
    media_type_/2,
    program_/2.

:- multifile
    error:has_type/2.

error:has_type(media_type, MediaType) :-
  MediaType = media(Super/Sub,Params),
  error:has_type(atom, Super),
  error:has_type(atom, Sub),
  error:has_type(list(pair(atom)), Params).

:- setting(default_text_editor, atom, emacs,
           "The default program that is used for opening text files.").





%! extension_label(+Extension:atom, -Label:string) is det.
%! extension_label(-Extension:atom, -Label:string) is multi.

extension_label(Ext, Label) :-
  call_det_when(
    ground(Ext),
    (extension_(Key, Ext),
     label_(Key, Label))).



%! media_type(+MediaType:media_type)// is det.
%! media_type(-MediaType:media_type)// is det.

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

param_generate(Key-Value) -->
  atom(Key),
  "=",
  atom(Value).

media_type_parse(media(Super/Sub,Params)) -->
  ...(SuperCodes),
  "/",
  ...(SubCodes),
  (";" -> whites, params_parse(Params) ; eos, {Params = []}), !,
  {maplist(atom_codes, [Super,Sub], [SuperCodes,SubCodes])}.

params_parse([H|T]) -->
  param_parse(H), !,
  params_parse(T).
params_parse([]) --> "".

param_parse(Key-Value) -->
  ...(KeyCodes),
  "=",
  ...(ValueCodes),
  (";" -> whites ; eos),
  {maplist(atom_codes, [Key,Value], [KeyCodes,ValueCodes])}.



%! media_type(+MediaType:media_type) is semidet.
%! media_type(-MediaType:media_type) is multi.

media_type(MediaType) :-
  media_type_(_, MediaType).



%! media_type_encoding(+MediaType:media_type, +Encoding:atom) is semidet.
%! media_type_encoding(+MediaType:media_type, -Encoding:atom) is semidet.

% A parameter `charset'.
media_type_encoding(MediaType, Encoding) :-
  media_type_parameter(MediaType, charset-Encoding0), !,
  stream_ext:clean_encoding_(Encoding0, Encoding).
media_type_encoding(MediaType, Encoding) :-
  media_type_encoding_(MediaType, Encoding), !.

media_type_encoding_(media(text/turtle,_), utf8).



%! media_type_extension(+MediaType:media_type, +Extension:atom) is semidet.
%! media_type_extension(+MediaType:media_type, -Extension:atom) is det.
%! media_type_extension(-MediaType:media_type, +Extension:atom) is det.
%! media_type_extension(-MediaType:media_type, -Extension:atom) is multi.

media_type_extension(MediaType, Ext) :-
  call_det_when(
    (ground(MediaType) ; ground(Ext)),
    (extension_(Key, Ext),
     media_type_(Key, MediaType))).



%! media_type_family(+MediaType:media_type, +Family:term) is semidet.
%! media_type_family(+MediaType:media_type, -Family:term) is semidet.
%! media_type_family(-MediaType:media_type, +Family:term) is nondet.
%! media_type_family(-MediaType:media_type, -Family:term) is multi.

media_type_family(MediaType, Family) :-
  ground(Family), !,
  family_(Key, Family),
  media_type_(Key, MediaType).
media_type_family(MediaType, Family) :-
  media_type_(Key, MediaType),
  family_(Key, Family).


%! media_type_label(+MediaType:media_type, -Label:string) is det.
%! media_type_label(-MediaType:media_type, -Label:string) is multi.

media_type_label(MediaType, Label) :-
  call_det_when_ground(
    MediaType,
    (media_type_(Key, MediaType),
     label_(Key, Label))).



%! media_type_parameter(+MediaType:media_type, +Parameter:pair(atom)) is semidet.
%! media_type_parameter(+MediaType:media_type, -Parameter:pair(atom)) is nondet.

media_type_parameter(media(_/_,Params), Param) :-
  member(Param, Params).



%! media_type_program(+MediaType:media_type, -Program:atom, -Args:list) is nondet.
%! media_type_program(-MediaType:media_type, -Program:atom, -Args:list) is multi.

media_type_program(MediaType, Program, Args) :-
  media_type_(Key, MediaType),
  program_(Key, Program0),
  media_type_program_(Program0, Program, Args).

media_type_program_(program(Program,Args), Program, Args) :- !.
media_type_program_(text_editor, Program, []) :- !,
  setting(default_text_editor, Program).
media_type_program_(Program, Program, []).



%! must_be_media_type(+MediaTypes:list(media_type), +MediaType:media_type) is det.

must_be_media_type(MediaTypes, media(Super/Sub,_)) :-
  memberchk(media(Super/Sub,_), MediaTypes), !.
must_be_media_type(MediaTypes, MediaType) :-
  must_be(oneof(MediaTypes), MediaType).



% MEDIA TYPE REGISTRATIONS %

%! media_type_(?Extension:atom, ?MediaType:media_type, ?Programs:list(or([atom,compound])), ?Label:string) is nondet.

extension_('7z_1', '7z').
label_('7z_1', "7z").
media_type_('7z_1', media(application/'x-7z-compressed',[])).

extension_(atom_1, atom).
label_(atom_1, "Atom XML").
media_type_(atom_1, media(application/'atom+x',[])).
program_(atom_1, text_editor).

extension_(bmp_1, bmp).
label_(bmp_1, "Windows Bitmap (BMP)").
media_type_(bmp_1, media(image/bmp,[])).
program_(bmp_1, eog).

extension_(bz2_1, bz2).
label_(bz2_1, "bzip2").
media_type_(bz2_1, media(application/'x-bzip2',[])).
program_(bz2_1, program(bzip2,['-d'])).

extension_(cab_1, cab).
label_(cab_1, "Microsoft Cabinet").
media_type_(cab_1, media(application/'vnd.ms-cab-compressed',[])).

extension_(cdx_1, cdx).
label_(cdx_1, "CambridgeSoft ChemDraw").
media_type_(cdx_1, media(chemical/'x-cdx',[])).

extension_(cpio_1, cpio).
label_(cpio_1, "cpio").
media_type_(cpio_1, media(application/'x-cpio',[])).

extension_(csv_1, csv).
family_(csv_1, sparql(ask)).
family_(csv_1, sparql(select)).
label_(csv_1, "Comma-separated values (CSV)").
media_type_(csv_1, media(text/csv,[])).
program_(csv_1, text_editor).

extension_(doc_1, doc).
label_(doc_1, "Microsoft Word Document").
media_type_(doc_1, media(application/msword,[])).
program_(doc_1, [program(libreoffice,'--writer')]).

extension_(docm_1, docm).
label_(docm_1, "Microsoft Word Document").
media_type_(docm_1, media(application/'vnd.ms-word.document.macroenabled.12',[])).
program_(docm_1, program(libreoffice,'--writer')).

extension_(docx_1, docx).
label_(docx_1, "OpenOffice Wordprocessing Document").
media_type_(docx_1, media(application/'vnd.openxmlformats-officedocument.wordprocessingml.document',[])).
program_(docx_1, program(libreoffice,'--writer')).

extension_(dot_1, dot).
label_(dot_1, "GraphViz DOT").
media_type_(dot_1, media(text/'vnd.graphviz',[])).
program_(dot_1, text_editor).

extension_(dotm_1, dotm).
label_(dotm_1, "Microsoft Word Template").
media_type_(dotm_1, media(application/'vnd.ms-word.template.macroenabled.12',[])).
program_(dotm_1, program(libreoffice,'--writer')).

extension_(dotx_1, dotx).
label_(dotx_1, "OpenOffice Wordprocessing Template").
media_type_(dotx_1, media(application/'vnd.openxmlformats-officedocument.wordprocessingml.template',[])).
program_(dotx_1, program(libreoffice,'--writer')).

extension_(dwg_1, dwg).
label_(dwg_1, "Drawing (DWG) proprietary format used by CAD software").
media_type_(dwg_1, media(application/dwg,[])).

extension_(dxf_1, dxf).
label_(dxf_1, "AutoCAD Drawing Exchange Format (DXF)").
media_type_(dxf_1, media(image/'vnd.dxf',[])).

extension_(eps_1, eps).
label_(eps_1, "Encapsulated PostScript (EPS)").
media_type_(eps_1, media(image/eps,[])).
program_(eps_1, xfig).

extension_(epub_1, epub).
label_(epub_1, "ePub").
media_type_(epub_1, media(application/'epub+zip',[])).

extension_(exr_1, exr).
label_(exr_1, "OpenEXR is a high dynamic-range (HDR) image file format developed by Industrial Light & Magic for use in computer imaging applications").

extension_(fig_1, fig).
label_(fig_1, "FIG graphics language (Xfig)").
program_(fig_1, xfig).

extension_(fits_1, fits).
label_(fits_1, "Flexible Image Transport System (FITS)").
media_type_(fits_1, media(application/fits,[])).

extension_(flv_1, flv).
label_(flv_1, "Flash video (FLV)").
media_type_(flv_1, media(video/'x-flv',[])).

extension_(geojson_1, geojson).
label_(geojson_1, "GeoJSON").
media_type_(geojson_1, media(application/'vnd.geo+json',[])).
program_(geojson_1, text_editor).

extension_(gif_1, gif).
label_(gif_1, "Graphics Interchange Format (GIF)").
media_type_(gif_1, media(image/gif,[])).
program_(gif_1, eog).
program_(gif_1, xfig).

extension_(gml_1, gml).
label_(gml_1, "Geography Markup Language (GML)").
media_type_(gml_1, media(application/'gml+xml',[])).
program_(gml_1, text_editor).

extension_(gml_2, gml).
label_(gml_2, "Graph Markup Language (GML)").
media_type_(gml_2, media(text/'x-gml',[])).
program_(gml_2, gephi).

extension_(gpx_1, gpx).
label_(gpx_1, "GPS Exchange Format (GPX)").
media_type_(gpx_1, media(application/'gpx+xml',[])).

extension(gz_1, gz).
label_(gz_1, "GNU Zip").
media_type_(gz_1, media(application/gzip,[])).

extension_(hdt_1, hdt).
label_(hdt_1, "Header Dictionary Triples (HDT)").
media_type_(hdt_1, media(application/'vnd.hdt',[])).

extension_(html_1, html).
family_(html_1, rdf).
family_(html_1, rdfa).
label_(html_1, "Hyper Text Markup Language (HTML)").
media_type_(html_1, media(text/html,[])).
program_(html_1, firefox).

extension_(ico_1, ico).
label_(ico_1, "Windows Icon (Microsoft uses Media Type `image/x-icon')").
media_type_(ico_1, media(image/'vnd.microsoft.icon',[])).
program_(ico_1, eog).

extension_(jgf_1, jgf).
label_(jgf_1, "JSON Graph Format (JGF)").
media_type_(jgf_1, media(application/'vnd.jgf+json',[])).
program_(jgf_1, text_editor).

extension_(jp2_1, jp2).
label_(jp2_1, "JPEG 2000").
media_type_(jp2_1, media(image/jp2,[])).

encoding_(jpg_1, octet).
extension_(jpg_1, jpg).
label_(jpg_1, "Joint Photographic Experts Group (JPEG)").
media_type_(jpg_1, media(image/jpeg,[])).
program_(jpg_1, eog).
program_(jpg_1, xfig).

extension_(js_1, js).
label_(js_1, "JavaScript (JS)").
media_type_(js_1, media(application/javascript,[])).
program_(js_1, text_editor).

encoding_(json_1, utf8).
extension_(json_1, json).
label_(json_1, "JavaScript Object Notation (JSON)").
media_type_(json_1, media(application/json,[])).
program_(json_1, text_editor).

extension_(jsonld_1, jsonld).
family_(jsonld_1, rdf).
label_(jsonld_1, "JSON-LD 1.0").
media_type_(jsonld_1, media(application/'ld+json',[])).
program_(jsonld_1, text_editor).

extension_(jsp_1, jsp).
label_(jsp_1, "Java Server Pages (JSP)").
media_type_(jsp_1, media(application/jsp,[])).

extension_(kml_1, kml).
label_(kml_1, "KML").
media_type_(kml_1, media(application/'vnd.google-earth.kml+xml',[])).

extension_(kmz_1, kmz).
label_(kmz_1, "KMZ").
media_type_(kmz_1, media(application/'vnd.google-earth.kmz',[])).

extension_(lha_1, lha).
label_(lha_1, "LHA").
media_type_(lha_1, media(application/'x-lzh-compressed',[])).

extension_(mdb_1, mdb).
label_(mdb_1, "Microsoft Access Database").
media_type_(mdb_1, media(application/'vnd.ms-access',[])).
program_(mdb_1, program(libreoffice,['--base'])).

extension_(mobi_1, mobi).
label_(mobi_1, "Mobi").
media_type_(mobi_1, media(application/'vnd.amazon.mobi8-ebook',[])).

extension_(mol_1, mol).
label_(mol_1, "MDL Information Systems (MDL) Molfile").
media_type_(mol_1, media(chemical/'x-mdl-molfile',[])).

extension_(mp4_1, mp4).
label_(mp4_1, "MPEG-4 Part 14").
media_type_(mp4_1, media(video/mp4,[])).
program_(mp4_1, vlc).

extension_(n3_1, n3).
label_(n3_1, "Notation 3 (N3)").
media_type_(n3_1, media(text/n3,[])).
program_(n3_1, text_editor).

extension_(nc_1, nc).
label_(nc_1, "Network Common Data Form (NetCDF)").
media_type_(nc_1, media(application/netcdf,[])).

encoding_(nq_1, utf8).
extension_(nq_1, nq).
family_(nq_1, rdf).
label_(nq_1, "N-Quads 1.1").
media_type_(nq_1, media(application/'n-quads',[])).
program_(nq_1, text_editor).

encoding_(nt_1, utf8).
extension_(nt_1, nt).
family_(nt_1, rdf).
label_(nt_1, "N-Triples 1.1").
media_type_(nt_1, media(application/'n-triples',[])).
program_(nt_1, text_editor).

extension_(odp_1, odp).
label_(odp_1, "OpenDocument presenatation").
media_type_(odp_1, media(application/'vnd.oasis.opendocument.presentation',[])).
program_(odp_1, program(libreoffice)).

extension_(ods_1, ods).
label_(ods_1, "OpenDocument Spreadsheet").
media_type_(ods_1, media(application/'vnd.oasis.opendocument.spreadsheet',[])).
program_(ods_1, program(libreoffice,['--calc'])).

extension_(odt_1, odt).
label_(odt_1, "OpenDocument Text").
media_type_(odt_1, media(application/'vnd.oasis.opendocument.text',[])).
program_(odt_1, program(libreoffice,['--writer'])).

extension_(pbm_1, pbm).
label_(pbm_1, "Portable Bitmap Format (PBM)").
media_type_(pbm_1, media(image/'x-portable-bitmap',[])).

extension_(pct_1, pct).
label_(pct_1, "PICT is a graphics file format introduced on the original Apple Macintosh computer as its standard metafile format.").
media_type_(pct_1, media(image/'x-pict',[])).

extension_(pcx_1, pcx).
label_(pcx_1, "PiCture EXchange (PC Paintbrush)").
media_type_(pcx_1, media(image/'vnd.zbrush.pcx',[])).
program_(pcx_1, eog).
program_(pcx_1, xfig).

extension_(pdf_1, pdf).
label_(pdf_1, "Portable Document Format (PDF)").
media_type_(pdf_1, media(application/pdf,[])).
program_(pdf_1, evince).
program_(pdf_1, xpdf).

extension_(pgm_1, pgm).
label_(pgm_1, "Portable Graymap Format (PGM)").
media_type_(pgm_1, media(image/'x-portable-graymap',[])).

extension_(pic_1, pic).
label_(pic_1, "PIC language").

encoding_(pl_1, utf8).
extension_(pl_1, pl).
label_(pl_1, "Prolog").
media_type_(pl_1, media(application/'x-prolog',[])).
program_(pl_1, text_editor).

encoding_(png_1, octet).
extension_(png_1, png).
label_(png_1, "Portable Network Graphics (PNG)").
media_type_(png_1, media(image/png,[])).
program_(png_1, eog).
program_(png_1, xfig).

extension_(pnm_1, pnm).
label_(pnm_1, "Portable Anymap Format (PNM)").
media_type_(pnm_1, media(image/'x-portable-anymap',[])).
program_(pnm_1, eog).

extension_(pot_1, pot).
label_(pot_1, "Microsoft PowerPoint").
media_type_(pot_1, media(application/'vnd.ms-powerpoint',[])).
program_(pot_1, program(libreoffice,['--impress'])).

extension_(potm_1, potm).
label_(potm_1, "Microsoft PowerPoint Template").
media_type_(potm_1, media(application/'vnd.ms-powerpoint.template.macroenabled.12',[])).
program_(potm_1, program(libreoffice,['--impress'])).

extension_(potx_1, potx).
label_(potx_1, "OpenOffice Presentation Template").
media_type_(potx_1, media(application/'vnd.openxmlformats-officedocument.presentationml.template',[])).
program_(potx_1, program(libreoffice,['--impress'])).

extension_(pov_1, pov).
label_(pov_1, "Scene-description language for 3D modelling for the Persistence of Vision Raytracer.").

extension_(ppa_1, ppa).
label_(ppa_1, "Microsoft PowerPoint").
media_type_(ppa_1, media(application/'vnd.ms-powerpoint',[])).
program_(ppa_1, program(libreoffice,['--impress'])).

extension_(ppam_1, ppam).
label_(ppam_1, "Microsoft PowerPoint Add-in").
media_type_(ppam_1, media(application/'vnd.ms-powerpoint.addin.macroenabled.12',[])).
program_(ppam_1, program(libreoffice,['--impress'])).

extension_(ppm_1, ppm).
label_(ppm_1, "Portable Pixmap Format (PPM)").
media_type_(ppm_1, media(image/'x-portable-pixmap',[])).
program_(ppm_1, xfig).

extension_(pps_1, pps).
label_(pps_1, "Microsoft PowerPoint").
media_type_(pps_1, media(application/'vnd.ms-powerpoint',[])).
program_(pps_1, program(libreoffice,['--impress'])).

extension_(ppsm_1, ppsm).
label_(ppsm_1, "Microsoft PowerPoint Slideshow").
media_type_(ppsm_1, media(application/'vnd.ms-powerpoint.slideshow.macroenabled.12',[])).
program_(ppsm_1, program(libreoffice,['--impress'])).

extension_(ppsx_1, ppsx).
label_(ppsx_1, "OpenOffice Presentation Slideshow").
media_type_(ppsx_1, media(application/'vnd.openxmlformats-officedocument.presentationml.slideshow',[])).
program_(ppsx_1, program(libreoffice,['--impress'])).

extension_(ppt_1, ppt).
label_(ppt_1, "Microsoft PowerPoint").
media_type_(ppt_1, media(application/'vnd.ms-powerpoint',[])).
program_(ppt_1, program(libreoffice,['--impress'])).

extension_1(pptm_1, pptm).
label_(pptm_1, "Microsoft PowerPoint Presentation").
media_type_(pptm_1, media(application/'vnd.ms-powerpoint.presentation.macroenabled.12',[])).
program_(pptm_1, program(libreoffice,['--impress'])).

extension_(pptx_1, pptx).
label_(pptx_1, "OpenOffice Presentation").
media_type_(pptx_1, media(application/'vnd.openxmlformats-officedocument.presentationml.presentation',[])).
program_(pptx_1, program(libreoffice,['--impress'])).

extension_(ps_1, ps).
label_(ps_1, "PostScript (PS)").
media_type_(ps_1, media(application/postscript,[])).
program_(ps_1, evince).
program_(ps_1, xfig).
program_(ps_1, xpdf).

extension_(psd_1, psd).
label_(psd_1, "Adobe Photoshop Document (PSD)").
media_type_(psd_1, media(image/'image/vnd.adobe.photoshop',[])).

extension_(rar_1, rar).
label_(rar_1, "Roshal Archive (RAR)").
media_type_(rar_1, media(application/'vnd.rar',[])).

extension_(ras_1, ras).
label_(ras_1, "Sun Raster").
program_(ras_1, eog).

extension_(rdf_1, rdf).
family_(rdf_1, rdf).
label_(rdf_1, "RDF/XML 1.1").
media_type_(rdf_1, media(application/'rdf+xml',[])).
program_(rdf_1, text_editor).

encoding_(rq_1, utf8).
extension_(rq_1, rq).
label_(rq_1, "SPARQL 1.1 Query").
media_type_(rq_1, media(application/'sparql-query',[])).
program_(rq_1, text_editor).

extension_(rss_1, rss).
label_(rss_1, "Rich Site Summary (RSS)").
media_type_(rss_1, media(application/'rss+xml',[])).
program_(rss_1, text_editor).

extension_(rtf_1, rtf).
label_(rtf_1, "Rich Text Format (RTF)").
media_type_(rtf_1, media(application/rtf,[])).

extension_(ru_1, ru).
label_(ru_1, "SPARQL 1.1 Update").
media_type_(ru_1, media(application/'sparql-update',[])).
program_(ru_1, text_editor).

extension_(sgi_1, sgi).
label_(sgi_1, "Silicon Graphics Image (SGI)").
media_type_(sgi_1, media(image/sgi,[])).

encoding_(srj_1, utf8).
extension_(srj_1, srj).
family_(srj_1, sparql(ask)).
family_(srj_1, sparql(select)).
label_(srj_1, "SPARQL 1.1 Query Results JSON Format").
media_type_(srj_1, media(application/'sparql-results+json',[])).
program_(srj_1, text_editor).

extension_(srx_1, srx).
family_(srj_1, sparql(ask)).
family_(srj_1, sparql(select)).
label_(srx_1, "SPARQL Query Results XML Format").
media_type_(srx_1, media(application/'sparql-results+xml',[])).
program_1(srx_1, text_editor).

extension_(svg_1, svg).
label_(svg_1, "Scalable Vector Graphics (SVG)").
media_type_(svg_1, media(image/'svg+xml',[])).
program_(svg_1, eog).
program_(svg_1, firefox).

extension_(tar_1, tar).
media_type_(tar_1, media(application/'x-tar',[])).
label_(tar_1, "TAR").

extension_(tga_1, tga).
label_(tga_1, "Truevision Advanced Raster Graphics Adapter (TARGA)").
media_type_(tga_1, media(image/'x-targa',[])).
program_(tga_1, eog).

extension_(tiff_1, tiff).
label_(tiff_1, "Tagged Image File Format (TIFF)").
media_type_(tiff_1, media(image/tiff,[])).
program_(tiff_1, eog).
program_(tiff_1, xfig).

extension_(torrent_1, torrent).
label_(torrent_1, "BitTorrent").
media_type_(torrent_1, media(application/'x-bittorrent',[])).
program_(torrent_1, 'transmission-gtk').

encoding_(trig_1, utf8).
extension_(trig_1, trig).
label_(trig_1, "TriG 1.1").
media_type_(trig_1, media(application/trig,[])).
program_(trig_1, text_editor).

extension_(trix_1, trix).
label_(trix_1, "Triples in XML (TriX)").
program_(trix_1, text_editor).

extension_(tsv_1, tsv).
family_(tsv_1, sparql(ask)).
family_(tsv_1, sparql(select)).
label_(tsv_1, "Tag-separated values (TSV)").
media_type_(tsv_1, media(text/'tab-separated-values',[])).
program_(tsv_1, text_editor).

encoding_(ttl_1, utf8).
extension_(ttl_1, ttl).
label_(ttl_1, "Turtle 1.1").
media_type_(ttl_1, media(text/turtle,[])).
program_(ttl_1, text_editor).

extension_(wbmp_1, wbmp).
label_(wbmp_1, "Wireless Application Protocol Bitmap Format (Wireless Bitmap)").
media_type_(wbmp_1, media(image/'vnd.wap.bmp',[])).
program_(wbmp_1, eog).

extension_(xbm_1, xbm).
label_(xbm_1, "X BitMap (XBM)").
media_type_(xbm_1, media(image/'x-bitmap',[])).
program_(xbm_1, eog).
program_(xbm_1, xfig).

extension_(xhtml_1, xhtml).
family_(xhtml_1, rdf).
family_(xhtml_1, rdfa).
label_(xhtml_1, "XHTML").
media_type_(xhtml_1, media(application/'xhtml+xml',[])).
program_(xhtml_1, text_editor).

extension_(xla_1, xla).
label_(xla_1, "Microsoft Excel").
media_type_(xla_1, media(application/'vnd.ms-excel',[])).
program_(xla_1, program(libreoffice,['--calc'])).

extension_(xlam_1, xlam).
label_(xlam_1, "Microsoft Excel Add-in").
media_type_(xlam_1, media(application/'vnd.ms-excel.addin.macroenabled.12',[])).
program_(xlam_1, program(libreoffice,['--calc'])).

extension_(xls_1, xls).
label_(xls_1, "Microsoft Excel").
media_type_(xls_1, media(application/'vnd.ms-excel',[])).
program_(xls_1, program(libreoffice,['--calc'])).

extension_(xlsb_1, xlsb).
label_(xlsb_1, "Microsoft Excel Spreadsheet").
media_type_(xlsb_1, media(application/'vnd.ms-excel.sheet.binary.macroenabled.12',[])).
program_(xlsb_1, program(libreoffice,['--calc'])).

extension_(xlsm_1, xlsm).
label_(xlsm_1, "Microsoft Excel Spreadsheet").
media_type_(xlsm_1, media(application/'vnd.ms-excel.sheet.macroenabled.12',[])).
program_(xlsm_1, program(libreoffice,['--calc'])).

extension_(xlsx_1, xlsx).
label_(xlsx_1, "OpenOffice Spreadsheet").
media_type_(xlsx_1, media(application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet',[])).
program_(xlsx_1, program(libreoffice,['--calc'])).

extension_(xlt_1, xlt).
label_(xlt_1, "Microsoft Excel").
media_type_(xlt_1, media(application/'vnd.ms-excel',[])).
program_(xlt_1, program(libreoffice,['--calc'])).

extension_(xltm_1, xltm).
label_(xltm_1, "Microsoft Excel Template").
media_type_(xltm_1, media(application/'vnd.ms-excel.template.macroenabled.12',[])).
program_(xltm_1, program(libreoffice,['--calc'])).

extension_(xltx_1, xltx).
label_(xltx_1, "OpenOffice Spreadsheet Template").
media_type_(xltx_1, media(application/'vnd.openxmlformats-officedocument.spreadsheetml.template',[])).
program_(xltx_1, program(libreoffice,['--calc'])).

extension_(xml_1, xml).
label_(xml_1, "Extended Markup Language (XML)").
media_type_(xml_1, media(text/xml,[])).
program_(xml_1, text_editor).

extension_(xpm_1, xpm).
label_(xpm_1, "X PixMap (XPM)").
media_type_(xpm_1, media(image/'x-xpixmap',[])).
program_(xpm_1, eog).
program_(xpm_1, xfig).

extension_(xz_1, xz).
label_(xz_1, "xz").
media_type_(xz_1, media(application/'x-xz',[])).

extension_(yml_1, yml).
label_(yml_1, "YAML Ain't Markup Language (YAML)").
media_type_(yml_1, media(application/'x-yaml',[])).
program_(yml_1, text_editor).

extension_(vdx_1, vdx).
label_(vdx_1, "Microsoft Visio XML drawing").

extension_(vml_1, vml).
label_(vml_1, "Vector Markup Language (VML), part of Microsoft Open Office XML").
media_type_(vml_1, media(application/'vnd.openxmlformats-officedocument.vmlDrawing',[])).

extension_(vmlz_1, vmlz).
label_(vmlz_1, "GNU zipped VML").
media_type_(vmlz_1, media(application/'vnd.openxmlformats-officedocument.vmlDrawing',[])).

extension_(warc_1, warc).
label_(warc_1, "Web ARChive (WARC) archive format").
media_type_(warc_1, media(application/warc,[])).

extension_(wbmp_1, wbmp).
label_(wbmp_1, "Wireless Application Protocol Bitmap Format (WBMP)").
media_type_(wbmp_1, media(image/'vnd.wap.wbmp',[])).

extension_(webp_1, webp).
media_type_(webp_1, media(image/webp,[])).
label_(webp_1, "Google image format for the web (WebP)").

extension_(wmv_1, wmv).
label_(wmv_1, "Windows Media Video (WMV)").
media_type_(wmv_1, media(video/'x-ms-wmv',[])).
program_(wmv_1, vlc).

extension_(wrl_1, wrl).
media_type_(wrl_1, media(model/vrml,[])).
label_(wrl_1, "Virtual Reality Modeling Language (VRML)").

extension_(wrz_1, wrz).
label_(wrz_1, "GNU zipped VRML").
media_type_(wrz_1, media(model/vrml,[])).

extension_(zip_1, zip).
label_(zip_1, "ZIP").
media_type_(zip_1, media(application/zip,[])).
