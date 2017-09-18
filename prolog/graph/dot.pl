:- module(
  dot,
  [
    dot_hash/2, % +Term, -Hash
    dot_node/3, % +Out, +Id, +Options
    graphviz/3, % +Method, -ProcIn, +Format
    graphviz/4  % +Method, -ProcIn, +Format, -ProcOut
  ]
).

/** <module> DOT

@author Wouter Beek
@version 2017/08-2017/09
*/

:- use_module(library(call_ext)).
:- use_module(library(debug)).
:- use_module(library(hash_ext)).
:- use_module(library(option)).
:- use_module(library(process)).

:- discontiguous
    output_format/1,
    output_format/2.





%! dot_hash(@Term, -Hash:atom) is det.
%
% GraphViz-friendly hash.

dot_hash(Term, Hash2) :-
  md5(Term, Hash1),
  atomic_concat(n, Hash1, Hash2).



%! dot_node(+Out:stream, +Id:atom, +Options:list(compound)) is det.
%
% The following Options are supported:
%
%   * label(+string)
%
%     Is printed as a DOT HTML label to allow Unicode characters.
%
%   * Other options are written as DOT attributes.

dot_node(Out, Id, Options1) :-
  maplist(dot_attribute, Options1, Options2),
  atomic_list_concat(Options2, ',', Options3),
  format(Out, '  ~a [~a];\n', [Id,Options3]),
  debug(dot, '  ~a [~a];\n', [Id,Options3]).

dot_attribute(Option, Atom) :-
  Option =.. [Name,Value],
  dot_attribute(Name, Value, Atom).

dot_attribute(label, Value, Atom) :- !,
  format(atom(Atom), 'label=<~a>', [Value]).
dot_attribute(Name, Value, Atom) :-
  format(atom(Atom), '~a="~a"', [Name,Value]).



%! graphviz(+Method:atom, -ProcIn:stream, +Format:atom) is det.

graphviz(Method, ProcIn, Format) :-
  call_must_be(method, Method),
  call_must_be(output_format_none, Format),
  process_create(path(Method), ['-T',Format], [stdin(pipe(ProcIn))]).

output_format_none(Format) :-
  output_format(Format, none).



%! graphviz(+Method:atom, -ProcIn:stream, +Format:atom,
%!          -ProcOut:stream) is det.
%
% @arg Method The algorithm used by GraphViz for positioning the tree
%             nodes.
%
% @arg Format The file type of the GraphViz output file.
%
% @type_error if Method is not a value of method/1.
%
% @type_error if Format is not a value of output_format/1.

graphviz(Method, ProcIn, Format, ProcOut) :-
  call_must_be(method, Method),
  call_must_be(output_format_not_none, Format),
  output_format(Format, Type),
  process_create(path(Method), ['-T',Format],
                 [stdin(pipe(ProcIn)),stdout(pipe(ProcOut))]),
  set_stream(ProcOut, type(Type)).

output_format_not_none(Format) :-
  output_format(Format, Type),
  Type \== none.



%! method(?Method:atom) is nondet.

method(circo).
method(dot).
method(fdp).
method(neato).
method(osage).
method(sfdp).
method(twopi).



%! output_format(?Format:atom) is nondet.
%! output_format(?Format:atom, ?Type:oneof([binary,none,text])) is nondet.
%
% @arg Type The type of output.  `none' in case there is no output.

output_format(Format) :-
  output_format(Format, _).

output_format('plain-ext', text).
output_format('xdot1.2', text).
output_format('xdot1.4', text).
% BMP
output_format(bmp, binary).
output_format(canon, text).
% CGImage, a drawable image object in Core Graphics (the low-level
% procedural drawing API for iOS and Mac OS X).
output_format(cgimage, binary).
output_format(cmap, text).
output_format(cmapx, text).
output_format(cmapx_np, text).
% DOT
output_format(dot, text).
% DOT JSON
output_format(dot_json, text).
% Encapsulated PostScript
output_format(eps, binary).
% OpenEXR is a high dynamic-range (HDR) image file format developed by
% Industrial Light & Magic for use in computer imaging applications.
output_format(exr, binary).
% FIG
output_format(fig, text).
% GD
output_format(gd, text).
% GD2
output_format(gd2, binary).
% GIF
output_format(gif, binary).
% GTK
output_format(gtk, none).
% @see DOT
output_format(gv, text).
% ICO
output_format(ico, binary).
output_format(imap, text).
output_format(imap_np, text).
% HTML image map
output_format(ismap, text).
% JPEG 2000
output_format(jp2, binary).
% @see JPEG
output_format(jpe, binary).
% JPEG
output_format(jpeg, binary).
% @see JPEG
output_format(jpg, binary).
output_format(json, text).
output_format(json0, text).
%output_format(mp).
% PICT is a graphics file format introduced on the original Apple
% Macintosh computer as its standard metafile format.
output_format(pct, binary).
% Portable Document Format (PDF)
output_format(pdf, binary).
% PIC language developed for troff
output_format(pic, text).
% @see PICT
output_format(pict, binary).
output_format(plain, text).
output_format(png, binary).
% Scene-description language for 3D modelling for the Persistence of
% Vision Raytracer.
output_format(pov, binary).
% PostScript
output_format(ps, binary).
% PostScript output with PDF notations.
output_format(ps2, binary).
% Adobe Photoshop PSD
output_format(psd, binary).
% SGI
output_format(sgi, binary).
% SVG
output_format(svg, text).
% compressed SVG
output_format(svgz, binary).
% Truevision TGA or TARGA
output_format(tga).
% TIFF
output_format(tif, binary).
% TIFF
output_format(tiff, binary).
% TK graphics primitives
output_format(tk, text).
% Microsoft Visio XML drawing file format
output_format(vdx, text).
% Vector Markup Language (VML)
output_format(vml, text).
% compressed Vector Markup Language (VML)
output_format(vmlz, binary).
% Virtual Reality Modeling Language (VRML)
output_format(vrml).
% Wireless BitMap (WBMP)
output_format(wbmp).
% image format for the Web (WEBP/WebP)
output_format(webp).
output_format(x11, none).
output_format(xdot, text).
output_format(xdot_json, text).
output_format(xlib, none).
