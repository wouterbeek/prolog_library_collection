:- module(
  gv,
  [
    dot_edge/3,            % +Out, +FromId, +ToId
    dot_edge/4,            % +Out, +FromId, +ToId. +Attrs
    dot_id/2,              % +Term, -Id
    dot_node/3,            % +Out, +Id, +Attrs
    gv_export/2,     % +File, :Goal_1
    gv_export/4,     % +Method, +Format, +File, :Goal_1
    gv_media_type/2, % ?Format, ?Type
    gv_open/1,       % -ProcIn
    gv_open/2,       % -ProcIn, -ProcOut
    gv_open/3,       % +Method, +Format, -ProcIn
    gv_open/4,       % +Method, +Format, -ProcIn, -ProcOut
    gv_reply/3,      % +Method, +MediaType, :Goal_1
    gv_show/1,       % :Goal_1
    gv_show/3        % +Method, +Format, :Goal_1
  ]
).

/** <module> GraphViz

# Grammar for DOT HTML-like labels

```
label :   text
        | table
text :   textitem
       | text textitem
textitem :   string
           | <BR/>
           | <FONT> text </FONT>
           | <I> text </I>
           | <B> text </B>
           | <U> text </U>
           | <O> text </O>
           | <SUB> text </SUB>
           | <SUP> text </SUP>
           | <S> text </S>
table : [ <FONT> ] <TABLE> rows </TABLE> [ </FONT> ]
rows :   row
       | rows row
       | rows <HR/> row
row: <TR> cells </TR>
cells :   cell
        | cells cell
        | cells <VR/> cell
cell:   <TD> label </TD>
      | <TD> <IMG/> </TD>
```

@author Wouter Beek
@version 2017/08-2017/11
*/

:- use_module(library(call_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(settings)).

:- discontiguous
    output_format/1,
    output_format/2.

:- meta_predicate
    gv_export(+, 1),
    gv_export(+, +, +, 1),
    gv_reply(+, +, 1),
    gv_show(1),
    gv_show(+, +, 1).

:- setting(default_export_format, atom, pdf, "The default format for exporting graphs.").
:- setting(default_method, atom, dot, "The default method for generating graphs.").
:- setting(default_show_program, atom, gtk, "The default external program used for viewing graphs.").





%! dot_edge(+Out:stream, +FromId:atom, +ToId:atom) is det.

dot_edge(Out, FromId, ToId) :-
  format_debug(dot, Out, "  ~a -> ~a;", [FromId,ToId]).



%! dot_edge(+Out:stream, +FromId:atom, +ToId:atom, +Attrs:list(compound)) is det.

dot_edge(Out, FromId, ToId, Attrs) :-
  attributes_atom(Attrs, AttrsAtom),
  format_debug(dot, Out, "  ~a -> ~a [~a];", [FromId,ToId,AttrsAtom]).



%! dot_id(@Term, -Id:atom) is det.
%
% Id is a DOT-compatible unique identifier for Term.

dot_id(Term, Id) :-
  md5(Term, Hash),
  atomic_concat(n, Hash, Id).



%! dot_node(+Out:stream, +Id:atom, +Attrs:list(compound)) is det.
%
% The following attributes are supported:
%
%   * label(+list(string))
%
%     Is printed as a DOT HTML label to allow Unicode characters.
%
%   * Other options are written as DOT attributes.

dot_node(Out, Id, Attrs) :-
  attributes_atom(Attrs, AttrsAtom),
  format_debug(dot, Out, "  ~a [~a];", [Id,AttrsAtom]).

dot_attribute(Attr1, Attr2) :-
  Attr1 =.. [Name,Value],
  dot_attribute(Name, Value, Attr2).

dot_attribute(label, Values, Attr) :-
  is_list(Values), !,
  atomics_to_string(Values, "<BR/>", Value),
  dot_attribute(label, Value, Attr).
dot_attribute(label, Value, Attr) :- !,
  format(string(Attr), "label=<~a>", [Value]).
dot_attribute(Name, Value, Attr) :-
  format(string(Attr), "~a=\"~a\"", [Name,Value]).



%! gv_export(+File:atom, :Goal_1) is det.
%! gv_export(+Method:atom, +Format:atom, +File:atom, :Goal_1) is det.

gv_export(File, Goal_1) :-
  setting(default_method, Method),
  setting(default_show_format, Format),
  gv_export(Method, Format, File, Goal_1).


gv_export(Method, Format, File, Goal_1) :-
  output_format(Format, Type),
  setup_call_cleanup(
    open(File, write, Out, [type(Type)]),
    setup_call_cleanup(
      gv_open(Method, Format, ProcIn, ProcOut),
      (
        call(Goal_1, ProcIn),
        close(ProcIn),
        copy_stream_data(ProcOut, Out)
      ),
      close(ProcOut)
    ),
    close(Out)
  ).



%! gv_media_type(?Format:atom, ?Type:compound) is nondet.

gv_media_type(bmp, image/bmp).
gv_media_type(gif, image/gif).
gv_media_type(jpeg, image/jpeg).
gv_media_type(json, application/json).
gv_media_type(pdf, application/pdf).
gv_media_type(png, image/png).
gv_media_type(ps, application/postscript).
gv_media_type(dot, text/'vnd.graphviz').
gv_media_type(svg, image/'svg+xml').
gv_media_type(tiff, image/tiff).



%! gv_open(-ProcIn:stream) is det.
%! gv_open(+Method:atom, +Format:atom, -ProcIn:stream) is det.
%
% Open a GraphViz input stream but no GraphViz output stream.  This is
% used when _no_ export needs to be created, but content is for
% example displayed temporarily inside an application.

gv_open(ProcIn) :-
  setting(default_method, Method),
  setting(default_show_format, Format),
  gv_open(Method, Format, ProcIn).


gv_open(Method, Format, ProcIn) :-
  call_must_be(method, Method),
  call_must_be(output_format_none, Format),
  process_create(path(Method), ['-T',Format], [stdin(pipe(ProcIn))]).

output_format_none(Format) :-
  output_format(Format, none).



%! gv_open(-ProcIn:stream, -ProcOut:stream) is det.
%! gv_open(+Method:atom, +Format:atom, -ProcIn:stream,
%!               -ProcOut:stream) is det.
%
% Open a GraphViz input stream _and_ a GraphViz output stream.  The
% input stream expects statments in the DOT language.  The output
% stream will be in the indicated Format.
%
% @arg Method The algorithm used by GraphViz for positioning the tree
%             nodes.
%
% @arg Format The file type of the GraphViz output file.
%
% @type_error if Method is not a value of method/1.
%
% @type_error if Format is not a value of output_format/1.

gv_open(ProcIn, ProcOut) :-
  setting(default_method, Method),
  setting(default_show_format, Format),
  gv_show(Method, Format, ProcIn, ProcOut).


gv_open(Method, Format, ProcIn, ProcOut) :-
  call_must_be(method, Method),
  call_must_be(output_format_not_none, Format),
  output_format(Format, Type),
  process_create(
    path(Method),
    ['-T',Format],
    [stdin(pipe(ProcIn)),stdout(pipe(ProcOut))]
  ),
  set_stream(ProcOut, type(Type)).

output_format_not_none(Format) :-
  output_format(Format, Type),
  Type \== none.



%! gv_reply(+Method:atom, +MediaType:compound, :Goal_1) is det.

gv_reply(Method, media(Supertype/Subtype,_), Goal_1) :-
  format("Content-Type: ~a/~a\n\n", [Supertype,Subtype]),
  gv_media_type(Format, Supertype/Subtype),
  gv_open(Method, Format, ProcIn, ProcOut),
  call(Goal_1, ProcIn),
  close(ProcIn),
  copy_stream_data(ProcOut, current_output),
  close(ProcOut).



%! gv_show(:Goal_1) is det.
%! gv_show(+Method, +Format, :Goal_1) is det.

gv_show(Goal_1) :-
  setting(default_method, Method),
  setting(default_show_format, Format),
  gv_show(Method, Format, Goal_1).


gv_show(Method, Format, Goal_1) :-
  setup_call_cleanup(
    gv_open(Method, Format, ProcIn),
    call(Goal_1, ProcIn),
    close(ProcIn)
  ).





% SETTINGS %

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





% HELPERS %

%! attributes_atom(+Attrs:list(compound), -AttrsAtom:atom) is det.

attributes_atom(Attrs, AttrsAtom) :-
  maplist(dot_attribute, Attrs, AttrAtoms),
  atomics_to_string(AttrAtoms, ",", AttrsAtom).
