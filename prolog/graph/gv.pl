:- module(
  gv,
  [
    gv_edge/3,         % +Out, +FromId, +ToId
    gv_edge/4,         % +Out, +FromId, +ToId. +Attributes
    gv_export/2,       % +File, :Goal_1
    gv_export/4,       % +Method, +Ext, +File, :Goal_1
    gv_extension/1,    % ?Ext
    gv_html_replace/2, % +Unescaped, -Escaped
    gv_id/2,           % +Term, -Id
    gv_node/3,         % +Out, +Id, +Attributes
    gv_open/1,         % -ProcIn
    gv_open/2,         % -ProcIn, -ProcOut
    gv_open/3,         % +Method, +Ext, -ProcIn
    gv_open/4,         % +Method, +Ext, -ProcIn, -ProcOut
    gv_reply/3,        % +Method, +MediaType, :Goal_1
    gv_show/1,         % :Goal_1
    gv_show/3,         % +Method, +Ext, :Goal_1
    is_gv_media_type/1 % +MediaType
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
@version 2017/08-2017/12
*/

:- use_module(library(call_ext)).
:- use_module(library(dcg)).
:- use_module(library(debug_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(media_type)).
:- use_module(library(option)).
:- use_module(library(os_ext)).
:- use_module(library(process)).
:- use_module(library(settings)).

:- discontiguous
    extension_type_/1,
    extension_type_/2.

:- meta_predicate
    gv_export(+, 1),
    gv_export(+, +, +, 1),
    gv_reply(+, +, 1),
    gv_show(1),
    gv_show(+, +, 1).

:- setting(default_export_extension, atom, pdf,
           "The extension of the default format for exporting graphs.").
:- setting(default_method, atom, dot,
           "The default method for generating graphs.").
:- setting(default_show_extension, atom, gtk,
           "The extension of the default external program used for viewing graphs.").





%! gv_attribute(+Attribute:compound, -String:string) is det.

gv_attribute(Attr, Str) :-
  Attr =.. [Name,Value],
  gv_attribute(Name, Value, Str).

gv_attribute(label, Values, Str) :-
  is_list(Values), !,
  atomics_to_string(Values, "<BR/>", Value),
  gv_attribute(label, Value, Str).
gv_attribute(label, Value0, Str) :- !,
  gv_html_replace(Value0, Value),
  format(string(Str), "label=<~s>", [Value]).
gv_attribute('URL', Value0, Str) :- !,
  gv_html_replace(Value0, Value),
  format(string(Str), "URL=\"~s\"", [Value]).
gv_attribute(Name, Value, Str) :-
  format(string(Str), "~a=\"~s\"", [Name,Value]).



%! gv_attributes(+Attributes:list(compound), -String:string) is det.

gv_attributes(Attrs, Str) :-
  maplist(gv_attribute, Attrs, Strs),
  atomics_to_string(Strs, ",", Str).



%! gv_edge(+Out:stream, +FromId:atom, +ToId:atom) is det.

gv_edge(Out, FromId, ToId) :-
  format_debug(gv, Out, "  ~a -> ~a;", [FromId,ToId]).



%! gv_edge(+Out:stream, +FromId:atom, +ToId:atom,
%!          +Attributes:list(compound)) is det.

gv_edge(Out, FromId, ToId, Attrs) :-
  gv_attributes(Attrs, Str),
  format_debug(gv, Out, "  ~a -> ~a [~s];", [FromId,ToId,Str]).



%! gv_export(+File:atom, :Goal_1) is det.
%! gv_export(+Method:atom, +Ext:atom, +File:atom, :Goal_1) is det.

gv_export(File, Goal_1) :-
  setting(default_method, Method),
  setting(default_export_extension, Ext),
  gv_export(Method, Ext, File, Goal_1).


gv_export(Method, Ext, File, Goal_1) :-
  extension_type_(Ext, Type),
  setup_call_cleanup(
    open(File, write, Out, [type(Type)]),
    setup_call_cleanup(
      gv_open(Method, Ext, ProcIn, ProcOut),
      (
        call_cleanup(
          call(Goal_1, ProcIn),
          close(ProcIn)
        ),
        copy_stream_data(ProcOut, Out)
      ),
      close(ProcOut)
    ),
    close(Out)
  ).



%! gv_extension(?Format:atom) is nondet.

gv_extension(bmp).
gv_extension(dot).
gv_extension(gif).
gv_extension(jpeg).
gv_extension(json).
gv_extension(pdf).
gv_extension(png).
gv_extension(ps).
gv_extension(svg).
gv_extension(tiff).



%! gv_html_replace(+Unescaped:string, -Escaped:string) is det.

gv_html_replace(String1, String2) :-
  string_phrase(html_replace, String1, String2).

html_replace, "&lt;" --> "<", !, html_replace.
html_replace, "&gt;" --> ">", !, html_replace.
html_replace, "&amp;" --> "&", !, html_replace.
html_replace, [C] --> [C], !, html_replace.
html_replace --> "".



%! gv_id(@Term, -Id:atom) is det.
%
% Id is a DOT-compatible unique identifier for Term.

gv_id(Term, Id) :-
  md5(Term, Hash),
  atomic_concat(n, Hash, Id).



%! gv_node(+Out:stream, +Id:atom, +Attributes:list(compound)) is det.
%
% The following attributes are supported:
%
%   * label(+list(string))
%
%     Is printed as a DOT HTML label to allow Unicode characters.
%
%   * Other options are written as DOT attributes.

gv_node(Out, Id, Attrs) :-
  gv_attributes(Attrs, Str),
  format_debug(gv, Out, "  ~a [~s];", [Id,Str]).



%! gv_open(-ProcIn:stream) is det.
%! gv_open(+Method:atom, +Extension:atom, -ProcIn:stream) is det.
%
% Open a GraphViz input stream but no GraphViz output stream.  This is
% used when _no_ export needs to be created, but content is for
% example displayed temporarily inside an application.

gv_open(ProcIn) :-
  setting(default_method, Method),
  setting(default_show_extension, Ext),
  gv_open(Method, Ext, ProcIn).


gv_open(Method, Ext, ProcIn) :-
  call_must_be(method, Method),
  call_must_be(extension_type_none, Ext),
  process_create(path(Method), ['-T',Ext], [stdin(pipe(ProcIn))]).

extension_type_none(Ext) :-
  extension_type_(Ext, none).



%! gv_open(-ProcIn:stream, -ProcOut:stream) is det.
%! gv_open(+Method:atom, +Extension:atom, -ProcIn:stream, -ProcOut:stream) is det.
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
  setting(default_show_extension, Ext),
  gv_open(Method, Ext, ProcIn, ProcOut).


gv_open(Method, Ext, ProcIn, ProcOut) :-
  call_must_be(method, Method),
  call_must_be(extension_type_not_none, Ext),
  extension_type_(Ext, Type),
  process_create(
    path(Method),
    ['-T',Ext],
    [stdin(pipe(ProcIn)),stdout(pipe(ProcOut))]
  ),
  set_stream(ProcOut, type(Type)).

extension_type_not_none(Ext) :-
  extension_type_(Ext, Type),
  Type \== none.



%! gv_reply(+Method:atom, +MediaType:compound, :Goal_1) is det.

gv_reply(Method, MediaType, Goal_1) :-
  atom_phrase(media_type(MediaType), Atom),
  format("Content-Type: ~a\n\n", [Atom]),
  media_type_extension(MediaType, Ext),
  assertion(gv_extension(Ext)),
  gv_open(Method, Ext, ProcIn, ProcOut),
  call(Goal_1, ProcIn),
  close(ProcIn),
  copy_stream_data(ProcOut, current_output),
  close(ProcOut).



%! gv_show(:Goal_1) is det.
%! gv_show(+Method:atom, +Extension:atom, :Goal_1) is det.

gv_show(Goal_1) :-
  setting(default_method, Method),
  setting(default_show_extension, Ext),
  gv_show(Method, Ext, Goal_1).


gv_show(Method, Ext, Goal_1) :-
  setup_call_cleanup(
    gv_open(Method, Ext, ProcIn),
    call(Goal_1, ProcIn),
    close(ProcIn)
  ).



%! is_gv_media_type(+MediaType:compound) is semidet.

is_gv_media_type(MediaType) :-
  media_type_extension(MediaType, Ext),
  gv_extension(Ext).





% GENERICS %

%! method(?Method:atom) is nondet.

method(circo).
method(dot).
method(fdp).
method(neato).
method(osage).
method(sfdp).
method(twopi).



%! extension_type_(?Extension:atom) is nondet.
%! extension_type_(?Extension:atom, ?Type:oneof([binary,none,text])) is nondet.
%
% @arg Type The type of output.  `none' in case there is no output.

extension_type_(Ext) :-
  extension_type_(Ext, _).

% BMP
extension_type_(bmp, binary).
extension_type_(canon, text).
% CGImage, a drawable image object in Core Graphics (the low-level
% procedural drawing API for iOS and Mac OS X).
extension_type_(cgimage, binary).
extension_type_(cmap, text).
extension_type_(cmapx, text).
extension_type_(cmapx_np, text).
% DOT
extension_type_(dot, text).
% DOT JSON
extension_type_(dot_json, text).
% Encapsulated PostScript
extension_type_(eps, binary).
% OpenEXR is a high dynamic-range (HDR) image file format developed by
% Industrial Light & Magic for use in computer imaging applications.
extension_type_(exr, binary).
% FIG
extension_type_(fig, text).
% GD
extension_type_(gd, text).
% GD2
extension_type_(gd2, binary).
% GIF
extension_type_(gif, binary).
% GTK
extension_type_(gtk, none).
% @see DOT
extension_type_(gv, text).
% ICO
extension_type_(ico, binary).
extension_type_(imap, text).
extension_type_(imap_np, text).
% HTML image map
extension_type_(ismap, text).
% JPEG 2000
extension_type_(jp2, binary).
% @see JPEG
extension_type_(jpe, binary).
% JPEG
extension_type_(jpeg, binary).
% @see JPEG
extension_type_(jpg, binary).
extension_type_(json, text).
extension_type_(json0, text).
%extension_type_(mp).
% PICT is a graphics file format introduced on the original Apple
% Macintosh computer as its standard metafile format.
extension_type_(pct, binary).
% Portable Document Format (PDF)
extension_type_(pdf, binary).
% PIC language developed for troff
extension_type_(pic, text).
% @see PICT
extension_type_(pict, binary).
extension_type_(plain, text).
extension_type_('plain-ext', text).
extension_type_(png, binary).
% Scene-description language for 3D modelling for the Persistence of
% Vision Raytracer.
extension_type_(pov, binary).
% PostScript
extension_type_(ps, binary).
% PostScript output with PDF notations.
extension_type_(ps2, binary).
% Adobe Photoshop PSD
extension_type_(psd, binary).
% SGI
extension_type_(sgi, binary).
% SVG
extension_type_(svg, text).
% compressed SVG
extension_type_(svgz, binary).
% Truevision TGA or TARGA
extension_type_(tga).
% TIFF
extension_type_(tif, binary).
% TIFF
extension_type_(tiff, binary).
% TK graphics primitives
extension_type_(tk, text).
% Microsoft Visio XML drawing file format
extension_type_(vdx, text).
% Vector Markup Language (VML)
extension_type_(vml, text).
% compressed Vector Markup Language (VML)
extension_type_(vmlz, binary).
% Virtual Reality Modeling Language (VRML)
extension_type_(vrml).
% Wireless BitMap (WBMP)
extension_type_(wbmp).
% image format for the Web (WEBP/WebP)
extension_type_(webp).
extension_type_(x11, none).
extension_type_(xdot, text).
extension_type_(xdot_json, text).
extension_type_('xdot1.2', text).
extension_type_('xdot1.4', text).
extension_type_(xlib, none).
