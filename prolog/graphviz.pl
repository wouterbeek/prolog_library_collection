:- module(
  graphviz,
  [
    graphviz/4,     % +Method, -ProcIn, +Format, -ProcOut
    graphviz_hash/2 % +Term, -Hash
  ]
).

/** <module> GraphViz

@author Wouter Beek
@version 2017/08
*/

:- use_module(library(call_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(process)).





%! graphviz(+Method:atom, -ProcIn:stream, +Format:atom,
%!          -ProcOut:stream) is det.
%
% @arg Method The algorithm used by GraphViz for positioning the tree
%             nodes.
%
% @arg Format The file type of the GraphViz output file.
%
% @type_error if Method is not a value of graphviz_method/1.
%
% @type_error if Format is not a value of graphviz_format/1.

graphviz(Method, ProcIn, Format, ProcOut) :-
  call_must_be(graphviz_method, Method),
  call_must_be(graphviz_format, Format),
  process_create(
    path(Method),
    ['-T',Format],
    [stdin(pipe(ProcIn)),stdout(pipe(ProcOut))]
  ).

graphviz_format(bmp).
graphviz_format(canon).
graphviz_format(dot).
graphviz_format(gv).
graphviz_format(xdot).
graphviz_format('xdot1.2').
graphviz_format('xdot1.4').
graphviz_format(cgimage).
graphviz_format(cmap).
graphviz_format(eps).
graphviz_format(exr).
graphviz_format(fig).
graphviz_format(gd).
graphviz_format(gd2).
graphviz_format(gif).
graphviz_format(gtk).
graphviz_format(ico).
graphviz_format(imap).
graphviz_format(cmapx).
graphviz_format(imap_np).
graphviz_format(cmapx_np).
graphviz_format(ismap).
graphviz_format(jp2).
graphviz_format(jpg).
graphviz_format(jpeg).
graphviz_format(jpe).
graphviz_format(pct).
graphviz_format(pict).
graphviz_format(pdf).
graphviz_format(pic).
graphviz_format(plain).
graphviz_format('plain-ext').
graphviz_format(png).
graphviz_format(pov).
graphviz_format(ps).
graphviz_format(ps2).
graphviz_format(psd).
graphviz_format(sgi).
graphviz_format(svg).
graphviz_format(svgz).
graphviz_format(tga).
graphviz_format(tif).
graphviz_format(tiff).
graphviz_format(tk).
graphviz_format(vml).
graphviz_format(vmlz).
graphviz_format(vrml).
graphviz_format(wbmp).
graphviz_format(webp).
graphviz_format(xlib).
graphviz_format(x11).

graphviz_method(circo).
graphviz_method(dot).
graphviz_method(fdp).
graphviz_method(neato).
graphviz_method(osage).
graphviz_method(sfdp).
graphviz_method(twopi).



%! graphviz_hash(@Term, -Hash:atom) is det.
%
% GraphViz-friendly hash.

graphviz_hash(Term, Hash2) :-
  md5(Term, Hash1),
  atomic_concat(n, Hash1, Hash2).
