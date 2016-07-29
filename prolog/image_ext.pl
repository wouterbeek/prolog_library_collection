:- module(
  image_ext,
  [
    image_dimensions/3,     % +File, -Width, -Height
    image_file_extension/1, % ?Ext
    is_image_file/1,        % @Term
    is_image_iri/1          % @Term
  ]
).

/** <module> Image extensions

Support for image files.

@author Wouter Beek
@version 2015/08, 2015/10-2015/11, 2016/07
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/process_ext)).
:- use_module(library(readutil)).
:- use_module(library(typecheck)).

:- dynamic
    user:prolog_file_type/2,
    user:module_uses/2.

:- multifile
    user:prolog_file_type/2,
    user:module_uses/2.

user:prolog_file_type(bmp, bmp).
user:prolog_file_type(bmp, image).
user:prolog_file_type(gif, gif).
user:prolog_file_type(gif, image).
user:prolog_file_type(jpeg, jpeg).
user:prolog_file_type(jpeg, image).
user:prolog_file_type(jpg, jpeg).
user:prolog_file_type(jpg, image).
user:prolog_file_type(png, png).
user:prolog_file_type(png, image).

user:module_uses(image_ext, program(identify)).





%! image_dimensions(+File:atom, -Width:float, -Height:float) is det.
%
% @see Requires ImageMagick.

image_dimensions(File, Width, Height) :-
  run_process(
    identify,
    [file(File)],
    [output_goal(image_dimensions0(File, Width, Height))]
  ).


image_dimensions0(File, Width, Height, In) :-
  read_stream_to_codes(In, Cs),
  phrase(image_dimensions0(File, Width, Height), Cs).


image_dimensions0(File, Width, Height) -->
  atom(File),
  " ",
  ...,
  " ",
  integer(Width),
  "x",
  integer(Height),
  done.



%! image_file_extension(+Ext) is semidet.
%! image_file_extension(-Ext) is nondet.

image_file_extension(Ext) :-
  user:prolog_file_type(Ext, image).



%! is_image_file(+File) is semidet.
%
% Determines whether a file stores an image or not based on
% its file name extension.

is_image_file(File) :-
  file_extensions(File, Exts),
  once((
    member(Ext, Exts),
    image_file_extension(Ext)
  )).



%! is_image_iri(+Iri) is semidet.
% Succeeds if the given Iri is commonly understood to denote an image file.

is_image_iri(Iri) :-
  is_iri(Iri),
  iri_comp(Iri, path, Path),
  is_image_file(Path).
