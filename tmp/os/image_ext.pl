:- module(
  image_ext,
  [
    image_dimensions/3, % +File:atom
                        % -Width:float
                        % -Height:float
    image_file/1, % +File:atom
    image_file_extension/1 % ?FileExtension:atom
  ]
).

/** <module> Image extensions

Support for image files.

@author Wouter Beek
@version 2014/03, 2014/05-2014/06, 2015/02-2015/03
*/

:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(generics/db_ext)).
:- use_module(plc(process/process_ext)).

:- db_add_novel(user:prolog_file_type(bmp, bmp)).
:- db_add_novel(user:prolog_file_type(bmp, image)).
:- db_add_novel(user:prolog_file_type(gif, gif)).
:- db_add_novel(user:prolog_file_type(gif, image)).
:- db_add_novel(user:prolog_file_type(jpeg, jpeg)).
:- db_add_novel(user:prolog_file_type(jpeg, image)).
:- db_add_novel(user:prolog_file_type(jpg, jpeg)).
:- db_add_novel(user:prolog_file_type(jpg, image)).
:- db_add_novel(user:prolog_file_type(png, png)).
:- db_add_novel(user:prolog_file_type(png, image)).





%! image_dimensions(+File:atom, -Width:float, -Height:float) is det.

image_dimensions(File, Width, Height):-
  handle_process(
    identify,
    [file(File)],
    [output_goal(process_identify_output(File,Width,Height)),program(identify)]
  ).

process_identify_output(File, Width, Height, Stream):-
  read_stream_to_codes(Stream, Codes),
  phrase(image_dimensions(File, Width, Height), Codes).



%! image_dimensions(+File:atom, -Width:float, -Height:float)// is det.

image_dimensions(File, Width, Height) -->
  atom(File),
  " ",
  atom(_FileType),
  " ",
  integer(Width),
  "x",
  integer(Height),
  dcg_done.


%! image_file(+File:atom) is semidet.

image_file(File):-
  file_name_extension(_, Extension, File),
  image_file_extension(Extension).


%! image_file_extension(+ImageFileExtension) is semidet.
%! image_file_extension(-ImageFileExtension) is nondet.

image_file_extension(Ext):-
  nonvar(Ext), !,
  image_file_extension0(Ext), !.
image_file_extension(Ext):-
  image_file_extension0(Ext).

image_file_extension0(Ext):-
  user:prolog_file_type(Ext, image).
