:- module(
  os_ext,
  [
    gnu_wc/2,            % +File, -Lines
    gnu_wc/4             % +File, -Lines, -Words, -Bytes
    image_dimensions/3,  % +File, -Width, -Height
    os_root_prefix/1,    % ?Prefix
    renice/2,            % +Pid, +Nice
    sort_file/1,         % +File
    sort_file/2,         % +File, +Opts
  ]
).
:- reexport(library(true)).

/** <module> OS extensions

Support for using external programs and other OS functions.

@author Wouter Beek

@version 2015/08-2017/01, 2017/04
*/

:- use_module(library(aggregate)).
:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(io)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(settings)).
:- use_module(library(thread_ext)).
:- use_module(library(typecheck)).
:- use_module(library(zlib)).

:- setting(
     os:tmp_dir,
     term,
     _,
     "Temporary directory."
   ).





%! gnu_wc(+File:atom, -Lines:nonneg) is det.
%! gnu_wc(+File:atom, -Lines:nonneg, -Words:nonneg, -Bytes:nonneg) is det.

gnu_wc(File, Lines) :-
  gnu_wc(File, Lines, _, _).


gnu_wc(File, Lines, Words, Bytes) :-
  run_process(wc, [file(File)], gnu_wc_(Lines,Words,Bytes)).

gnu_wc_(Lines, Words, Bytes, In) :-
  read_stream_to_codes(In, Codes),
  phrase(gnu_wc_(Lines, Words, Bytes), Codes, _Rest).

% E.g., `427  1818 13512 README.md`
gnu_wc_(Lines, Words, Bytes) -->
  whites,
  integer(Lines),
  whites,
  integer(Words),
  whites,
  integer(Bytes).



%! image_dimensions(+File, -Width:float, -Height:float) is det.
%
% @see Requires ImageMagick.

image_dimensions(File, Width, Height) :-
  run_process(
    identify,
    [file(File)],
    image_dimensions0(File, Width, Height)
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



%! os_root_prefix(+Prefix) is semidet.
%! os_root_prefix(-Prefix) is multi.

:- if(os(unix)).
os_root_prefix(/).
:- endif.
:- if(os(windows)).
os_root_prefix('C:\\').
:- endif.



%! renice(+Pid:positive_integer, +Nice:between(-20,19)) is det.

renice(Pid, N) :-
  with_mutex(process_id,(
    (   process_id(Pid)
    ->  must_be(between(-20,19), N),
        run_process(renice, [10,Pid])
    ;   existence_error(process, Pid)
    )
  )).
