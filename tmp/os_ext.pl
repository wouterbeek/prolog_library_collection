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
  process_create(
    path(wc),
    [file(File)],
    [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
  ),
  thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
  read_stream_to_codes(ProcOut, Codes),
  phrase(gnu_wc_out(Lines, Words, Bytes), Codes, _),
  process_wait(Pid, exit(Status)),
  process_status(Status).

% E.g., `427  1818 13512 README.md`
gnu_wc_out(Lines, Words, Bytes) -->
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
  process_create(
    path(identify),
    [file(File)],
    [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
  ),
  thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
  read_stream_to_codes(ProcOut, Codes),
  phrase(image_dimensions_out(File, Width, Height), Codes, _),
  process_wait(Pid, exit(Status)),
  process_status(Status).

image_dimensions_out(File, Width, Height) -->
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
        process_create(
          path(renice),
          [10,Pid],
          [process(Pid0),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
        ),
        thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
        thread_create(copy_stream_data(ProcOut, user_output), _, [detached(true)]),
        read_stream_to_codes(ProcOut, Codes),
        process_wait(Pid, exit(Status)),
        process_status(Status)
    ;   existence_error(process, Pid)
    )
  )).
