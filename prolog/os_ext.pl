:- module(
  os_ext,
  [
    exists_program/1,    % +Program
    image_dimensions/3,  % +File, -Width, -Height
    open_pdf/1,          % +File
    os/1,                % ?Os
    os_path/1,           % ?File
    os_path_separator/1, % ?Sep
    os_root_prefix/1,    % ?Prefix
    renice/2,            % +Pid:positive_integer, +Nice:between(-20,19)
    sort_file/1,         % +File
    sort_file/2,         % +File, +Opts
    wc/2,                % +File, -NumLines
    wc/4                 % +File, -NumLines, -NumWords, -NumBytes
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
:- use_module(library(cli_ext)).
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





%! exists_program(+Program) is semidet.
%
% Succeeds if the given program can be run from PATH.

exists_program(Program) :-
  var(Program), !,
  instantiation_error(Program).
exists_program(Program) :-
  os_path(Prefix),
  atomic_list_concat([Prefix,Program], /, Exe),
  access_file(Exe, execute), !.



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



%! open_pdf(+File) is det.
%
% Opens the given PDF file.

open_pdf(File) :-
  once((
    member(Program, [xpdf,evince]),
    exists_program(Program)
  )),
  run_process(Program, [file(File)]).



%! os(+Os) is semidet.
%! os(-Os) is det.
%
% Succeeds if Os names the current Operating System.
%
% Supported values are:
%   * mac
%   * unix
%   * windows

os(mac) :-
  current_prolog_flag(apple, true), !.
os(unix) :-
  current_prolog_flag(unix, true), !.
os(windows) :-
  current_prolog_flag(windows, true), !.



%! os_path(+File) is semidet.
%! os_path(-File) is nondet.
%
% Succeeds if File is on the OS path.

os_path(File) :-
  getenv('PATH', Path),
  os_path_separator(Sep),
  atomic_list_concat(Files, Sep, Path),
  member(File0, Files),
  prolog_to_os_filename(File, File0).



%! os_path_separator(+Separator) is semidet.
%! os_path_separator(-Separator) is det.
% Suceeds if Separator is the OS path separator character.

os_path_separator(Sep) :-
  os(Os),
  os_path_separator(Os, Sep).


%! os_path_separator(+Os:os, -Separator) is det.

os_path_separator(Os, Sep) :-
  os(Os),
  (   memberchk(Os, [mac,unix])
  ->  Sep = (:)
  ;   Os == windows
  ->  Sep = (;)
  ).



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



%! wc(+File, -NumLines) is det.
%! wc(+File, -NumLines, -NumWords, -NumBytes) is det.

wc(File, NumLines) :-
  wc(File, NumLines, _, _).


wc(File, NumLines, NumWords, NumBytes) :-
  run_process(wc, [file(File)], wc0(NumLines,NumWords,NumBytes)).

wc0(NumLines, NumWords, NumBytes, In) :-
  read_stream_to_codes(In, Cs),
  phrase(wc0(NumLines, NumWords, NumBytes), Cs, _).

% Example:
%
% ```bash
% 427  1818 13512 README.md
% ```

wc0(NumLines, NumWords, NumBytes) -->
  whites, integer(NumLines),
  whites, integer(NumWords),
  whites, integer(NumBytes).
