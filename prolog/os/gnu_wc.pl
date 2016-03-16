:- module(
  gnu_wc,
  [
    file_lines/2 % +File, -NoLines
  ]
).

/** <module> GNU word count

@author Wouter Beek
@version 2015/08, 2015/10, 2016/03
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(os/io_ext)).
:- use_module(library(os/process_ext)).





%! file_lines(+File, -NoLines) is det.
% @tbd Use phrase_from_stream/2.

file_lines(File, N):-
  run_process(wc, ['-l',file(File)], [output_goal(parse_wc(N))]).

parse_wc(N, Read):-
  read_input_to_codes(Read, Cs),
  phrase((integer(N), done), Cs).
