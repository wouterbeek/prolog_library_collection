:- module(
  gnu_wc,
  [
    file_lines/2 % +File:atom
                 % -NumberOfLines:nonneg
  ]
).

/** <module> GNU word count

Prolog API to GNU word count.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/dcg_content)).
:- use_module(library(io_ext)).
:- use_module(library(process_ext)).
%:- use_module(library(pure_input)).





%! file_lines(+File:atom, -NumberOfLines:nonneg) is det.
% @tbd Use phrase_from_stream/2.

file_lines(File, N):-
  run_process(wc, ['-l',file(File)], [output_goal(parse_wc(N))]).
parse_wc(N, Read):- read_input_to_codes(Read, Cs), phrase((integer(N), dcg_done), Cs).
