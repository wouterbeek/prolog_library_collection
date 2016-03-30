:- module(
  gnu_wc,
  [
    file_lines/2, % +File, -NunLines
    wc/4          % +File, -NumLines, -NumWords, -NumBytes
  ]
).

/** <module> GNU word count

@author Wouter Beek
@version 2015/08, 2015/10, 2016/03
*/

:- use_module(library(dcg/basics)).
:- use_module(library(os/io_ext)).
:- use_module(library(os/process_ext)).





%! file_lines(+File, -NoLines) is det.

file_lines(File, N):-
  wc(File, N, _, _).



%! wc(+File, -NumLines, -NumWords, -NumBytes) is det.

wc(File, N1, N2, N3) :-
  run_process(wc, [file(File)], [output_goal(parse_wc0(N1,N2,N3))]).

parse_wc0(N1, N2, N3, Read):-
  read_input_to_codes(Read, Cs),
  phrase(wc0(N1, N2, N3), Cs, _).

% Example:
% ```bash
%    427  1818 13512 README.md
% ```
wc0(N1, N2, N3) -->
  whites, integer(N1),
  whites, integer(N2),
  whites, integer(N3).
