:- module(
  gnu_wc,
  [
    wc/2, % +File, -NumLines
    wc/4  % +File, -NumLines, -NumWords, -NumBytes
  ]
).

/** <module> GNU word count

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(dcg/basics)).
:- use_module(library(os/io)).
:- use_module(library(os/process_ext)).





%! wc(+File, -NumLines) is det.
%! wc(+File, -NumLines, -NumWords, -NumBytes) is det.

wc(File, N1) :-
  wc(File, N1, _, _).


wc(File, N1, N2, N3) :-
  run_process(wc, [file(File)], [output_goal(parse_wc0(N1,N2,N3))]).

parse_wc0(N1, N2, N3, In):-
  read_stream_to_codes(In, Cs),
  phrase(wc0(N1, N2, N3), Cs, _).

% Example:
% ```bash
% 427  1818 13512 README.md
% ```
wc0(N1, N2, N3) -->
  whites, integer(N1),
  whites, integer(N2),
  whites, integer(N3).
