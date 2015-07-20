:- module(
  progress,
  [
    progress_bar/3 % +Current:number
                   % +End:number
                   % -ProgressBar:atom
  ]
).

/** <module> Progress

Tools for tracking the progress of (parallelized) batch processing.

@author Wouter Beek
@version 2015/02
*/

:- use_module(plc(generics/atom_ext)).





%! progress_bar(+Current:integer, End:integer, ProgressBar:atom) is det.
% Returns an atomic progress bar that displays the current value
%  onto a scale that runs from `1` to the given end value.
%
% @arg Current An integer, representing the current processed value.
% @arg End An integer, representing the last value to be processed.
% @arg ProgressBar The atomic representation of a progress bar.

progress_bar(End, End, ProgressBar2):- !,
  progress_bar0(End, End, ProgressBar1),
  format(atom(ProgressBar2), '~w [done]', [ProgressBar1]).
progress_bar(Current, End, ProgressBar):-
  progress_bar0(Current, End, ProgressBar).

progress_bar0(Current1, End, ProgressBar):-
  (   End =:= 0
  ->  Percentage = 100
  ;   Percentage is round(Current1 / End * 100)
  ),
  format_integer(Percentage, 3, Percentage1),
  (   End =:= 0
  ->  Progress = 10
  ;   Progress is round(Current1 / (End / 10))
  ),
  atom_number(EndAtom, End),
  atom_length(EndAtom, EndLength),
  format_integer(Current1, EndLength, Current2),
  repeating_atom('=', Progress, Bar),
  Fill is 10 - Progress,
  repeating_atom('-', Fill, NonBar),
  format(
    atom(ProgressBar),
    '~w% ~w~w (~w/~w)',
    [Percentage1, Bar, NonBar, Current2, End]
  ).
