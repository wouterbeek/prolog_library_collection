:- module(
  io_ext,
  [
    read_stream_to_atom/2, % +Read:stream
                           % -Content:atom
    read_stream_to_string/2 % +Read:stream
                            % -Content:string
  ]
).

/** <module> Input/Output Extensions

Predicates that extend the swipl builtin I/O predicates operating on streams.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(readutil)).





%! read_stream_to_atom(+Read:stream, -Content:atom) is det.
% Stores the contents of an atom stream to an atom.

read_stream_to_atom(Read, A):-
  read_stream_to_codes(Read, Cs),
  atom_codes(A, Cs).



%! read_stream_to_string(+Read:stream, -Content:string) is det.
% Stores the contents of an atom stream to an atom.

read_stream_to_atom(Read, S):-
  read_stream_to_codes(Read, Cs),
  string_codes(S, Cs).
