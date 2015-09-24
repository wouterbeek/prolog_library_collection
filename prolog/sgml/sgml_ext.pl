:- module(
  sgml_ext,
  [
    sgml_parser/2 % +Read:stream
                  % :Goal_1
  ]
).
:- reexport(library(sgml)).
:- reexport(library(sgml_write)).

/** <module> SGML extensions

Extended support for parsing SGML.

@author Wouter Beek
@version 2015/09
*/

:- meta_predicate(sgml_parser(+,1)).





%! sgml_parser(+Read:stream, :Goal_1) is det.
% Creates a parser that processes the given stream and that resets
% the stream to the original position before the parser was created.
%
% @author Based on a version by Jan Wielemaker
%         that appears in ClioPatria.

sgml_parser(Read, Goal_1):-
  setup_call_cleanup(
    make_parser(Read, Parser, State),
    call(Goal_1, Parser),
    clean_parser(Read, Parser, State)
  ).

make_parser(Read, Parser, state(Pos)):-
  stream_property(Read, position(Pos)),
  new_sgml_parser(Parser, []).

clean_parser(Read, Parser, state(Pos)):-
  free_sgml_parser(Parser),
  set_stream_position(Read, Position).
