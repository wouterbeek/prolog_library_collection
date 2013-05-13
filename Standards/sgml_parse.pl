:- module(
  sgml_parse,
  [
    cleanup_parser/3, % +Stream:stream
                      % +Parser
                      % +State:term
    make_parser/3 % +Stream:stream
                  % -Parser
                  % -State:term
  ]
).

/** <module> SGML

This module was created by taking predicates from ClioPatria that are not in
SWI-Prolog. The predicates were created by Jan Wielemaker, I only put them
together in this file.

@author Wouter Beek
@version 2013/04
*/

:- use_module(library(sgml)).



%! cleanup_parser(+Stream:stream, +Parser, +State:term) is det.
% Removes the given SGML parser and sets the stream to the given position.
% The state term (containing the stream position) allows the stream to be
% reset to the original position before the parser was created.
%
% @arg Stream A text stream.
% @arg Parser An SGML parser. See module =|library(sgml)|=.
% @arg State A term of the form =|state(Position)|=, the
%        position that the stream is (re)set to.
%
% @author Jan Wielemaker
% @version 2011

cleanup_parser(Stream, Parser, state(Position)):-
  free_sgml_parser(Parser),
  set_stream_position(Stream, Position).

%! make_parser(+Stream:stream, -Parser, -State:term) is det.
% Creates a parser that processes the given stream and that starts at the
% given state.
%
% @arg Stream A text stream.
% @arg Parser An SGML parser. See module =|library(sgml)|=.
% @arg State A term of the form =|state(Position)|=, retuning the
%        position in the stream at which the parser starts.
%
% @author Jan Wielemaker
% @version 2011

make_parser(Stream, Parser, state(Position)):-
  stream_property(Stream, position(Position)),
  new_sgml_parser(Parser, []),
  set_sgml_parser(Parser, dialect(xmlns)).

