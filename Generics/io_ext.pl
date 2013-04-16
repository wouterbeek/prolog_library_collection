:- module(
  io_ext,
  [
    peek_atom/2, % +Stream:stream
                 % +Atom:atom
    peek_length/3 % +Stream:stream
                  % +Length:integer
                  % ?Codes:list(integer)
  ]
).

/** <module> I/O extensions

Predicates that extend the swipl builtin I/O predicates.

@author Wouter Beek
@version 2013/01
*/



%% peek_atom(+Stream:stream, +Atom:atom) is semidet.

peek_atom(Stream, Atom):-
  atom_codes(Atom, Codes),
  length(Codes, Length),
  peek_length(Stream, Length, Codes).

%% peek_length(
%%   +Stream:stream,
%%   +Length:integer,
%%   -Codes:list(integer)
%% ) is semidet.
%% peek_length(+Stream:stream, +Length:integer, -Codes:list(integer)) is det.
% Returns the next =Length= number of =Codes=.

peek_length(Stream, Length, Codes):-
  Length >= 0,
  stream_property(Stream, position(OriginalPosition)),
  % If peek_length0/3 fails directly, then the stream position will not be
  % restored to the original position. Therefore, we store the exist code in
  % =Status= and call this after restoring the stream to its original
  % position.
  (
    peek_length0(Stream, Length, Codes)
  ->
    Status = true
  ;
    Status = fail
  ),
  set_stream_position(Stream, OriginalPosition),
  call(Status).

peek_length0(_Stream, 0, []):-
  !.
peek_length0(Stream, Length, [Code | Codes]):-
  get_code(Stream, Code),
  NewLength is Length - 1,
  peek_length0(Stream, NewLength, Codes).

