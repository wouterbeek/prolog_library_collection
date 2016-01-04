:- module(
  peek_ext,
  [
    peek_atom/2, % +Stream:stream
                 % +Atom:atom
    peek_length/3, % +Stream:stream
                  % +Length:nonneg
                  % ?Codes:list(code)
    peek_until_code/3 % +Stream:stream
                      % +Code:code
                      % -Codes:list(code)
  ]
).

/** <module> Input stream peeking

@author Wouter Beek
@version 2013/01, 2013/06, 2013/08, 2014/01, 2014/03-2014/04, 2014/09-2014/12
*/





%! peek_atom(+Stream:stream, +Atom:atom) is semidet.
% Succeeds if the given atom can be peeked at in the given stream,
% i.e. without changing the stream pointer.

peek_atom(Stream, Atom):-
  atom_codes(Atom, Codes),
  length(Codes, Length),
  peek_length(Stream, Length, Codes).



%! peek_length(+Stream:stream, +Length:nonneg, -Codes:list(code)) is semidet.
% Returns the next =Length= number of =Codes= without changing the stream
% pointer.

peek_length(Stream, Length, Codes):-
  Length >= 0,
  stream_property(Stream, position(OriginalPosition)),
  % If peek_length0/3 fails directly, then the stream position will not be
  % restored to the original position. Therefore, we store the exist code in
  % =Status= and call this after restoring the stream to its original
  % position.
  (   peek_length0(Stream, Length, Codes)
  ->  Status = true
  ;   Status = fail
  ),
  % Move back to the original position in the stream.
  set_stream_position(Stream, OriginalPosition),
  call(Status).

peek_length0(_Stream, 0, []):- !.
peek_length0(Stream, Length, [H|T]):-
  get_code(Stream, H),
  NewLength is Length - 1,
  peek_length0(Stream, NewLength, T).



%! peek_until_code(+Stream:stream, +Code:code, -Codes:list(code)) is nondet.

peek_until_code(Stream, Code, L):-
  get_code(Stream, H), !,
  (   H == -1
  ->  fail
  ;   H == Code
  ->  L = []
  ;   L = [H|T],
      peek_until_code(Stream, Code, T)
  ).
