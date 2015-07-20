:- module(
  ansi_ext,
  [
    ansi_format/1, % +TriplesAndPairs:list
    ansi_format/2, % +Stream:stream
                   % +TriplesAndPairs:list
    ansi_format/4, % +Stream:stream
                   % +Attributes:list
                   % +Format
                   % +Args:list
    ansi_formatnl/1, % +TriplesAndPairs:list
    ansi_formatnl/2, % +Stream:stream
                     % +TriplesAndPairs:list
    ansi_formatnl/3, % +Attributes:list
                     % +Format
                     % +Args:list
    ansi_formatnl/4 % +Stream:stream
                    % +Attributes:list
                    % +Format
                    % +Args:list
  ]
).

/** <module> ANSI_EXT

Predicates for using ANSI markup in Unix consoles.

@author Wouter Beek
@version 2013/06-2013/07, 2013/09
*/

:- use_module(library(ansi_term)).
:- use_module(library(lists), except([delete/3,subset/2])).



ansi_format([]).
ansi_format([Attrs-Format-Args|T]):- !,
  ansi_format(Attrs, Format, Args),
  ansi_format(T).
ansi_format([ANSI-Format|T]):-
  is_list(ANSI), !,
  ansi_format(ANSI, Format, []),
  ansi_format(T).
ansi_format([Format-Args|T]):-
  is_list(Args), !,
  ansi_format([], Format, Args),
  ansi_format(T).
ansi_format([Format|T]):- !,
  ansi_format([], Format, []),
  ansi_format(T).

ansi_format(Stream, Triples):-
  forall(
    member(Attrs-Format-Args, Triples),
    ansi_format(Stream, Attrs, Format, Args)
  ).

%! ansi_format(+Stream:stream, +Attributes, +Format:atom, +Args:list) is det.
% Like the swipl builtin ansi_format/3, but allows writing to an arbitrary
% output stream.

ansi_format(Stream, Attrs, Format, Args):-
  with_output_to(Stream, ansi_format(Attrs, Format, Args)).

ansi_formatnl(Format):-
  ansi_format(Format),
  nl.

ansi_formatnl(Format, Args):-
  ansi_format(Format, Args),
  nl.

ansi_formatnl(Attrs, Format, Args):-
  ansi_format(Attrs, Format, Args),
  nl.

ansi_formatnl(Stream, Attrs, Format, Args):-
  with_output_to(Stream, ansi_formatnl(Attrs, Format, Args)).

