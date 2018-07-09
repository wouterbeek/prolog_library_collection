:- module(
  char_ext,
  [
    char_digit/2,    % ?Char, ?Digit
    downcase_char/2, % +Char, -DowncaseChar
    first_char/2,    % +Source, ?Char
    last_char/2,     % +Source, ?Char
    to_chars/2,      % +Source, -Chars
    upcase_char/2    % +Char, -UpcaseChar
  ]
).

/** <module> Character extensions

@author Wouter Beek
@version 2015/07, 2016/02
*/

:- use_module(library(apply)).





%! char_digit(+Char, -Digit) is semidet.
%! char_digit(-Char, +Digit) is det.

char_digit(Char, Digit) :-
  char_type(Char, digit(Digit)).



%! downcase_char(+Char, -DowncaseChar) is det.

downcase_char(X, Y) :-
  char_type(Y, to_lower(X)).



%! first_char(+Source, +Char) is semidet.
%! first_char(+Source, -Char) is semidet.
% Source is either an atom, a list of characters, a list of codes, a number or
% a string.
%
% Silently fails if the source does not have a first character.

first_char(In, H):-
  to_chars(In, [H|_]).



%! last_char(+Source, +Char) is semidet.
%! last_char(+Source, -Char) is semidet.
% Source is either an atom, a list of characters, a list of codes, a number or
% a string.
%
% Silently fails if the source does not have a last character.

last_char(In, X):-
  to_chars(In, L),
  last(L, X).



%! to_chars(+Source, -Chars:list(char)) is det.
% Source is either an atom, a list of characters, a list of codes, a number or
% a string.
%
% Notice that the empty list of characters and the empty list of codes
% both map onto the empty list of characters.

% Atom.
to_chars(A, L):-
  atom(A), !,
  atom_chars(A, L).
% Non-empty list of characters.
% Includes the empty list.
to_chars(L, L):-
  maplist(is_char, L), !.
% Non-empty list of codes.
to_chars(Cs, L):-
  maplist(char_code, L, Cs).
% Number.
to_chars(N, L):-
  number(N), !,
  number_chars(N, L).
% String.
to_chars(S, L):-
  string(S), !,
  string_chars(S, L).



%! upcase_char(+Char, -UpcaseChar) is det.

upcase_char(X, Y) :-
  char_type(Y, to_upper(X)).
