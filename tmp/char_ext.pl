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
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(lists)).





%! char_digit(+Char:char, -Digit:between(0,9)) is semidet.
%! char_digit(-Char:char, +Digit:between(0,9)) is det.

char_digit(Char, Digit) :-
  char_type(Char, digit(Digit)).



%! downcase_char(+Char:char, -DowncaseChar:char) is det.

downcase_char(X, Y) :-
  char_type(Y, to_lower(X)).



%! first_char(+Source, +Char:char) is semidet.
%! first_char(+Source, -Char:char) is semidet.
%
% Source is either an atom, a list of characters, a list of codes, a
% number or a string.
%
% Silently fails if the source does not have a first character.

first_char(Source, H):-
  to_chars(Source, [H|_]).



%! last_char(+Source, +Char:char) is semidet.
%! last_char(+Source, -Char:char) is semidet.
%
% Source is either an atom, a list of characters, a list of codes, a
% number or a string.
%
% Silently fails if the source does not have a last character.

last_char(Source, X):-
  to_chars(Source, L),
  last(L, X).



%! to_chars(+Source:or([atom,list(char),list(code),number,string]),
%!          -Chars:list(char)) is det.
%
% Source is either an atom, a list of characters, a list of codes, a
% number or a string.
%
% Notice that the empty list of characters and the empty list of codes
% both map onto the empty list of characters.

% the empty list
to_chars([], []) :- !.
% atom
to_chars(A, L):-
  atom(A), !,
  atom_chars(A, L).
% non-empty list of characters
to_chars(L, L):-
  maplist(is_char, L), !.
% non-empty list of codes
to_chars(Cs, L):-
  maplist(char_code, L, Cs).
% number
to_chars(N, L):-
  number(N), !,
  number_chars(N, L).
% string
to_chars(S, L):-
  string(S), !,
  string_chars(S, L).



%! upcase_char(+Char:char, -UpcaseChar:char) is det.

upcase_char(X, Y) :-
  char_type(Y, to_upper(X)).
