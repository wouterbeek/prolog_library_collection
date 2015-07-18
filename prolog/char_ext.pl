:- module(
  char_ext,
  [
    first_char/2, % +Input:or([atom,list(char),list(code),number,string])
                  % ?Char:char
    is_char/1, % @Term
    last_char/2, % +Input:or([atom,list(char),list(code),number,string])
                 % ?Char:char
    to_chars/2 % +Input:or([atom,list(char),list(code),number,string])
               % -Chars:list(char)
  ]
).

/** <module> Character extensions

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(apply)).
:- use_module(library(lists)).



%! first_char(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   +Char:char
%! ) is semidet.
%! first_char(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   -Char:char
%! ) is semidet.

first_char(In, H):-
  to_chars(In, [H|_]).



% is_char(@Term) is semidet.

is_char(Term):-
  atom(Term),
  atom_length(Term, 1).



%! last_char(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   +Char:char
%! ) is semidet.
%! last_char(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   -Char:char
%! ) is semidet.
% Silently fails if the input maps to the empty list of characters.

last_char(In, X):-
  to_chars(In, L),
  last(L, X).



%! to_chars(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   -Chars:list(char)
%! ) is det.
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
