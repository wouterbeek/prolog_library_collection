:- module(
  int_ext,
  [
    int_div/3, % +In1:integer
               % +In2:integer
               % -Out:integer
    int_mod/3, % +In1:integer
               % +In2:integer
               % -Out:integer
    int_plus/3, % ?X:integer
                % ?Y:integer
                % ?Z:integer
    to_integer/2 % +Atomic:or([atom,integer,string])
                 % -Integer:integer
  ]
).

/** <module> Integer extensions

Support predicates for integer values.

@author Wouter Beek
@version 2013/08, 2014/02, 2014/10
*/

:- use_module(library(error)).



%! int_div(+X:integer, +Y:integer, -Z:integer) is det.

int_div(X, Y, Z):-
  Z is floor(X / Y).



%! int_mod(+X:integer, +Y:integer, -Z:integer) is det.

int_mod(X, Y, Z):-
  Z is X mod Y.



%! int_plus(?X:integer, ?Y:integer, ?Z:integer) is det.

int_plus(X, Y, Z):-
  plus(X, Y, Z).



%! to_integer(+Atomic:or([atom,integer,string]), -Integer:integer) is det.

% Atom.
to_integer(Atom, Integer):-
  atom(Atom),
  atom_number(Atom, Number),
  to_integer(Number, Integer).
% Integer.
to_integer(Integer, Integer):-
  integer(Integer), !.
% String.
to_integer(String, Integer):-
  string(String), !,
  number_string(Number, String),
  to_integer(Number, Integer).
% Domain error.
to_integer(Term, _):-
  domain_error(or([atom,integer,string]), Term).
