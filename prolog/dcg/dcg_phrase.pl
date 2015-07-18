:- module(
  dcg_phrase,
  [
    atom_phrase/2, % :Dcg_0
                   % ?Atom:atom
    atom_phrase/3, % :Dcg_0
                   % +Atom1:atom
                   % ?Atom2:atom
    string_phrase/2, % :Dcg_0
                     % ?String:string
    string_phrase/3 % :Dcg_0
                    % +String1:string
                    % ?String2:string
  ]
).

/** <module> DCG phrase

Extensions to phrase/[2,3] for atom and string arguments.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(error)).

:- meta_predicate(atom_phrase(//,?)).
:- meta_predicate(atom_phrase(//,?,?)).
:- meta_predicate(string_phrase(//,?)).
:- meta_predicate(string_phrase(//,?,?)).



%! atom_phrase(:Dcg_0, ?Atom:atom)// is nondet.
% @throws instantiation_error
% @throws type_error

atom_phrase(Dcg_0, A):-
  var(A), !,
  phrase(Dcg_0, Cs),
  atom_codes(A, Cs).
atom_phrase(Dcg_0, A):-
  must_be(atom, A),
  atom_codes(A, Cs),
  phrase(Dcg_0, Cs).


%! atom_phrase(:Dcg_0, +Atom1:atom, ?Atom2:atom)// is nondet.
% @throws instantiation_error
% @throws type_error

atom_phrase(Dcg_0, A1, A2):-
  must_be(atom, A1),
  atom_codes(A1, Cs1),
  phrase(Dcg_0, Cs1, Cs2),
  atom_codes(A2, Cs2).



%! string_phrase(:Dcg_0, ?String:string)// is nondet.

string_phrase(Dcg_0, S):-
  var(S), !,
  phrase(Dcg_0, Cs),
  string_codes(S, Cs).
string_phrase(Dcg_0, S):-
  must_be(string, S),
  string_codes(S, Cs),
  phrase(Dcg_0, Cs).


%! string_phrase(:Dcg_0, +String1:string, ?String2:string)// is nondet.

string_phrase(Dcg_0, S1, S2):-
  must_be(string, S1),
  string_codes(S1, Cs1),
  phrase(Dcg_0, Cs1, Cs2),
  string_codes(S2, Cs2).
