:- module(
  dcg_phrase,
  [
    atom_phrase/2, % :Dcg_0
                   % ?Atom:atom
    atom_phrase/3, % :Dcg_0
                   % +Atom1:atom
                   % ?Atom2:atom
    dcg_max_width/3, % :Dcg_1
                     % +Arguments:list
                     % -MaxWidth:nonneg
    dcg_width/2, % :Dcg_0
                 % -Width:nonneg
    dcg_with_output_to/2, % +Output:compound
                          % :Dcg_0
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
@version 2015/07, 2015/11-2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(code_ext)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(error)).
:- use_module(library(lists)).

:- meta_predicate(atom_phrase(//,?)).
:- meta_predicate(atom_phrase(//,?,?)).
:- meta_predicate(dcg_max_width(3,+,-)).
:- meta_predicate(dcg_width(//,-)).
:- meta_predicate(dcg_with_output_to(+,//)).
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



%! dcg_max_width(:Dcg_1, +Arguments:list, -MaxWidth:nonneg) is det.

dcg_max_width(Dcg_1, Args, MaxW):-
  aggregate_all(
    max(W),
    (
      member(Arg, Args),
      dcg_width(dcg_call_cp(Dcg_1, Arg), W)
    ),
    MaxW
  ).



%! dcg_width(:Dcg_0, -Width:nonneg) is det.

dcg_width(Dcg_0, W):-
  dcg_with_output_to(codes(Cs), Dcg_0),
  length(Cs, W).



%! dcg_with_output_to(+Write:compound, :Dcg_0) is nondet.

dcg_with_output_to(Write, Dcg_0):-
  phrase(Dcg_0, Cs),
  with_output_to(Write, put_codes(Cs)).



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
