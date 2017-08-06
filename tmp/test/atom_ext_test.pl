:- module(atom_ext_test, []).

:- use_module(library(plunit)).

:- begin_tests('atom_ending_in(+,+,-) is det.').

:- use_module(library(atom_ext)).

test(
  'atom_ending_in(+,+,-) is det. TRUE',
  [forall(atom_ending_in_test(Atom,Suffix,NewAtom,true))]
):-
  atom_ending_in(Atom, Suffix, NewAtom).
test(
  'atom_ending_in(+,+,-) is det. FAIL',
  [fail,forall(atom_ending_in_test(Atom,Suffix,NewAtom,fail))]
):-
  atom_ending_in(Atom, Suffix, NewAtom).

atom_ending_in_test('/home/sjaak/', /, '/home/sjaak/', true).
atom_ending_in_test('/home/sjaak',  /, '/home/sjaak/', true).
atom_ending_in_test('/home/sjaak',  /, '/home/sjaak',  fail).
atom_ending_in_test('/home/sjaak',  /, '/home/sjaak',  fail).
atom_ending_in_test(peoples,        s, peoples,        true).
atom_ending_in_test(people,         s, peoples,        true).

:- end_tests('atom_ending_in(+,+,-) is det.').
