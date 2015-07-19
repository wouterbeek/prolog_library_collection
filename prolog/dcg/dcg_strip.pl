:- module(
  dcg_strip,
  [
    strip//1, % :Dcg_0
    strip_left//1, % :Dcg_0
    strip_right//1 % :Dcg_0
  ]
).

/** <module> DCG strip

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_abnf)).

:- meta_predicate(strip(//,?,?)).
:- meta_predicate(strip_left(//,?,?)).
:- meta_predicate(strip_right(//,?,?)).





strip(Dcg_0), Cs -->
  '*'(Dcg_0, []),
  string(Cs),
  '+'(Dcg_0, []),
  eos, !.
strip(_), Cs -->
  string(Cs),
  eos.


strip_left(Dcg_0), Cs -->
  '*'(Dcg_0, []),
  string(Cs),
  eos, !.



strip_right(Dcg_0), Cs -->
  string(Cs),
  '+'(Dcg_0, []),
  eos, !.
strip_right(_), Cs -->
  string(Cs),
  eos.
