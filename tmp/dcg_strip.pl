:- module(
  dcg_strip,
  [
    dcg_strip//1, % :Dcg_0
    dcg_strip_left//1, % :Dcg_0
    dcg_strip_right//1 % :Dcg_0
  ]
).

/** <module> DCG strip

@author Wouter Beek
@version 2015/07, 2015/10-2015/11
*/

:- use_module(library(dcg)).

:- meta_predicate(dcg_strip(//,?,?)).
:- meta_predicate(dcg_strip_left(//,?,?)).
:- meta_predicate(dcg_strip_right(//,?,?)).





dcg_strip(Dcg_0), Cs       --> *(Dcg_0), string(Cs), *(Dcg_0), eos.

dcg_strip_left(Dcg_0), Cs  --> *(Dcg_0), string(Cs).

dcg_strip_right(Dcg_0), Cs --> string(Cs), *(Dcg_0), eos.
