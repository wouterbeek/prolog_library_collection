:- module(
  chr_ext,
  [
    chr_closure/3, % +Set, -Closure, +Module
    chr_closure/5  % +Set, -Closure, +Module, +Topic, :Dcg_3
  ]
).
:- reexport(library(chr)).

/** <module> Constraint Handling Rules

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).

:- use_module(library(dcg)).
:- use_module(library(dcg_pl)).

:- meta_predicate
    chr_closure(+, -, +, +, 3),
    chr_debug(+, 3, +).





%! chr_closure(+Set:ordset, -Closure:ordset, +Module:atom) is det.
%! chr_closure(+Set:ordset, -Closure:ordset, +Module:atom, +Topic:compound, :Dcg_3) is det.
%
% Wrapper around chr_closure/5 that used the default debug tools.

chr_closure(S1, S2, Mod):-
  chr_closure(S1, S2, Mod, chr, term).


chr_closure(S1, S2, Mod, Topic, Dcg_3):-
  maplist(call_on_module(Mod), S1),
  aggregate_all(
    set(Fact),
    (
      Mod:find_chr_constraint(Fact),
      chr_debug(Topic, Dcg_3, Fact)
    ),
    S2
  ), !.





% HELPERS %

%! call_on_module(+Module:atom, :Goal_0) is det.

call_on_module(Mod, Goal_0):-
  call(Mod:Goal_0).



%! chr_debug(+Topic:compound, :Dcg_3, +Fact) is det.

chr_debug(Topic, Dcg_3, Fact):-
  debug(Topic, dcg_call(Dcg_3, Fact)).
