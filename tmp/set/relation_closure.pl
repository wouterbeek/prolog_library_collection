:- module(
  relation_closure,
  [
    relation_closure/3 % +Set:ordset(pair)
                       % -Closure:ordset(pair)
                       % +Module:atom
  ]
).
:- reexport(library(set/ordset_closure)).

/** <module> Relation closure

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(set/relation)).

%! relation_closure(
%!   +Set:ordset(pair),
%!   -Closure:ordset(pair),
%!   +Module:atom
%! ) is det.

relation_closure(Rel1, Rel2, Mod):-
  relation_components(Rel1, Vs, Es1),
  ordset_closure(Es1, Es2, Mod),
  relation_components(Rel2, Vs, Es2).
