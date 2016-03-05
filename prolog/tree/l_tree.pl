:- module(
  l_tree,
  [
    is_l_tree/1, % @Term
    l_tree_to_graph/2 % +Tree, -Graph
  ]
).

/** <module> Labeled trees

@author Wouter Beek
@version 2016/02-2016/03
*/

:- use_module(library(graph/l/l_graph)).
:- use_module(library(lists)).





%! is_l_tree(@Term) is semidet.

is_l_tree(t(_,Pairs)) :-
  forall(member(_-T, Pairs), is_l_tree(T)).



%! l_tree_to_edges(+Tree, -Es) is det.

l_tree_to_edges(t(X,Pairs), Es) :-
  findall(
    [edge(X,P,Y)|Es],
    (
      member(P-t(Y,Pairs0), Pairs),
      l_tree_to_edges(t(Y,Pairs0), Es)
    ),
    Ess
  ),
  append(Ess, Es).



%! l_tree_to_graph(+Tree, -G) is det.

l_tree_to_graph(Tree, G) :-
  l_tree_to_edges(Tree, Es),
  l_edges_vertices(Es, Vs),
  G = graph(Vs, Es).
