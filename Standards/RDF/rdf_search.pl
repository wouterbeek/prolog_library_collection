:- module(
  rdf_search,
  [
    breadth_first_rdf_traversal/5 % +Set:oneof([ground,list,ordset])
                                  % +R1:uri
                                  % +R2:uri
                                  % -Sol1:ordset
                                  % -Sol2:ordset
  ]
).

/** <module> RDF SEARCH

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(meta_ext)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)). % Meta declarations.

:- rdf_meta(breadth_first_rdf_traversal(+,r,r,-,-)).
:- rdf_meta(breadth_first_rdf_traversal(+,r,r,+,+,-,-)).



breadth_first_rdf_traversal(Set, R1, R2, Sol1, Sol2):-
  is_ordset(Set),
  !,
  breadth_first_rdf_traversal(Set, R1, R2, Set, [], Sol1, Sol2).
breadth_first_rdf_traversal(List, R1, R2, Sol1, Sol2):-
  is_list(List),
  !,
  list_to_ord_set(List, Set),
  breadth_first_rdf_traversal(Set, R1, R2, Sol1, Sol2).
breadth_first_rdf_traversal(Element, R1, R2, Sol1, Sol2):-
  ground(Element),
  !,
  breadth_first_rdf_traversal([Element], R1, R2, Sol1, Sol2).

breadth_first_rdf_traversal([], _R_AB, _R_BA, SolA, SolB, SolA, SolB):-
  !.
breadth_first_rdf_traversal(A1, R_AB, R_BA, HistA1, HistB1, SolA, SolB):-
  setoff(B, (member(A, A1), rdf_has(B, R_AB, A), \+ member(B, HistB1)), B2),
  setoff(A, (member(B, B2), rdf_has(B, R_BA, A), \+ member(A, HistA1)), A2),
  ord_union(HistA1, A2, HistA2),
  ord_union(HistB1, B2, HistB2),
  breadth_first_rdf_traversal(A2, R_AB, R_BA, HistA2, HistB2, SolA, SolB).

