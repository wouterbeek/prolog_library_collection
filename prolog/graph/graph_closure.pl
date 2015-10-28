:- module(
  graph_closure,
  [
    graph_closure/4 % +Set:ordset
                    % :V2Ns_2
                    % -Closure:ordset
                    % +Options:list(compound)
  ]
).

/** <module> Graph theory: Closure

Graph closures.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- meta_predicate(graph_closure(+,2,-,+)).
:- meta_predicate(graph_closure(+,+,2,+,-)).

:- predicate_options(graph_closure/4, 4, [
     traversal(+oneof([breadth,depth]))
   ]).





%! graph_closure(
%!   +Set:ordset,
%!   :V2Ns_2,
%!   -Closure:ordset,
%!   +Options:list(compound)
%! ) is det.
% Closes a set of objects under a given predicate expressing a relation
% between those objects.
%
% The following options are supported:
%   * traversal(+oneof([breath,depth]))

graph_closure([], _, [], _):- !.
graph_closure([H|T], V2Ns_2, Sol, Opts):-
  option(traversal(Trav), Opts),
  graph_closure(Trav, [H|T], V2Ns_2, [H], Sol).

%! graph_closure(
%!   +Traversal:oneof([breadth,depth]),
%!   +Set:ordset,
%!   :V2Ns_2,
%!   +CurrentSet:ordset,
%!   -Closure:ordset
%! ) is det.

graph_closure(_, [], _, Sol, Sol):- !.
graph_closure(Traversal, [H|T1], V2Ns_2, Vs1, Sol):-
  call(V2Ns_2, H, Ns1),
  ord_subtract(Ns1, Vs1, Ns2),
  (   Traversal == depth
  ->  append(Ns2, T1, T2)
  ;   Traversal == breadth
  ->  append(T1, Ns2, T2)
  ),
  ord_union(Vs1, Ns2, Vs2),
  graph_closure(Traversal, T2, V2Ns_2, Vs2, Sol).
