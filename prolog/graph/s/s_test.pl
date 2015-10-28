:- module(
  s_graph_test,
  [
    s_graph_test/2 % ?Name:compound
             % ?Graph:ugraph
  ]
).

/** <module> Graphs that are used for testing predicates

Supports unit tests in various modules.

@author Wouter Beek
@version 2015/10
*/




%! s_graph_test(+Name:compound, -Graph:ugraph) is semidet.
%! s_graph_test(-Name:compound, -Graph:ugraph) is multi.

% Paths.
s_graph_test(path(2), [1-[2],2-[1]]).
s_graph_test(path(3), [1-[2],2-[1,3],3-[2]]).
s_graph_test(path(4), [1-[2],2-[1,3],3-[2,4],4-[3]]).
s_graph_test(path(5), [1-[2],2-[1,3],3-[2,4],4-[3,5],5-[4]]).

% Stars.
s_graph_test(star(2), [1-[2],2-[1]]).
s_graph_test(star(3), [1-[2],2-[1,3],3-[2]]).
s_graph_test(star(4), [1-[4],2-[4],3-[4],4-[1,2,3]]).
s_graph_test(star(5), [1-[5],2-[5],3-[5],4-[5],5-[1,2,3,4]]).
s_graph_test(star(6), [1-[6],2-[6],3-[6],4-[6],5-[1,2,3,4,6],6-[]]).

% Cycles.
s_graph_test(cycle(3), [1-[2,3],2-[1,3],3-[1,2]]).
s_graph_test(cycle(4), [1-[2,3],2-[1,3],3-[2,4],4-[1,3]]).
s_graph_test(cycle(5), [1-[2,3],2-[1,3],3-[2,4],4-[3,5],5-[1,4]]).
s_graph_test(cycle(6), [1-[2,3],2-[1,3],3-[2,4],4-[3,5],5-[4,5],6-[1,5]]).

% Equivalence.
s_graph_test(equiv(1), [1-[2,3,4],2-[1,3,4],3-[1,2,4],4-[1,2,3]]).

s_graph_test("1_5", [1-[3],2-[3,4,5],3-[1,2,4,5],4-[2,3],5-[2,3]]).
