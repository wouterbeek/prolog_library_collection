:- module(
  s_test,
  [
    s_test_graph/2 % ?Name:compound
                   % ?Graph:ugraph
  ]
).

/** <module> Graphs that are used for testing predicates

Supports unit tests in various modules.

@author Wouter Beek
@version 2015/10
*/





%! s_test_graph(+Name:compound, -Graph:ugraph) is semidet.
%! s_test_graph(-Name:compound, -Graph:ugraph) is multi.

s_test_graph(Name, G):-
  ground(Name), !,
  once(s_test_graph0(Name, G)).
s_test_graph(Name, G):-
  s_test_graph0(Name, G).

% Paths.
s_test_graph0(path(2), [1-[2],2-[1]]).
s_test_graph0(path(3), [1-[2],2-[1,3],3-[2]]).
s_test_graph0(path(4), [1-[2],2-[1,3],3-[2,4],4-[3]]).
s_test_graph0(path(5), [1-[2],2-[1,3],3-[2,4],4-[3,5],5-[4]]).

% Star-shaped.
s_test_graph0(star(2), [1-[2],2-[1]]).
s_test_graph0(star(3), [1-[2],2-[1,3],3-[2]]).
s_test_graph0(star(4), [1-[4],2-[4],3-[4],4-[1,2,3]]).
s_test_graph0(star(5), [1-[5],2-[5],3-[5],4-[5],5-[1,2,3,4]]).
s_test_graph0(star(6), [1-[6],2-[6],3-[6],4-[6],5-[1,2,3,4,6],6-[]]).

% Cyclic.
s_test_graph0(cycle(1), [1-[2,3,4],2-[1,3,4],3-[1,2,4],4-[1,2,3]]).
s_test_graph0(cycle(3), [1-[2,3],2-[1,3],3-[1,2]]).
s_test_graph0(cycle(4), [1-[2,3],2-[1,3],3-[2,4],4-[1,3]]).
s_test_graph0(cycle(5), [1-[2,3],2-[1,3],3-[2,4],4-[3,5],5-[1,4]]).
s_test_graph0(cycle(6), [1-[2,3],2-[1,3],3-[2,4],4-[3,5],5-[4,5],6-[1,5]]).

% Equivalence.
s_test_graph0(equiv(1), [1-[2,3,4],2-[1,3,4],3-[1,2,4],4-[1,2,3]]).
s_test_graph0(equiv(2), []).
s_test_graph0(equiv(3), [a-[a,b],b-[a,b]]).
s_test_graph0(equiv(4), [a-[a]]).
s_test_graph0(equiv(5), [a-[a,b]]).
s_test_graph0(equiv(6), [a-[a,b],b-[a,b],c-[c,d],d-[c,d]]).
s_test_graph0(equiv(7), [a-[a,b,c,d],b-[a,b,c,d],c-[a,b,c,d],d-[a,b,c,d]]).

% Regular / non-regular.
s_test_graph0(nonregular(1), [1-[3],2-[3,5],3-[1,2,4,5],4-[2,3,5],5-[2,3,4]]).

% Various
s_test_graph0(various(1), [1-[3],2-[3,4,5],3-[1,2,4,5],4-[2,3],5-[2,3]]).
s_test_graph0(various(2), [1-[3],2-[3,5],3-[1,2,4,5],4-[2,3,5],5-[2,3,4]]).
