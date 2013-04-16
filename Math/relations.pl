:- module(
  relations,
  [
    bijective/1, % +F:bourbaki
    closure/4, % +S:ord_set(pair)
               % +Antecedent:goal
               % +Consequence:compound
               % -NewS:ord_set(pair)
    codomain/2, % +F:bourbaki
                % -Y:ord_set
    domain/2, % +F:bourbaki
              % -X:ord_set
    equivalence_class/4, % +U:ord_set,
                         % +R:binary_relation,
                         % +X,
                         % -EqX:ord_set
    equivalence/1, % +R:binary_relation
    graph/2, % +F:bourbaki
             % -G:graph
    image/2, % +F:bourbaki
             % -ImgF:ord_set
    image/3, % +S:ord_set
             % +G:graph
             % -ImgS:ord_set
    pair/2, % +S:ord_set
            % -XY:pair
    quotient_set/3, % +S:ord_set
                    % +EqR:binary_relation
                    % -QuotS:ord_set) is det.
    reflexive/1, % +R:binary_relation
    reflexive_closure/2, % S:ord_set(pair)
                         % -NewS:ord_set(pair)
    surjective/1, % +F:bourbaki
    symmetric/1, % +R:binary_relation
    symmetric_closure/2, % +S:ord_set(pair)
                         % -NewS:ord_set(pair)
    transitive/1, % +R:binary_relation
    transitive_closure/2 % +S:ord_set(pair)
                         % -NewS:ord_set(pair)
  ]
).

/** <module> Relations

---+ Types

---++ Bourbaki function definitions

We use function definitions as in Bourbaki1957:

==
bourbaki(X, Y, F)
==

Where =X= is the domain, =Y= is the codomain, and =G= is the graph.

---++ Graph

Graphs are collections of relations representing the data points of a
function. We represent them as ordered sets of pairs.

---++ Pair

Traditionally, pairs are represented using sets, i.e.,
$\langle x, y \rangle =_{def} \{ x, \{ x, y \} \}$.

We represent pairs as compound terms of the form =|X-Y|=.

---++ Relation

---+++ Binary relations

Binary relations can be efficiently represented as =|list(pair)|=
=|[x_1-y_1, \ldots, x_m-y_m]|=.

@author Wouter Beek
@version 2012/11
*/

:- use_module(generic(meta_ext)).

:- meta_predicate equivalence_class(+,2,+,-).



%% bijective(F) is semidet.
% Succeeds if the given function is bijective.
%
% @param F A Bourbaki function definition.

bijective(F):-
  preimage(F, X),
  inverse(F, X).

%% closure(
%%   +S:ord_set(pair),
%%   +Antecedent:goal,
%%   +Consequence:compound,
%%   -NewS:ord_set(pair)
%% ) is det.
% Calculates the closure for the given set of pairs.
%
% @param S An ordered set of pairs, representing a binary relation.
% @param Antecedent An executable goals that includes =S= and that contains
%        the free variables in =Consequence=.
% @param Consequence A compound term with free variables. These are the
%        instances that will be included in =NewS=.
% @param NewS The set =S= under closure.

closure(S, Antecedent, Consequence, NewS):-
  % These are the elements that are added by the closure.
  setoff(
    Consequence,
    call(Antecedent),
    Consequences
  ),
  % The original members are included as well.
  ord_union(S, Consequences, NewS).

%% codomain(+F:bourbaki, -Y:ord_set) is det.
% Returns the codomain of a Bourbaki function definition.
%
% @param F A Bourbaki function definition.
% @param Y An ordered set representing the codomain of a function.

codomain(bourbaki(_X, Y, _F), Y).

%% domain(+F:bourbaki, -X:ord_set) is det.
% Returns the domain of a Bourbaki function definition.
%
% @param F A bourbaki function definition.
% @param X An ordered set representing the domain of a function.

domain(bourbaki(X, _Y, _F), X).

%% equivalence(+R:binary_relation) is semidet.
% Succeeds if the given relation is an equivalence relation.
%
% @param R A binary relation.

equivalence(R):-
  reflexive(R),
  symmetric(R),
  transitive(R).

%% equivalence_class(
%%   +U:ord_set,
%%   +R:binary_relation,
%%   +X,
%%   -EqX:ord_set
%% ) is det.
% Returns the equivalence class of =X= relative to equivalence relation =R=.
%
% The function that maps from elements onto their equivalence classes is
% sometimes calles the *|canonical projection map|*.
%
% @param U The universe of discource. This comprises =X=.
%          Clearly $EqX \subseteq U=. No double occurrences and sorted.
% @param R An binary equivalence relation, i.e., a relation that is
%            1. Reflexive
%            2. Symmetric
%            3. Transitive
% @param X The element whose equivalence class is returned.
% @param EqX The equivalence class of =X=. no double occurrences and sorted.

equivalence_class(U, R, X, EqX):-
  % Findall/3 suffices to preserve order in =EqX=,
  % since =U= is assumed to be ordered.
  findall(
    Y,
    (
      member(Y, U),
      call(R, X, Y)
    ),
    EqX
  ).

%% graph(+F:bourbaki, -G:graph) is det.
% Returns the graph of a Bourbaki function definition.
%
% @param F A Bourbaki function definition.
% @param G A graph.

graph(bourbaki(_X, _Y, G), G).

%% image(+F:bourbaki, -ImgF:ord_set) is det.
% Returns the image of the function definition by the given
% Bourbaki definition.

image(bourbaki(DomF, _CodF, G), ImgX):-
  image(DomF, G, ImgX).

%% image(+S:ord_set, +G:graph, -ImgS:ord_set) is det.
% Returns the image of set =S= under the function with graph =G=.
%
% @param S An ordered set.
% @param G A graph, i.e., an ordered set of pairs.
% @param ImgS An ordered set.

image(S, G, ImgX):-
  findall(
    Y,
    (
      member(X, S),
      member(X-Y, G)
    ),
    ImgX
  ).

%% pair(+R:binary_relation, ?Pair:list) is nondet.
% Pairs in a binary relation. Both for checking and for generating.
%
% @param R A binary relation.
% @param Pair A list of two elements, representing a pair.

pair(R, X-Y):-
  member(X-Y, R).

%% quotient_set(+S:ord_set, +EqR:binary_relation, -QuotS:ord_set) is det.
% Returns the quotient set for =S= under equivalence relation =EqR=.
%
% The quotient set of a set S is the set of all equivalence sets of
% elements in S. A quotient set of S is also a partition of S.
% A quotient set of S is normally notated as $S/~$, where ~ is the
% equivalence relation.
%
% @param S An ordered set.
% @param EqR A binary equivalence relation.
% @param QuotS The quotient set of =S=. An ordered set.

quotient_set(S, EqR, QuotS):-
  setoff(
    EqX,
    (
      member(X, S),
      equivalence_class(S, EqR, X, EqX)
    ),
    QuotS
  ).

%% reflextive(R) is semidet.
% Succeeds if the given binary relation is reflexive.
%
% @param R A binary relation.

reflexive(R):-
  forall(
    member(X-Ys, R),
    member(X, Ys)
  ).

reflexive_closure(S, NewS):-
  closure(S, (member(X-_, S) ; member(_-X, S)), X-X, NewS).

%% surjective(+F:bourbaki) is semidet.
% Succeeds if the function defined by the given Bourbaki definition is
% surjective.
%
% A function is surjective if for every element in its codomain there is
% at least one preimage in its domain.
%
% For efficiency we do not run a loop on all elements in the codomain etc.
% Instead we use the following theorem: A function f is surjective iff
% the codomain of f is the image of f's domain (under f).
%
% @param F A Bourbaki function definition.

surjective(F):-
  codomain(F, X),
  image(F, X).

%% symmetric(R) is semidet.
% Succeeds if the given relation is symmetric.
%
% @param R A binary relation.

symmetric(R):-
  forall(
    pair(R, X-Y),
    pair(R, Y-X)
  ).

symmetric_closure(S, NewS):-
  closure(S, pair(S, X-Y), Y-X, NewS).

%% transitive(+R:binary_relation) is semidet.
% Suceeds if the given binary relation is transitive.
%
% @param R A binary relation.

transitive(R):-
  forall(
    (
      pair(R, X-Y),
      pair(R, Y-Z)
    ),
    pair(R, X-Z)
  ).

transitive_closure(S, NewS):-
  closure(S, (pair(S, X-Y), pair(S, Y-Z)), X-Z, NewS).
