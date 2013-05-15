:- module(
  relations,
  [
    bijective/1, % +F:bourbaki
    closure/4, % +S:ordset(pair)
               % +Antecedent:goal
               % +Consequence:compound
               % -NewS:ordset(pair)
    codomain/2, % +F:bourbaki
                % -Y:ordset
    domain/2, % +F:bourbaki
              % -X:ordset
    equivalence_class/4, % +U:ordset,
                         % +R:binary_relation,
                         % +X,
                         % -EqX:ordset
    equivalence/1, % +R:binary_relation
    graph/2, % +F:bourbaki
             % -G:graph
    image/2, % +F:bourbaki
             % -ImgF:ordset
    image/3, % +S:ordset
             % +G:graph
             % -ImgS:ordset
    pair/2, % +S:ordset
            % -XY:pair
    quotient_set/3, % +S:ordset
                    % +EqR:binary_relation
                    % -QuotS:ordset) is det.
    reflexive/1, % +R:binary_relation
    reflexive_closure/2, % S:ordset(pair)
                         % -NewS:ordset(pair)
    surjective/1, % +F:bourbaki
    symmetric/1, % +R:binary_relation
    symmetric_closure/2, % +S:ordset(pair)
                         % -NewS:ordset(pair)
    transitive/1, % +R:binary_relation
    transitive_closure/2 % +S:ordset(pair)
                         % -NewS:ordset(pair)
  ]
).

/** <module> Relations

# Types

## Bourbaki function definitions

We use function definitions as in Bourbaki1957:

==
bourbaki(X, Y, F)
==

Where =X= is the domain, =Y= is the codomain, and =G= is the graph.

## Graph

Graphs are collections of relations representing the data points of a
function. We represent them as ordered sets of pairs.

## Pair

Traditionally, pairs are represented using sets, i.e.,
$\langle x, y \rangle =_{def} \{ x, \{ x, y \} \}$.

We represent pairs as compound terms of the form =|X-Y|=.

## Relation

### Binary relations

Binary relations can be efficiently represented as =|list(pair)|=
=|[x_1-y_1, \ldots, x_m-y_m]|=.

@author Wouter Beek
@version 2012/11
*/

:- use_module(generics(meta_ext)).

:- meta_predicate(closure(*,0,+,*)).
:- meta_predicate(equivalence_class(+,2,+,-)).
:- meta_predicate(quotient_set(+,2,-)).



%! bijective(F) is semidet.
% Succeeds if the given function is bijective.
%
% @arg F A Bourbaki function definition.

bijective(F):-
  preimage(F, X),
  inverse(F, X).

%! closure(
%!   +S:ordset(pair),
%!   :Antecedent,
%!   +Consequence:compound,
%!   -NewS:ordset(pair)
%! ) is det.
% Calculates the closure for the given set of pairs.
%
% @arg S An ordered set of pairs, representing a binary relation.
% @arg Antecedent An executable goals that includes =S= and that contains
%        the free variables in =Consequence=.
% @arg Consequence A compound term with free variables. These are the
%        instances that will be included in =NewS=.
% @arg NewS The set =S= under closure.

closure(S, Antecedent, Consequence, NewS):-
  % These are the elements that are added by the closure.
  setoff(
    Consequence,
    call(Antecedent),
    Consequences
  ),
  % The original members are included as well.
  ord_union(S, Consequences, NewS).

%! codomain(+F:bourbaki, -Y:ordset) is det.
% Returns the codomain of a Bourbaki function definition.
%
% @arg F A Bourbaki function definition.
% @arg Y An ordered set representing the codomain of a function.

codomain(bourbaki(_X, Y, _F), Y).

%! domain(+F:bourbaki, -X:ordset) is det.
% Returns the domain of a Bourbaki function definition.
%
% @arg F A bourbaki function definition.
% @arg X An ordered set representing the domain of a function.

domain(bourbaki(X, _Y, _F), X).

%! equivalence(+R:binary_relation) is semidet.
% Succeeds if the given relation is an equivalence relation.
%
% @arg R A binary relation.

equivalence(R):-
  reflexive(R),
  symmetric(R),
  transitive(R).

%! equivalence_class(
%!   +U:ordset,
%!   +R:binary_relation,
%!   +X,
%!   -EqX:ordset
%! ) is det.
% Returns the equivalence class of =X= relative to equivalence relation =R=.
%
% The function that maps from elements onto their equivalence classes is
% sometimes calles the *|canonical projection map|*.
%
% @arg U The universe of discource. This comprises =X=.
%          Clearly $EqX \subseteq U=. No double occurrences and sorted.
% @arg R An binary equivalence relation, i.e., a relation that is
%            1. Reflexive
%            2. Symmetric
%            3. Transitive
% @arg X The element whose equivalence class is returned.
% @arg EqX The equivalence class of =X=. no double occurrences and sorted.

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

%! graph(+F:bourbaki, -G:graph) is det.
% Returns the graph of a Bourbaki function definition.
%
% @arg F A Bourbaki function definition.
% @arg G A graph.

graph(bourbaki(_X, _Y, G), G).

%! image(+F:bourbaki, -ImgF:ordset) is det.
% Returns the image of the function definition by the given
% Bourbaki definition.

image(bourbaki(DomF, _CodF, G), ImgX):-
  image(DomF, G, ImgX).

%! image(+S:ordset, +G:graph, -ImgS:ordset) is det.
% Returns the image of set =S= under the function with graph =G=.
%
% @arg S An ordered set.
% @arg G A graph, i.e., an ordered set of pairs.
% @arg ImgS An ordered set.

image(S, G, ImgX):-
  findall(
    Y,
    (
      member(X, S),
      member(X-Y, G)
    ),
    ImgX
  ).

% @tbd
inverse(X, X).

%! pair(+R:binary_relation, ?Pair:list) is nondet.
% Pairs in a binary relation. Both for checking and for generating.
%
% @arg R A binary relation.
% @arg Pair A list of two elements, representing a pair.

pair(R, X-Y):-
  member(X-Y, R).

% @tbd
preimage(X, X).

%! quotient_set(+S:ordset, +EqR:binary_relation, -QuotS:ordset) is det.
% Returns the quotient set for =S= under equivalence relation =EqR=.
%
% The quotient set of a set S is the set of all equivalence sets of
% elements in S. A quotient set of S is also a partition of S.
% A quotient set of S is normally notated as $S/~$, where ~ is the
% equivalence relation.
%
% @arg S An ordered set.
% @arg EqR A binary equivalence relation.
% @arg QuotS The quotient set of =S=. An ordered set.

quotient_set(S, EqR, QuotS):-
  setoff(
    EqX,
    (
      member(X, S),
      equivalence_class(S, EqR, X, EqX)
    ),
    QuotS
  ).

%! reflextive(R) is semidet.
% Succeeds if the given binary relation is reflexive.
%
% @arg R A binary relation.

reflexive(R):-
  forall(
    member(X-Ys, R),
    member(X, Ys)
  ).

reflexive_closure(S, NewS):-
  closure(S, (member(X-_, S) ; member(_-X, S)), X-X, NewS).

%! surjective(+F:bourbaki) is semidet.
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
% @arg F A Bourbaki function definition.

surjective(F):-
  codomain(F, X),
  image(F, X).

%! symmetric(R) is semidet.
% Succeeds if the given relation is symmetric.
%
% @arg R A binary relation.

symmetric(R):-
  forall(
    pair(R, X-Y),
    pair(R, Y-X)
  ).

symmetric_closure(S, NewS):-
  closure(S, pair(S, X-Y), Y-X, NewS).

%! transitive(+R:binary_relation) is semidet.
% Suceeds if the given binary relation is transitive.
%
% @arg R A binary relation.

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
