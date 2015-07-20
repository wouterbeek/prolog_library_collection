:- module(
  relations,
  [
    bijective/1, % +Function:bourbaki
    codomain/2, % +Function:bourbaki
                % -Codomain:ordset
    domain/2, % +Function:bourbaki
              % -Domain:ordset
    graph/2, % +Function:bourbaki
             % -Graph:ugraph
    image/2, % +Function:bourbaki
             % -Image:ordset
    image/3, % +Set:ordset
             % +Graph:ugraph
             % -Image:ordset
    surjective/1 % +Function:bourbaki
  ]
).

/** <module> Relations

# Types

## Bourbaki function definitions

We use function definitions as in Bourbaki1957:

```prolog
bourbaki(Domain:ordset,Codomain:ordset,Graph:ugraph)
```

Where =X= is the domain, =Y= is the codomain, and =G= is the graph.

A relations is said to map *into* if it is functional.

## Graph

Graphs are collections of relations representing the data points of a
function. We represent them as ordered sets of pairs.

## Pair

Traditionally, pairs are represented using sets, i.e.,
$\langle x, y \rangle =_{def} \{ x, \{ x, y \} \}$.

We represent pairs as compound terms of the form `X-Y`.

## Relation

### Binary relations

Binary relations can be efficiently represented as `list(pair)`
`[x_1-y_1, \ldots, x_m-y_m]`.

@author Wouter Beek
@version 2012/11, 2013/08, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).



%! bijective(+Function:bourbaki) is semidet.
% Succeeds if the given function is bijective.

bijective(Function):-
  preimage(Function, Set),
  inverse(Function, Set).


%! codomain(+Function:bourbaki, -Codomain:ordset) is det.
% Returns the codomain of a Bourbaki function definition.

codomain(bourbaki(_,Codomain, _), Codomain).


%! domain(+Function:bourbaki, -Domain:ordset) is det.
% Returns the domain of a Bourbaki function definition.

domain(bourbaki(Domain,_,_), Domain).


%! graph(+Function:bourbaki, +Graph:ugraph) is semidet.
%! graph(+Function:bourbaki, -Graph:ugraph) is det.
% Returns the graph of a Bourbaki function definition.

graph(bourbaki(_,_,Graph), Graph).


%! image(+Function:bourbaki, -Image:ordset) is det.
% Returns the image of the function definition by the given
% Bourbaki definition.

image(bourbaki(Domain,_,Graph), Image):-
  image(Domain, Graph, Image).


%! image(+Set:ordset, +Graph:ugraph, -Image:ordset) is det.
% Returns the image of set `Set` under the function with graph `Graph`.

image(Set, Graph, Image):-
  findall(
    Y,
    (
      member(X, Set),
      member(X-Y, Graph)
    ),
    Image
  ).


% @tbd
inverse(X, X).


% @tbd
preimage(X, X).


%! surjective(+Function:bourbaki) is semidet.
% Succeeds if the function defined by the given Bourbaki definition is
% surjective.
%
% A function is *surjective* if for every element in its codomain there is
% at least one preimage in its domain.
%
% A function that is surjective is said to map *onto*.
%
% For efficiency we do not run a loop on all elements in the codomain etc.
% Instead we use the following theorem:
% ```
% A function f is surjective iff
% the codomain of f is the image of f's domain (under f).

surjective(Function):-
  codomain(Function, Codomain),
  domain(Function, Domain),
  image(Domain, Function, Codomain).

