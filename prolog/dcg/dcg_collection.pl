:- module(
  dcg_collection,
  [
    collection//5, % :Begin_0
                   % :End_0
                   % :Separator_0
                   % :Writer_1
                   % +Elements:list
    list//1, % +Elements:list
    list//2, % :Writer_1
             % +Elements:list
    pair//2, % +Element1
             % +Element2
    pair//3, % :Writer_1
             % +Element1
             % +Element2
    quadruple//4, % +Element1
                  % +Element2
                  % +Element3
                  % +Element4
    quadruple//5, % :Writer_1
                  % +Element1
                  % +Element2
                  % +Element3
                  % +Element4
    set//1, % +Elements:ordset
    set//2, % :Writer_1
            % +Elements:ordset
    triple//3, % +Element1
               % +Element2
               % +Element3
    triple//4, % :Writer_1
               % +Element1
               % +Element2
               % +Element3
    tuple//1, % +Elements:list
    tuple//2 % :Writer_1
             % +Elements:list
  ]
).

/** <module> DCG collection

DCG rules for generating collections.
This module supports the following collection properties:
  * Collections may contain other collections as their elements.
  * Arbitrary DCGs generate the `Begin` and `End` of the collection.
  * An arbitrary DCG separates elements in the collection, i.e. `Separator`.
  * Each element is written with the same DCG rule `ElementWriter`.

For convenience's sake, the following collection instances are predefined:
  * List
  * Pair
  * Set
  * Tuple

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_pl_term)).
:- use_module(library(dcg/dcg_unicode)).

:- meta_predicate(bracketed_collection(+,//,3,+,?,?)).
:- meta_predicate(collection(//,//,//,3,+,?,?)).
:- meta_predicate(collection_item(3,+,?,?)).
:- meta_predicate(collection_items(//,//,//,3,+,?,?)).
:- meta_predicate(list(3,+,?,?)).
:- meta_predicate(pair(3,+,?,?)).
:- meta_predicate(pair(3,+,+,?,?)).
:- meta_predicate(quadruple(3,+,+,+,+,?,?)).
:- meta_predicate(set(3,+,?,?)).
:- meta_predicate(triple(3,+,+,+,?,?)).
:- meta_predicate(tuple(3,+,?,?)).





%! collection(
%!   :Begin_0,
%!   :End_0,
%!   :Separator_0,
%!   :Writer_1,
%!   +Elements:list
%! )// is det.
% Generates a represention of the collection of given elements.
%
% @arg Begin_0 DCG rule that is called before the first element.
% @arg End_0 DCG rule that is called after the last element.
% @arg Separator_0 DCG rules that is called in between each two elements.
% @arg Writer_1 Unary DCG rule that writes a single element.
% @arg Elements A list of ground terms that denote
%      the members of the collection.

collection(Begin_0, End_0, Sep_0, Writer_1, L) -->
  {must_be(list, L)},
  dcg_between(
    Begin_0,
    collection_items(Begin_0, End_0, Sep_0, Writer_1, L),
    End_0
  ).

collection_items(_, _, _, _, []) --> !, [].
collection_items(_, _, _, Writer_1, [H]) --> !,
  collection_item(Writer_1, H).
collection_items(Begin_0, End_0, Sep_0, Writer_1, [H|T]) -->
  collection_item(Writer_1, H),
  Sep_0,
  collection_items(Begin_0, End_0, Sep_0, Writer_1, T).

collection_item(Writer_1, L) -->
  {is_list(L)}, !,
  list(Writer_1, L).
collection_item(Writer_1, X) -->
  dcg_call(Writer_1, X).



%! list(+Elements:list)// is det.
% Wrapper around list//2 using the default writer.

list(L) -->
  list(pl_term, L).


%! list(:Writer_1, +Elements:list)// is det.

list(Writer_1, L) -->
  bracketed_collection(square, comma, Writer_1, L).



%! pair(+Element1, +Element2)// is det.
% Wrapper around pair//3 using the default writer.

pair(E1, E2) -->
  pair(pl_term, E1, E2).


%! pair(:Writer_1, +Element1, +Element2)// is det.
% Prints the given pair.

pair(Writer_1, E1, E2) -->
  bracketed_collection(langular, comma, Writer_1, [E1,E2]).



%! quadruple(+Element1, +Element2, +Element3, +Element4)// is det.
% Wrapper around quadruple//4 using the default writer..

quadruple(E1, E2, E3, E4) -->
  quadruple(pl_term, E1, E2, E3, E4).


%! quadruple(:Writer_1, +Element1, +Element2, +Element3, +Element4)// is det.

quadruple(Writer_1, E1, E2, E3, E4) -->
  tuple(Writer_1, [E1,E2,E3,E4]).



%! set(+Elements:ordset)// is det.
% Wrapper around set//2 using the default writer..

set(S) -->
  set(pl_term, S).


%! set(:Writer_1, +Elements:ordset)// is det.

set(Writer_1, S) -->
  bracketed_collection(curly, comma, Writer_1, S).



%! triple(+Element1, +Element2, +Element3)// is det.
% Wrapper around triple//4 using the default writer.

triple(E1, E2, E3) -->
  triple(pl_term, E1, E2, E3).


%! triple(:Writer_1, +Element1, +Element2, +Element3)// is det.

triple(Writer_1, E1, E2, E3) -->
  tuple(Writer_1, [E1,E2,E3]).



%! tuple(+Elements:list)// is det.
% Wrapper around tuple//2 using the default writer.

tuple(L) -->
  tuple(pl_term, L).


%! tuple(:Writer_1, +Elements:list)// is det.
% Prints a tuple.

tuple(Writer_1, L) -->
  bracketed_collection(langular, comma, Writer_1, L).





% HELPERS %

%! bracketed_collection(
%!   +Type:atom,
%!   :Sep_0,
%!   :Writer_1,
%!   +Elements:list
%! )// is det.

bracketed_collection(Type, Sep, Writer, L) -->
  collection(
    opening_bracket(Type, _),
    closing_bracket(Type, _),
    Sep_0,
    Writer_1,
    L
  ), !.
