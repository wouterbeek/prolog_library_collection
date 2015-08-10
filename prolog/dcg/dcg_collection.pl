:- module(
  dcg_collection,
  [
    collection//5, % :Begin
                   % :End
                   % :Separator
                   % :ElementWriter
                   % +Elements:list
    list//2, % :ElementWriter
             % +Elements:list
    pair//2, % :ElementWriter
             % +Pair:pair
    pair//3, % :ElementWriter
             % +Element1
             % +Element2
    quadruple//5, % :ElementWriter
                  % +Element1
                  % +Element2
                  % +Element3
                  % +Element4
    set//2, % :ElementWriter
            % +Elements:ordset
    triple//4, % :ElementWriter
               % +Element1
               % +Element2
               % +Element3
    tuple//2 % :ElementWriter
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

For convenience's sake, the following collection instances are supported:
  * List
  * Name-value pair
  * Pair
  * Set
  * Tuple

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(default)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- meta_predicate(bracketed_collection(+,2,//,3,+,?,?)).
:- meta_predicate(collection(//,//,//,3,+,?,?)).
:- meta_predicate(collection_items(//,//,//,3,+,?,?)).
:- meta_predicate(list(3,+,?,?)).
:- meta_predicate(pair(3,+,?,?)).
:- meta_predicate(pair(3,+,+,?,?)).
:- meta_predicate(quadruple(3,+,+,+,+?,?)).
:- meta_predicate(set(3,+,?,?)).
:- meta_predicate(triple(3,+,+,+,?,?)).
:- meta_predicate(tuple(3,+,?,?)).





%! collection(
%!   :Begin,
%!   :End,
%!   :Separator,
%!   :ElementWriter,
%!   +Elements:list
%! )// is det.
% Generates a represention of the collection of given elements.
%
% @arg Begin DCG rule that is called before the first element.
% @arg End DCG rule that is called after the last element.
% @arg Separator DCG rules that is called in between each two elements.
% @arg ElementWriter Unary DCG rule that writes a single element.
% @arg Elements A list of ground terms that denote
%      the members of the collection.

collection(Begin, End, Sep, Writer, L) -->
  {must_be(list, L)},
  dcg_between(
    Begin,
    collection_items(Begin, End, Sep, Writer, L),
    End
  ).
collection(_, _, _, Writer, X) -->
  dcg_call_cp(Writer, X).

collection_items(_, _, _, _, []) --> !, [].
collection_items(Begin, End, Sep, Writer, [H]) --> !,
  collection(Begin, End, Sep, Writer, H).
collection_items(Begin, End, Sep, Writer, [H|T]) -->
  collection(Begin, End, Sep, Writer, H),
  Sep,
  collection_items(Begin, End, Sep, Writer, T).



%! list(:ElementWriter, +Elements:list)// is det.
% Lists are printed recursively, using indentation relative to the given
% indentation level.

list(Writer, L) -->
  bracketed_collection(square, comma, Writer, L).



%! pair(:ElementWriter, +Pairs:pair)// is det.

pair(Writer, E1-E2) -->
  pair(Writer, E1, E2).



%! pair(:ElementWriter, +Element1, +Element2)// is det.
% Prints the given pair.

pair(Writer, E1, E2) -->
  bracketed_collection(langular, comma, Writer, [E1,E2]).



%! quadruple(
%!   :ElementWriter,
%!   +Element1,
%!   +Element2,
%!   +Element3,
%!   +Element4
%! )// is det.

quadruple(Writer, E1, E2, E3, E4) -->
  tuple(Writer, [E1,E2,E3,E4]).



%! set(:ElementWriter, +Elements:ordset)// is det.

set(Writer, S) -->
  bracketed_collection(curly, comma, Writer, S).



%! triple(:ElementWriter, +Element1, +Element2, +Element3)// is det.

triple(Writer, E1, E2, E3) -->
  tuple(Writer, [E1,E2,E3]).



%! tuple(:ElementWriter, +Elements:list)// is det.
% Prints a tuple.

tuple(Writer, L) -->
  bracketed_collection(langular, comma, Writer, L).





% HELPERS %

bracketed_collection(Type, Sep, Writer, L) -->
  collection(
    opening_bracket(Type, _),
    closing_bracket(Type, _),
    Sep,
    Writer,
    L
  ).
