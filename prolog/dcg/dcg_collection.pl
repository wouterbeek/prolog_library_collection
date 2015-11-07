:- module(
  dcg_collection,
  [
    list//1, % +Elements:list
    list//2, % :Writer_1
             % +Elements:list
    pair//2, % :X_0
             % :Y_0
    quadruple//4, % :X_0
                  % :Y_0
                  % :Z_0
                  % :Q_0
    set//1, % +Elements:list
    set//2, % :Writer_1
            % +Elements:list
    triple//3, % :X_0
               % :Y_0
               % :Z_0
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
@version 2015/08, 2015/10-2015/11
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_unicode)).

:- meta_predicate(collection0(3,+,?,?)).
:- meta_predicate(collection0(3,//,+,?,?)).
:- meta_predicate(list(3,+,?,?)).
:- meta_predicate(pair(//,//,?,?)).
:- meta_predicate(quadruple(//,//,//,//,?,?)).
:- meta_predicate(set(3,+,?,?)).
:- meta_predicate(triple(//,//,//,?,?)).
:- meta_predicate(tuple(3,+,?,?)).





%! list(+Elements:list)// is det.
% Wrapper around list//2 using the default writer.

list(L) -->
  list(pl_term, L).


%! list(:Writer_1, +Elements:list)// is det.

list(Writer_1, L) -->
  "[", collection0(Writer_1, L), "]".



%! pair(:X_0, :Y_0)// is det.

pair(X_0, Y_0) -->
  "〈", X_0, ",", Y_0, "〉".



%! quadruple(:X_0, :Y_0, :Z_0, :Q_0)// is det.

quadruple(X_0, Y_0, Z_0, Q_0) -->
  "〈", X_0, ",", Y_0, ",", Z_0, ",", Q_0, "〉".



%! set(+Elements:ordset)// is det.
% Wrapper around set//2 using the default writer..

set(L) -->
  set(pl_term, L).


%! set(:Writer_1, +Elements:ordset)// is det.

set(Writer_1, L) -->
  "{", collection0(Writer_1, L), "}".



%! triple(:X_0, :Y_0, :Z_0)// is det.
% Wrapper around triple//4 using the default writer.

triple(X_0, Y_0, Z_0) -->
  "〈", X_0, ",", Y_0, ",", Z_0, "〉".



%! tuple(+Elements:list)// is det.
% Wrapper around tuple//2 using the default writer.

tuple(L) -->
  tuple(pl_term, L).


%! tuple(:Writer_1, +Elements:list)// is det.
% Prints a tuple.

tuple(Writer_1, L) -->
  "〈", collection0(Writer_1, L), "〉".





% HELPERS %

%! collection0(:Writer_1, +Elements:list)// is det.

collection0(Writer_1, L) -->
  collection0(Writer_1, comma, L).


%! Collection0(:Writer_1, :Separator_0, +Elements:list)// is det.

collection0(_, _, []) --> !, "".
collection0(Writer_1, _, [H]) --> !,
  dcg_call(Writer_1, H).
collection0(Writer_1, Sep_0, [H|T]) -->
  dcg_call(Writer_1, H),
  Sep_0,
  collection0(Writer_1, Sep_0, T).
