:- module(
  dcg_collection,
  [
    list//1,   % +L
    list//2,   % :Writer_3, +L
    pair//2,   % :X_2, :Y_2
    quad//4,   % :X_2, :Y_2, :Z_2, :Q_2
    set//1,    % +L
    set//2,    % :Writer_3, +L
    triple//3, % :X_2, :Y_2, :Z_2
    tuple//1,  % +L
    tuple//2   % :Writer_3, +L
  ]
).

/** <module> DCG collection

DCG rules for generating collections.
This module supports the following collection properties:
  - Collections may contain other collections as their elements.
  - Arbitrary DCGs generate the `Begin` and `End` of the collection.
  - An arbitrary DCG separates elements in the collection, i.e. `Separator`.
  - Each element is written with the same DCG rule `ElementWriter`.

For convenience's sake, the following collection instances are predefined:
  - List
  - Pair
  - Set
  - Tuple

@author Wouter Beek
@version 2015/08, 2015/10-2015/11, 2016/03
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_unicode)).

:- meta_predicate
    collection0(3, +, ?, ?),
    collection0(3, //, +, ?, ?),
    list(3, +, ?, ?),
    pair(//, //, ?, ?),
    quad(//, //, //, //, ?, ?),
    set(3, +, ?, ?),
    triple(//, //, //, ?, ?),
    tuple(3, +, ?, ?).





%! list(+L)// is det.
%! list(:Writer_3, +L)// is det.

list(L) -->
  list(term, L).


list(Writer_3, L) -->
  "[", collection0(Writer_3, L), "]".



%! pair(:X_2, :Y_2)// is det.

pair(X_2, Y_2) -->
  "〈", X_2, ",", Y_2, "〉".



%! quad(:X_2, :Y_2, :Z_2, :Q_2)// is det.

quad(X_2, Y_2, Z_2, Q_2) -->
  "〈", X_2, ",", Y_2, ",", Z_2, ",", Q_2, "〉".



%! set(+L)// is det.
%! set(:Writer_3, +L)// is det.

set(L) -->
  set(term, L).


set(Writer_3, L) -->
  "{", collection0(Writer_3, L), "}".



%! triple(:X_2, :Y_2, :Z_2)// is det.
% Wrapper around triple//4 using the default writer.

triple(X_2, Y_2, Z_2) -->
  "〈", X_2, ",", Y_2, ",", Z_2, "〉".



%! tuple(+L)// is det.
%! tuple(:Writer_3, +L)// is det.
% Prints a tuple.

tuple(L) -->
  tuple(term, L).


tuple(Writer_3, L) -->
  "〈", collection0(Writer_3, L), "〉".





% HELPERS %

%! collection0(:Writer_3, +L)// is det.
%! collection0(:Writer_3, :Sep_2, +L)// is det.

collection0(Writer_3, L) -->
  collection0(Writer_3, comma, L).


collection0(_, _, []) --> !, "".
collection0(Writer_3, _, [H]) --> !,
  dcg_call(Writer_3, H).
collection0(Writer_3, Sep_2, [H|T]) -->
  dcg_call(Writer_3, H),
  Sep_2,
  collection0(Writer_3, Sep_2, T).
