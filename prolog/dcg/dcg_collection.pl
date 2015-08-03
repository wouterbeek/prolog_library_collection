:- module(
  dcg_collection,
  [
    collection//6, % :Begin
                   % :End
                   % :Ordering
                   % :Separator
                   % :ElementWriter
                   % +Elements:list
    list//2, % :ElementWriter
             % +Elements:list
    nvpair//3, % :ElementWriter
               % +Name
               % +Value
    pair//3, % +Mode:oneof([ascii,html])
             % :ElementWriter
             % +Pair:pair
    pair//3, % :ElementWriter
             % +Element1
             % +Element2
    set//2, % :ElementWriter
            % +Elements:list
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
  - Collections may contain other collections as their elements.
  - Arbitrary DCGs generate the `Begin` and `End` of the collection.
  - An arbitrary DCG separates elements in the collection, i.e. `Separator`.
  - Each element is written with the same DCG rule `ElementWriter`.

For convenience's sake, the following collection instances are supported:
  - List
  - Name-value pair
  - Pair
  - Set
  - Tuple

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(default)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- meta_predicate(collection(//,//,2,//,3,+,?,?)).
:- meta_predicate(collection_items(//,//,2,//,3,+,?,?)).
:- meta_predicate(list(3,+,?,?)).
:- meta_predicate(nvpair(3,+,+,?,?)).
:- meta_predicate(pair(+,3,+,?,?)).
:- meta_predicate(pair(3,+,+,?,?)).
:- meta_predicate(set(3,+,?,?)).
:- meta_predicate(triple(3,+,+,+,?,?)).
:- meta_predicate(tuple(3,+,?,?)).





%! collection(
%!   :Begin,
%!   :End,
%!   :Ordering,
%!   :Separator,
%!   :ElementWriter,
%!   +Elements:list
%! )// is det.
% Generates a represention of the collection of given elements.
%
% @arg Begin DCG rule that is called before the first element.
% @arg End DCG rule that is called after the last element.
% @arg Ordering Binary predicate that orders the given elements.
% @arg Separator DCG rules that is called in between each two elements.
% @arg ElementWriter Unary DCG rule that writes a single element.
% @arg Elements A list of ground terms that denote
%      the members of the collection.

collection(Begin, End, Ord, Sep, Writer, L0) -->
  % Allow an arbitrary ordering to be enforced, e.g. list_to_set/2.
  {
    is_list(L0), !,
    defval(=, Ord),
    once(call(Ord, L0, L))
  },
  dcg_between(
    Begin,
    collection_items(Begin, End, Ord, Sep, Writer, L),
    End
  ).
collection(_, _, _, _, Writer, X) -->
  dcg_call_cp(Writer, X).

collection_items(_, _, _, _, _, []) --> !, "".
collection_items(Begin, End, Ord, Sep, Writer, [H]) --> !,
  collection(Begin, End, Ord, Sep, Writer, H).
collection_items(Begin, End, Ord, Sep, Writer, [H|T]) -->
  collection(Begin, End, Ord, Sep, Writer, H),
  Sep,
  collection_items(Begin, End, Ord, Sep, Writer, T).



%! list(:ElementWriter, +Elements:list)// is det.
% Lists are printed recursively, using indentation relative to the given
% indentation level.

list(Writer, L) -->
  bracketed_collection(square, =, comma, Writer, L).



%! nvpair(:ElementWriter, +Name, +Value)// is det.

nvpair(Writer, N, V) -->
  collection(dcg_void, semi_colon, =, nvpair_sep, Writer, [N,V]).

nvpair_sep --> ": ".



%! pair(:ElementWriter, +Pairs:pair)// is det.

pair(Writer, E1-E2) -->
  pair(Writer, E1, E2).



%! pair(:ElementWriter, +Element1, +Element2)// is det.
% Prints the given pair.

pair(Writer, E1, E2) -->
  bracketed_collection(langular, =, comma, Writer, [E1,E2]).



%! set(:ElementWriter, +Elements:list)// is det.

set(Writer, L) -->
  bracketed_collection(curly, list_to_ord_set, comma, Writer, L).



%! triple(:ElementWriter, +Element1, +Element2, +Element3)// is det.

triple(Writer, E1, E2, E3) -->
  tuple(Writer, [E1,E2,E3]).



%! tuple(:ElementWriter, +Elements:list)// is det.
% Prints a tuple.

tuple(Writer, L) -->
  bracketed_collection(langular, =, comma, Writer, L).





% HELPERS %

bracketed_collection(Type, Ord, Sep, Writer, L) -->
  collection(
    opening_bracket(Type),
    closing_bracket(Type),
    Ord,
    Sep,
    Writer,
    L
  ).
