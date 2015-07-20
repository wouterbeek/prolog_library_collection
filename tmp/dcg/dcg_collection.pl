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
    pair//4, % +Mode:oneof([ascii,html])
             % :ElementWriter
             % +Element1
             % +Element2
    set//2, % :ElementWriter
            % +Elements:list
    triple//5, % +Mode:oneof([ascii,html])
               % :ElementWriter
               % +Element1
               % +Element2
               % +Element3
    tuple//3 % +Mode:oneof([ascii,html])
             % :ElementWriter
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
@version 2013/07-2013/09, 2013/11-2014/01 2014/03, 2014/05, 2015/01, 2015/03,
         2015/05
*/

:- use_module(library(option)).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_meta)).
:- use_module(plc(generics/meta_ext)).
:- use_module(plc(generics/option_ext)).

:- meta_predicate(collection(//,//,2,//,3,+,?,?)).
:- meta_predicate(collection_items(//,//,2,//,3,+,?,?)).
:- meta_predicate(list(3,+,?,?)).
:- meta_predicate(nvpair(3,+,+,?,?)).
:- meta_predicate(pair(+,3,+,?,?)).
:- meta_predicate(pair(+,3,+,+,?,?)).
:- meta_predicate(set(3,+,?,?)).
:- meta_predicate(triple(+,3,+,+,+,?,?)).
:- meta_predicate(tuple(+,3,+,?,?)).





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

collection(Begin, End, Ordering, Separator, ElementWriter, L1) -->
  % Allow an arbitrary ordering to be enforced, e.g. list_to_set/2.
  {
    is_list(L1), !,
    defval(=, Ordering),
    once(call(Ordering, L1, L2))
  },
  dcg_between(
    Begin,
    collection_items(Begin, End, Ordering, Separator, ElementWriter, L2),
    End
  ).
collection(_, _, _, _, ElementWriter, Element) -->
  dcg_call_cp(ElementWriter, Element).

collection_items(_, _, _, _, _, []) --> !, "".
collection_items(Begin, End, Ordering, Separator, ElementWriter, [H]) --> !,
  collection(Begin, End, Ordering, Separator, ElementWriter, H).
collection_items(Begin, End, Ordering, Separator, ElementWriter, [H|T]) -->
  collection(Begin, End, Ordering, Separator, ElementWriter, H),
  Separator,
  collection_items(Begin, End, Ordering, Separator, ElementWriter, T).



%! list(:ElementWriter, +Elements:list)// is det.
% Lists are printed recursively, using indentation relative to the given
% indentation level.

list(ElementWriter, Elements) -->
  collection(`[`, `]`, =, comma, ElementWriter, Elements).



%! nvpair(:ElementWriter, +Name, +Value)// is det.

nvpair(ElementWriter, Name, Value) -->
  collection(``, `;`, =, nvpair_sep, ElementWriter, [Name,Value]).

nvpair_sep -->
  ": ".



%! pair(+Mode:oneof([ascii,html]), :ElementWriter, +Pairs:pair)// is det.

pair(Mode, ElementWriter, E1-E2) -->
  pair(Mode, ElementWriter, E1, E2).


%! pair(
%!   +Mode:oneof([ascii,html]),
%!   :ElementWriter,
%!   +Element1,
%!   +Element2
%! )// is det.
% Prints the given pair.
%
% @arg Mode The kind of brackets that are printed for this pair.
%      Either `ascii` for using the ASCII characters `<` and `>` (default)
%      or `html` for using the HTML escape sequences `&lang;` and `&rang;`.
% @arg ElementWriter
% @arg Element1
% @arg Element2

pair(Mode, ElementWriter, E1, E2) -->
  {defval(ascii, Mode)},
  collection(langle(Mode), rangle(Mode), =, comma, ElementWriter, [E1,E2]).



%! set(:ElementWriter, +Elements:list)// is det.

set(ElementWriter, L) -->
  collection(`{`, `}`, list_to_ord_set, comma, ElementWriter, L).



%! triple(
%!   +Mode:oneof([ascii,html]),
%!   :ElementWriter,
%!   +Element1,
%!   +Element2,
%!   +Element3
%! )// is det.

triple(Mode, Writer, E1, E2, E3) -->
  tuple(Mode, Writer, [E1,E2,E3]).



%! tuple(+Mode:oneof([ascii,html]), :ElementWriter, +Elements:list)// is det.
% Prints a tuple.
%
% @arg Mode The kind of brackets that are printed for this pair.
%      Either `ascii` for using the ASCII characters `<` and `>` (default)
%      or `html` for using the HTML escape sequences `&lang;` and `&rang;`.
% @arg ElementWriter
% @arg Elements

tuple(Mode, ElementWriter, L) -->
  {defval(ascii, Mode)},
  collection(langle(Mode), rangle(Mode), =, comma, ElementWriter, L).





% HELPERS %

langle(ascii) --> "<".
langle(html) --> "&lang;".

rangle(ascii) --> ">".
rangle(html) --> "&rang;".
