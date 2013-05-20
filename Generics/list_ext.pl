:- module(
  list_ext,
  [
    after/3, % ?X
             % ?Y
             % +L
    before/3, % ?X
              % ?Y
              % +L
    combination/2, % +Lists:list(list(term))
                   % -Combination:list(term)
    combination/3, % +List:list(term)
                   % +Length:integer
                   % -Combination:list(term)
    delete_list/3, % +List:list,
                   % +Delete:list,
                   % -NewList:list
    element_cut/4, % +L:list,
                   % +Element,
                   % -L1:list,
                   % -L2:list
    first/2, % +L:list,
             % ?First
    first/3, % +L:list,
             % +N:integer
             % -Firsts:list
    length_cut/4, % +L:list
                  % +Cut:integer
                  % -L1:list
                  % -L2:list
    list_replace/3, % +List:list
                    % +Replacements:list(term-term)
                    % -NewList:list
    list_to_ordered_pairs/2, % +L:list
                             % -Pairs:ordset(ordset)
    member/3, % ?Element1
              % ?Element2
              % ?List
    nth_minus_0/3, % +I:integer
                   % +L:list
                   % -Element:term
    nth_minus_1/3, % +I:integer
                   % +L:list
                   % -Element:term
    pair/3, % ?Pair
            % ?Former
            % ?Latter
    pairs/3, % ?Pairs:list
             % ?Formers:list
             % ?Latters:list
    random_member/2, % +List:list
                     % -Member
    repeating_list/3, % +Term:term
                      % +Repeats:integer
                      % -List:list(object)
    remove_first/2, % +List:list,
                    % -NewList:list
    remove_firsts/2, % +List:list
                     % -NewList:list
    remove_last/2, % +List:list
                   % -NewList:list
    shorter/3, % +Comparator:pred
               % +List1:list(object)
               % +List2:list(object)
    split_list_by_number_of_sublists/3, % +List:list
                                        % +NumberOfSublists:integer
                                        % -Sublists:list(list)
    split_list_by_size/3, % +List:list
                          % +SizeOfSublists:integer
                          % -Sublists:list(list)
    strict_sublist/2, % ?SubList:list
                      % +List:list
    sublist/2, % ?SubList:list
               % +List:list

% SORTING
    sort/3 % +Options:list(nvpair)
           % +:List:list
           % -Sorted:list
  ]
).

/** <module> LIST_EXT

Extra list functions for use in SWI-Prolog.

@author Wouter Beek
@version 2011/08-2012/02, 2012/09-2012/10, 2012/12, 2013/03, 2013/05
*/

:- use_module(math(math_ext)).



%! after(?X, ?Y, +L:list) is nondet.
% X appears after Y in the given list.

after(X, Y, L):-
  before(Y, X, L).

%! before(?X, ?Y, +L:list) is nondet.
% X appears before Y in the given list.

before(X, Y, L):-
  nth0(I, L, X),
  nth0(J, L, Y),
  I < J.

%! combination(+Lists:list(list(term)), -Combination:list(term)) is nondet.
% Returns a combination of items from the given lists.
%
% @arg Lists A list of lists of terms.
% @arg Combination A list of terms.

combination([], []).
combination([List | Lists], [H | T]):-
  member(H, List),
  combination(Lists, T).

combination(_List, 0, []):-
  !.
combination(List, Length, [H | T]):-
  member(H, List),
  succ(Length, NewLength),
  combination(List, NewLength, T).

%! delete_list(+List:list, +Deletes:list, -NewList:list) is det.
% Deletes the second list from the first one, returning the third.
%
% @arg List Then list from which elements are deleted.
% @arg Deletes The list of element that are deleted.
% @arg NewList The result of deleting the given elements from the given
%    list.
% @see Allows an arbitrary number of applications of delete/3.

delete_list(NewList, [], NewList).
delete_list(List, [Delete | Deletes], NewList):-
  delete(List, Delete, TempNewList),
  delete_list(TempNewList, Deletes, NewList).

%! element_cut(+L:list, +Element:atom, -L1:list, -L2:list) is det.
% Cuts the given list at the given element, returning the two cut lists.
% The cut element is itself not part of any of the results.
%
% @arg L The list that is to be cut.
% @arg Element The element at which the cut is made.
% @arg L2 The list of elements that occur before the cut.
% @arg L2 The list of elements that occur after the cut.

element_cut([], _Element, [], []):-
  !.
element_cut([Element | T], Element, [], T):-
  !.
element_cut([OtherElement | L], Element, [OtherElement | L1], L2):-
  element_cut(L, Element, L1, L2).

%! first(+List:list, ?Element:term) is semidet.
% Succeeds if the given element is the head of the given list.
% Fails if the list has no head.
%
% @arg List Any list.
% @arg Element The head element of the list, if any.
% @see This is the inverse of the default method last/2.

first([Element | _List], Element).

%! first(+L:list, +N:integer, -First:list) is det.
% Returns the first N element from list L, if these are present.
% This never fails but returns a list of length $0 < l(F) < N$ in case
% $l(L) < N$.
%
% @arg L The given list.
% @arg N The length of the returned sublist.
% @arg First The prepended sublist of =L=.

first(L, N, First):-
  length_cut(L, N, First, _L2).

%! length_cut(+L:list, +Cut:integer, -L1:list, -L2:list) is det.
% Cuts the given list in two sublists, where the former sublist
% has the given length.
%
% @arg L The full list.
% @arg Cut An integer indicating the length of the former sublist.
% @arg L1 The sublist that is the beginning of =L= with length =Cut=.
% @arg L2 The sublist that remains after =L1= has been removed from =L=.

length_cut(L, Cut, L, []):-
  length(L, N),
  N < Cut,
  !.
length_cut(L, Cut, L1, L2):-
  length(L1, Cut),
  append(L1, L2, L).

%! list_replace(
%!   +List:list,
%!   +Replacements:list(term-term),
%!   -NewList:list
%! ) is det.
% Returns the given list in which the given replacements have been made.
%
% @arg List The original list.
% @arg Replacements A list of replacements of the form =|term-term|=.
% @arg NewList The list in which all replacants have been replaced.

list_replace([], _Replacements, []).
list_replace([Replacant | T], Replacements, NewList):-
  member(Replacant-Replacer, Replacements),
  !,
  list_replace(T, Replacements, NewT),
  (
    is_list(Replacer)
  ->
    append(Replacer, NewT, NewList)
  ;
    NewList = [Replacer | NewT]
  ).
list_replace([H | T], Replacements, [H | NewT]):-
  list_replace(T, Replacements, NewT).

%! list_to_ordered_pairs(+L:list, -Pairs:ordset(ordset)) is det.
% Returns the ordered list of ordered pairs that occur in the given list.
%
% @arg L The given list.
% @arg Pairs An ordered list of ordered pairs.

list_to_ordered_pairs([], []):-
  !.
% The pairs need to be internally ordered, but the order in which the
% pairs occur is immaterial.
list_to_ordered_pairs([H | T], S):-
  list_to_orderd_pairs_(H, T, S1),
  list_to_ordered_pairs(T, S2),
  append(S1, S2, S).

list_to_orderd_pairs_(_H, [], []):-
  !.
list_to_orderd_pairs_(H, [T | TT], [Pair | S]):-
  list_to_ord_set([H, T], Pair),
  list_to_orderd_pairs_(H, TT, S).

%! member(X, Y, L) is nondet.
% Pairs from a list.
%
% @arg X The first argument of the pair.
% @arg Y The second argument of the pair.
% @arg L The list from which pairs are taken.

member(X, Y, L):-
  member(X, L),
  member(Y, L).

%! nth_minus_0(+I:integer, +L:list, -Element) is det.
% Succeeds if the given element occurs at =|length(List) - I|= in list =L=.
%
% @arg I The index, an integer in =|[0, length(List) - 1]|=.
% @arg L Any list.
% @arg Element An element occurring in the given list.
% @see The inverse of default method nth0/3.

nth_minus_0(I, L, Element):-
  reverse(L, RevL),
  nth0(I, RevL, Element).

%! nth_minus_1(-I:integer, +L:list, +Element) is semidet.
% Succeeds if the given element occurs at =|length(L) - I|= in list =L=.
%
% @arg I The index, an integer in =|[0, length(List)]|=.
% @arg L Any list.
% @arg Element An element occurring in the given list.
% @see The inverse of default method nth1/3.

nth_minus_1(I, L, Element):-
  reverse(L, RevL),
  nth1(I, RevL, Element).

%! pair(-Pair, +Former, +Latter) is det.
%! pair(+Pair, -Former, -Latter) is det.
% Supporting various pair formats.

pair(X1/X2, X1, X2).
pair(X1-X2, X1, X2).
pair([X1,X2], X1, X2).

%! pairs(+Pairs:list, -Formers:list, -Latters:list) is det.
%! pairs(-Pairs:list, +Formers:list, +Latters:list) is det.

pairs([], [], []).
pairs([Pair | Pairs], [X1 | T1], [X2 | T2]):-
  pair(Pair, X1, X2),
  !,
  pairs(Pairs, T1, T2).

%! random_member(+List:list, -Member) is det.
% Returns a randomly chosen member from the given list.
%
% @arg List
% @arg Member

random_member(List, Member):-
  length(List, Length),
  random_betwixt(Length, Random),
  nth0(Random, List, Member).

%! repeating_list(+Term:term, +Repeats:integer, -List:list(term)) is det.
% Returns the list of the given number of repeats of the given term.
%
% @arg Term
% @arg Repeats
% @arg List

repeating_list(Object, Repeats, List):-
  nonvar(List),
  !,
  repeating_list1(Object, Repeats, List).
repeating_list(Object, Repeats, List):-
  nonvar(Repeats),
  !,
  repeating_list2(Object, Repeats, List).

%! repeating_list1(-Object, -Repeats:integer, +List:list) is nondet.
% Returns the object and how often it occurs in the repeating list.

repeating_list1(_Object, 0, []).
repeating_list1(Object, Repeats, [Object | T]):-
  forall(
    member(X, T),
    X = Object
  ),
  length([Object | T], Repeats).

%! repeating_list2(+Object, +Repeats:integer, -List:list) is det.

repeating_list2(_Object, 0, []):-
  !.
repeating_list2(Object, Repeats, [Object | List]):-
  succ(NewRepeats, Repeats),
  repeating_list2(Object, NewRepeats, List).

%! remove_first(+List, -ListWithoutFirst)
% Returns a list that is like the given list, but without the first element.
% Fails if there is no first element in the given list.
%
% @arg List Any nonempty list.
% @arg ListWithoutFirst A new list without the first elemnent.
% @see The inverse of remove_last/2.

remove_first([_First | ListWithoutFirst], ListWithoutFirst).

%! remove_firsts(+Lists, -ListsWithoutFirst)
% Returns the given lists without the first elements.
%
% @arg Lists Any list of non-empty lists.
% @arg ListsWithoutFirst ...
% @see Uses remove_first/2.

remove_firsts([], []).
remove_firsts(
  [List | Lists],
  [ListWithoutFirst | ListsWithoutFirst]
):-
  remove_first(List, ListWithoutFirst),
  remove_firsts(Lists, ListsWithoutFirst).

%! remove_last(+List, -NewList)
% Returns the given list with the last element removed.
%
% @arg List The original list.
% @arg NewList The original list with the last element remove.
% @see The inverse of remove_first/2.

remove_last([], []).
remove_last([Element], []):-
  atomic(Element).
remove_last([Element | Rest], [Element | NewRest]):-
  remove_last(Rest, NewRest).

%! shorter(+Order:pred/2, +List:list(term), +List2:list(term)) is semidet.
% Succeeds if =List1= has relation =Order= to =List2=.
%
% @arg Order A binary predicate. Either <, =, or >.
% @arg List1 A list of objects.
% @arg List2 A list of objects.

shorter(Order, List1, List2):-
  length(List1, Length1),
  length(List2, Length2),
  compare(Order, Length2, Length1).

split_list_by_number_of_sublists(List, NumberOfSublists, Sublists):-
  length(List, Length),
  succ(ReducedNumberOfSublists, NumberOfSublists),
  SizeOfSublists is Length div ReducedNumberOfSublists,
  split_list_by_size(List, SizeOfSublists, Sublists).

% The last sublists is exactly of the requested size.
% The empty list indicates this.
split_list_by_size([], _SizeOfSublists, []):-
  !.
split_list_by_size(List, SizeOfSublists, [Sublist | Sublists]):-
  length(Sublist, SizeOfSublists),
  append(Sublist, NewList, List),
  !,
  split_list_by_size(NewList, SizeOfSublists, Sublists).
% The last sublist is not exactly of the requested size. Give back
% what remains.
split_list_by_size(LastSublist, _SizeOfSublists, [LastSublist]).

%! sublist(?SubList:list, +List:list) is nondet.
% Returns sublists of the given list.

sublist([], []).
sublist([H | SubT], [H | T]):-
  sublist(SubT, T).
sublist(SubT, [_H | T]):-
  sublist(SubT, T).



% SORTING %

%! i(?Order1, ?Order2) is nondet.
% Inverter of order relations.
% This is used for sorting with the =inverted= option set to =true=.
%
% @arg Order1 An order relation, i.e. <, > or =.
% @arg Order1 An order relation, i.e. <, > or =.

i(<, >).
i(>, <).
i(=, =).

%! icompare(?InvertedOrder, @Term1, @Term2) is det.
% Determine or test the order between two terms in the inversion of the
% standard order of terms.
% This allows inverted sorting, useful for some algorithms that can
% operate (more) quickly on inversely sorted operations, without the
% cost of reverse/2.
% This is used for sorting with the =inverted= option set to =true=.
%
% @arg InvertedOrder One of <, > or =.
% @arg Term1
% @arg Term2
% @see compare/3 using uninverted order predicates.

icompare(InvertedOrder, Term1, Term2):-
  compare(Order, Term1, Term2),
  i(Order, InvertedOrder).

%! sort(+Options:list(nvpair), +List:list, -Sorted:list) is det.
% @arg Options A list of name-value pairs. The following options are
%        supported:
%        1. =|duplicates(boolean)|= Whether duplicate elements are retained
%           in the sorted list.
%        2. =|inverted(boolean)|= Whether the sorted list goes from lowest to
%           highest (standard) or from highest to lowest.

% The combination of _inverted_ and _|no duplicates|_ uses a dedicated
% comparator. This is cheaper that first sorting and then reversing the
% results.
sort(Options, List, Sorted):-
  option(inverted(true), Options, false),
  option(duplicates(false), Options, false),
  !,
  predsort(icompare, List, Sorted).
sort(Options, List, Sorted):-
  (
    option(duplicates(true), Options, false)
  ->
    msort(List, Sorted0)
  ;
    sort(List, Sorted0)
  ),
  (
    option(inverted(true), Options, false)
  ->
    reverse(Sorted0, Sorted)
  ;
    Sorted = Sorted0
  ).

strict_sublist(SubList, List):-
  sublist(SubList, List),
  SubList \== List.

