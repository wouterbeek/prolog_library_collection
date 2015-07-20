:- module(
  list_ext,
  [
    after/3, % ?After
             % ?Before
             % ?List:list
    append_intersperse/3, % +List:list
                          % +Separator
                          % -NewList:list
    before/3, % ?Before
              % ?After
              % ?List:list
    combination/2, % +Lists:list(list)
                   % -Combination:list
    common_list_prefix/3, % +List1:list
                          % +List2:list
                          % ?Sublist:list
    complement_list/4, % +FromList:list
                       % +Length:nonneg
                       % +FillElement
                       % -ToList:list
    directly_after/3, % ?After
                      % ?Before
                      % ?List:list
    directly_before/3, % ?After
                       % ?Before
                       % ?List:list
    element_cut/4, % +List:list
                   % +Element
                   % -List1:list
                   % -List2:list
    first/2, % +List:list
             % ?First
    first/3, % +List:list
             % +N:integer
             % -Firsts:list
    first_duplicate/2, % ?FirstDuplicate
                       % +List:list
    inflist/2, % +Element
               % -List
    length_cut/4, % +L:list
                  % +Cut:nonneg
                  % -L1:list
                  % -L2:list
    list_binary_term/3, % ?List:list
                            % ?Operator
                            % ?Term:compound
    list_replace/3, % +List:list
                    % +Replacements:list(pair)
                    % -NewList:list
    list_separator_concat/3, % +Lists:list(list)
                             % +Separator:list
                             % ?List:list
    list_truncate/3, % +List:list
                     % +MaximumLength:nonneg
                     % -TruncatedList:list
    member/3, % ?Element1
              % ?Element2
              % ?List:list
    member_default/3, % +Member
                      % +List:list
                      % +Default
    nth0_minus/3, % ?Index:nonneg
                  % ?List:list
                  % ?Element:term
    nth0_minus/4, % ?Index:nonneg
                  % ?List:list
                  % ?Element:term
                  % ?Rest:list
    nth1_minus/3, % ?Index:nonneg
                  % ?List:list
                  % ?Element:term
    nth1_minus/4, % ?Index:nonneg
                  % ?List:list
                  % ?Element:term
                  % ?Rest:list
    nth0chk/3, % ?Index:nonneg
               % ?List:List
               % ?Element
    nth0chk/4, % ?Index:nonneg
               % ?List:List
               % ?Element
               % ?Rest:list
    nth1chk/3, % ?Index:nonneg
               % ?List:List
               % ?Element
    nth1chk/4, % ?Index:nonneg
               % ?List:List
               % ?Element
               % ?Rest:list
    postfix/2, % ?Part:list
               % ?Whole:list
    random_sublist/2, % -Sublist:list
                      % +List:list
    random_sublist/3, % -Sublist:list
                      % +Length:nonneg
                      % +List:list
    remove_sublists/2, % +Lists1:list(list)
                       % -Lists2:list(list)
    repeating_list/3, % ?X
                      % ?N:nonneg
                      % ?L:list
    replace_nth/6, % +StartIndex:index
                   % ?Index:index
                   % +OldList:list
                   % ?OldElement
                   % +NewElement
                   % -NewList:list
    replace_nth0/5, % ?Index:nonneg
                    % +OldList:list
                    % ?OldElement
                    % +NewElement
                    % -NewList:list
    replace_nth1/5, % ?Index:positive_integer
                    % +OldList:list
                    % ?OldElement
                    % +NewElement
                    % -NewList:list
    selectchk_eq/3, % +Element
                    % +List:list
                    % -Rest:list
    shorter/3, % +Comparator:pred
               % +List1:list
               % +List2:list
    split_list_by_number_of_sublists/3, % +List:list
                                        % +NumberOfSublists:nonneg
                                        % -Sublists:list(list)
    split_list_by_size/3, % +List:list
                          % +SizeOfSublists:integer
                          % -Sublists:list(list)
    split_list_exclusive/3, % +List:list
                            % +Split:list
                            % -Sublists:list(list)
    strict_sublist/2, % ?SubList:list
                      % +List:list
    sublist/2 % ?SubList:list
              % +List:list
  ]
).
:- reexport(library(lists)).

/** <module> List extensions

Extensions to the set of list predicates in SWI-Prolog.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(apply)).
:- use_module(library(closure)).
:- use_module(library(error)).
:- use_module(library(random)).
:- use_module(library(typecheck)).





%! after(?After, ?Before, ?List:list) is nondet.
% Succeeds if After appears after Before in List.
%
% @see The inverse of before/3.

after(After, Before, List):-
  before(Before, After, List).



%! append_intersperse(+List:list, +Separator, -NewList:list)//
% Returns a list that is based on the given list, but interspersed with
% copies of the separator term.
%
% If the length of the given list is `n`, then the length of the new list
% is `2n - 1` for `n > 0`.

append_intersperse([], _S, []):- !.
append_intersperse([H], _S, [H]):- !.
append_intersperse([H|T1], S, [H,S|T2]):-
  append_intersperse(T1, S, T2).



%! before(?Before, ?After, ?List:list) is nondet.
% Succeeds if Before appears before After in List.
%
% @see The transitive closure of directly_before/3.

before(Before, After, List):-
  closure0(directly_before0(List), Before, After).

directly_before0(List, Before, After):-
  directly_before(Before, After, List).



%! combination(+Lists:list(list), -Combination:list) is nondet.
% Returns a combination of items from the given lists.
%
% ## Example
%
% ```
% ?- combination([[1,2,3],[4,5]], C).
% C = [1, 4] ;
% C = [1, 5] ;
% C = [2, 4] ;
% C = [2, 5] ;
% C = [3, 4] ;
% C = [3, 5].
% ```
%
% @arg Lists A list of lists of terms.
% @arg Combination A list of terms.

combination([], []).
combination([ListH|ListT], [H|T]):-
  member(H, ListH),
  combination(ListT, T).



%! common_list_prefix(+List1:list, +List2:list, +Sublist:list) is semidet.
%! common_list_prefix(+List1:list, +List2:list, -Sublist:list) is det.
% Returns the longest common prefix of the given two lists.

common_list_prefix([H1|_], [H2|_], []):-
  H1 \= H2, !.
common_list_prefix([H|T1], [H|T2], [H|T3]):-
  common_list_prefix(T1, T2, T3).



%! complement_list(
%!   +FromList:list,
%!   +Length:nonneg,
%!   +FillElement,
%!   -ToList:list
%! ) is det.

complement_list(L, Length2, _, L):-
  length(L, Length1),
  Length1 >= Length2, !.
complement_list(L1, Length2, Fill, L2):-
  length(L1, Length1),
  FillLength is Length2 - Length1,
  repeating_list(Fill, FillLength, FillList),
  append(L1, FillList, L2).



%! directly_after(?After, ?Before, ?List:list) is nondet.
% Succeeds if After occurs directly after Before in List.
%
% @see Inverse of directly_before/3.

directly_after(After, Before, List):-
  directly_before(Before, After, List).



%! directly_before(?Before, ?After, ?List:list) is nondet.
% Is not semi-deterministic for any instantiation.
% Example for `(+,+,+)`:
%
% ```prolog
% ?- directly_before(1, 2, [1,2,1,2]).
% true ;
% true ;
% false.
%
% ```
%
% @see Terminological variant of nextto/3.

directly_before(Before, After, List):-
  nextto(Before, After, List).



%! element_cut(+L:list, +Element:atom, -L1:list, -L2:list) is det.
% Cuts the given list at the given element, returning the two cut lists.
% The cut element is itself not part of any of the results.
%
% @arg L The list that is to be cut.
% @arg Element The element at which the cut is made.
% @arg L2 The list of elements that occur before the cut.
% @arg L2 The list of elements that occur after the cut.

element_cut([], _Element, [], []):- !.
element_cut([Element | T], Element, [], T):- !.
element_cut([OtherElement | L], Element, [OtherElement | L1], L2):-
  element_cut(L, Element, L1, L2).



%! first(+List:list, ?Element:term) is semidet.
% Succeeds if the given element is the head of the given list.
% Fails if the list has no head.
%
% @arg List Any list.
% @arg Element The head element of the list, if any.
% @see This is the inverse of the default method last/2.

first([H|_], H).



%! first(+L:list, +N:integer, -First:list) is det.
% Returns the first N elements from a list, if these are present.
%
% This never fails but returns a list of length $0 < l(F) < N$ in case
% $l(L) < N$.
%
% @arg L The given list.
% @arg N The length of the returned sublist.
% @arg First The prepended sublist of =L=.

first(L, N, First):-
  length_cut(L, N, First, _L2).



%! first_duplicate(+FirstDuplicate, +List:list) is semidet.
%! first_duplicate(-FirstDuplicate, +List:list) is semidet.
% Succeeds if FirstDuplicate is the first term that appears twice in List
%  when reading from left to right.
%
% ### Use of dif/2
%
% The following naive implementation:
%
% ```prolog
% first_dup(E, [E|L]):-
%   member(E, L).
% first_dup(E, [N|L]):-
%   \+ memberchk(N, L),
%   first_dup(E, L).
% ```
%
% gives the following answers:
%
% ```prolog
% ?- first_dup(E, [A,B,C]).
% E = A, A = B ;
% E = A, A = C ;
% false.
% ```
%
% missing out on one of the anwers we get when using dif/2:
%
% ```swipl
% ?- first_dup(E, [A,B,C]).
% E = A, A = B ;
% E = A, A = C ;
% E = B, B = C,
% dif(A, C),
% dif(A, C) ;
% false.
% ```
%
% @author Ulrich Neumerkel
% @see http://stackoverflow.com/questions/10260672/prolog-first-duplicate-value/10322639#10322639
% @tbd Not sure why SWI-Prolog shows the dif/2 constaint twice...

first_duplicate(X, [_|T]):-
  member(X, T).
first_duplicate(X, [H|T]):-
  maplist(dif(H), T),
  first_duplicate(X, T).



%! inflist(+Element, -List:list) is det.
% Lazy-lists containing an infinitely re-occurring element.
%
% # Example of use
%
% ```prolog
% ?- inflist(0, L), append([A,B,C,D,E,F|_], _, L).
% L = [0, 0, 0, 0, 0, 0|_G29924368],
% A = B, B = C, C = D, D = E, E = F, F = 0,
% freeze(_G29924368, list_ext: (_G29924368=[0|_G29924422], inflist(0, _G29924422)))
% ```
%
% @see Based on
%      [a StackOverflow answer](http://stackoverflow.com/questions/8869485/lazy-lists-in-prolog)
%      by Michael Hendricks.

inflist(X, L):-
  freeze(L, (
    L = [X|T],
    inflist(X, T)
  )).



%! length_cut(+L:list, +Cut:nonneg, -L1:list, -L2:list) is det.
% Cuts the given list in two sublists, where the former sublist
% has the given length.
%
% @arg L The full list.
% @arg Cut A non-negative integer indicating the length of the former sublist.
% @arg L1 The sublist that is the beginning of =L= with length =Cut=.
% @arg L2 The sublist that remains after =L1= has been removed from =L=.

length_cut(L, Cut, L, []):-
  length(L, N),
  N < Cut, !.
length_cut(L, Cut, L1, L2):-
  length(L1, Cut),
  append(L1, L2, L).



%! list_binary_term(+List:list, +Operator, -Term:compound) is det.
%! list_binary_term(-List:list, -Operator, +Term:compound) is det.

list_binary_term([H], _, H).
list_binary_term([H|T1], Op, L2):-
  list_binary_term(T1, Op, T2),
  L2 =.. [Op,H,T2].



%! list_replace(
%!   +List:list,
%!   +Replacements:list(term-term),
%!   -NewList:list
%! ) is det.
% Returns the given list in which the given replacements have been made.
%
% @arg List The original list.
% @arg Replacements A list of replacements of the form `term-term`.
% @arg NewList The list in which all replacants have been replaced.

% Done!
list_replace([], _Maps, []):- !.
% Match one of the replicants.
list_replace(L1, Maps, L2):-
  member(From-To, Maps),
  append(From, Rest1, L1), !,
  list_replace(Rest1, Maps, Rest2),
  (   is_list(To)
  ->  append(To, Rest2, L2)
  ;   L2 = [To|Rest2]
  ).
% A non-matching element.
list_replace([H|T1], Maps, [H|T2]):-
  list_replace(T1, Maps, T2).

list_separator_concat([], _Separator, []):- !.
list_separator_concat([List], _Separator, [List]):- !.
list_separator_concat([List | Lists], Separator, NewList):-
  append(List, Separator, FirstList),
  list_separator_concat(Lists, Separator, RestLists),
  append(FirstList, RestLists, NewList).



%! list_truncate(
%!   +List:list,
%!   +MaximumLength:nonneg,
%!   -TruncatedList:list
%! ) is det.
% Returns the truncated version of the given list.
% The maximum length indicates the exact maximum.
% Truncation will always result in a list which contains
% at most `Max` elements.
%
% @arg List The original list.
% @arg Max The maximum number of elements that is allowed in the list.
% @arg TruncatedList The truncated list.

% The list does not have to be truncated, it is not that long.
list_truncate(L, Max, L):-
  length(L, LL),
  LL =< Max, !.
% The list exceeds the maximum length, it is truncated.
list_truncate(L1, Max, L2):-
  length(L2, Max),
  append(L2, _, L1).



%! member(X, Y, L) is nondet.
% Pairs from a list.
%
% @arg X The first argument of the pair.
% @arg Y The second argument of the pair.
% @arg L The list from which pairs are taken.

member(X, Y, L):-
  member(X, L),
  member(Y, L).



%! member_default(?Element, ?List:list, +Default) is nondet.
% True if `Element` is a member of `List` or is `Default`.
%
% @see member/2 in library(lists).

member_default(Member, List, _):-
  member(Member, List), !.
member_default(Default, _, Default).



%! nth0_minus(?Index:nonneg, ?List:list, ?Element) is nondet.
% Succeeds if the given element occurs at `length(List) - I` in list =L=.
%
% @arg I The index, an integer in `[0, length(List) - 1]`.
% @arg L Any list.
% @arg Element An element occurring in the given list.
% @see The inverse of default method nth0/3.

nth0_minus(I, L1, E):-
  reverse(L1, L2),
  nth0(I, L2, E).


%! nth0_minus(?Index:nonneg, ?List:list, ?Element, Rest:list) is nondet.

nth0_minus(I, L1, E, R1):-
  reverse(L1, L2),
  nth0(I, L2, E, R2),
  reverse(R1, R2).



%! nth1_minus(?Index:nonneg, ?List:list, ?Element) is nondet.
% Succeeds if the given element occurs at `length(L) - I` in list =L=.
%
% @arg I The index, an integer in `[0, length(List)]`.
% @arg L Any list.
% @arg Element An element occurring in the given list.
% @see The inverse of default method nth1/3.

nth1_minus(I, L1, E):-
  reverse(L1, L2),
  nth1(I, L2, E).


%! nth1_minus(?Index:nonneg, ?List:list, ?Element, Rest:list) is nondet.

nth1_minus(I, L1, E, R1):-
  reverse(L1, L2),
  nth1(I, L2, E, R2),
  reverse(R1, R2).



%! nth0chk(?Index:nonneg, ?List:list, ?Element) is nondet.

nth0chk(I, L, E):-
  once(nth0(I, L, E)).


%! nth0chk(?Index:nonneg, ?List:list, ?Element, ?Rest:list) is nondet.

nth0chk(I, L, E, R):-
  once(nth0(I, L, E, R)).



%! nth1chk(?Index:nonneg, ?List:list, ?Element) is nondet.

nth1chk(I, L, E):-
  once(nth1(I, L, E)).


%! nth1chk(?Index:nonneg, ?List:list, ?Element, ?Rest:list) is nondet.

nth1chk(I, L, E, R):-
  once(nth1(I, L, E, R)).



%! postfix(?Part:list, ?Whole:list) is nondet.
% True iff `Part` is a trailing substring of `Whole`.
%
% This is the same as `append(_, Part, Whole)`.
%
% @see prefix/2 in library(lists).

postfix(Part, Whole):-
  append(_, Part, Whole).



%! random_sublist(-Sublist:list, +List:list) is det.
% Returns a sublist of the given list that is (1) of random length
% and that (2) contains randomly selected elements.
%
% @tbd Shorter lists are more probable than longer lists,
%      because there are more sublists of larger length,
%      but each length is as probable to occur.

random_sublist(Sublist, List):-
  length(List, ListLength),
  random_between(0, ListLength, SublistLength),
  random_sublist(Sublist, SublistLength, List).


%! random_sublist(-Sublist:list, +Length:nonneg, +List:list) is det.

random_sublist([], 0, _).
random_sublist([H|Sublist], SublistLength1, List1):-
  random_select(H, List1, List2),
  SublistLength2 is SublistLength1 - 1,
  random_sublist(Sublist, SublistLength2, List2).



%! remove_sublists(+Lists1:list(list), -Lists2:list(list)) is det.
%
% ### Example
%
% ```prolog
% ?- remove_sublists([[a],[a,b],[a,b]], Ls).
% Ls = [[a, b], [a, b]].
% ```

remove_sublists(Ls1, Ls3):-
  select(SubL, Ls1, Ls2),
  member(L, Ls1),
  strict_sublist(SubL, L), !,
  remove_sublists(Ls2, Ls3).
remove_sublists(Ls, Ls).



%! repeating_list(?X, ?N:nonneg, ?L:list) is nondet.
% True when L is a list with N repeats of X

repeating_list(X, N, L) :-
  length(L, N),
  maplist(=(X), L).



%! replace_nth(
%!   +StartIndex:integer,
%!   ?Index:integer,
%!   +OldList:list,
%!   ?OldElement,
%!   +NewElement,
%!   -NewList:list
%! ) is det.
% Performs rather advanced in-list replacements.
%
% ## Examples
%
% Consecutive applications:
% ```
% ?- L1 = [[1,2,3],[4,5,6],[7,8,9]], replace_nth0(1, L1, E1, E2, L2), replace_nth0(2, E1, 6, a, E2).
% L1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]],
% E1 = [4, 5, 6],
% E2 = [4, 5, a],
% L2 = [[1, 2, 3], [4, 5, a], [7, 8, 9]].
% ```
%
% Alternative indexes:
% ```
% ?- list_ext:replace_nth1(I, [a,b,a], a, x, L2).
% I = 1,
% L2 = [x, b, a] ;
% I = 3,
% L2 = [a, b, x] ;
% false.
% ```
%
% @author Jan Wielemaker
% @author Richard O'Keefe
% @author Stefan Ljungstrand
% @author Wouter Beek
% @see The original implementation by Jan and Richard
%      used to start at index 1:
%      http://www.swi-prolog.org/pldoc/doc/home/vnc/prolog/lib/swipl/library/record.pl?show=src
% @see Stefan added the new and old element arguments on 2013/11/02
%      when we were discussing this on the ##prolog channel.

% If the index is given, then the predicate is (semi-)deterministic.
replace_nth(I1, I, L1, E1, E2, L2):-
  nonvar(I), !,
  I >= I1,
  replace_nth_(I1, I, L1, E1, E2, L2), !.
% If the index is not given, then there may be multiple answers.
replace_nth(I1, I, L1, E1, E2, L2):-
  replace_nth_(I1, I, L1, E1, E2, L2).

replace_nth_(I, I, [E1|T], E1, E2, [E2|T]).
replace_nth_(I1, I, [H|T1], E1, E2, [H|T2]):-
  I2 is I1 + 1,
  replace_nth_(I2, I, T1, E1, E2, T2).



%! replace_nth0(
%!   +Index:integer,
%!   +OldList:list,
%!   +OldElement,
%!   +NewElement,
%!   -NewList:list
%! ) is det.
% Performs rather advanced in-list replacements, counting from index 0.
%
% @see Wrapper around replace_nth/6.

replace_nth0(I, L1, E1, E2, L2):-
  replace_nth(0, I, L1, E1, E2, L2).



%! replace_nth1(
%!   +Index:integer,
%!   +OldList:list,
%!   +OldElement,
%!   +NewElement,
%!   -NewList:list
%! ) is det.
% Performs rather advanced in-list replacements, counting from index 1.
%
% @see Wrapper around replace_nth/6.

replace_nth1(I, L1, E1, E2, L2):-
  replace_nth(1, I, L1, E1, E2, L2).



%! selectchk_eq(+Element, +List, -Rest) is det.
% @see Inspired by memberchk_eq in hProlog.

selectchk_eq(X, [Y|Ys1], Zs) :-
  (   X == Y
  ->  Zs = Ys1
  ;   Zs = [Y|Ys2],
      selectchk_eq(X, Ys1, Ys2)
  ).



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



%! split_list_by_number_of_sublists(
%!   +List:list,
%!   +NumberOfSublists:nonneg,
%!   -Sublists:list(list)
%! ) is det.

split_list_by_number_of_sublists(List, NumberOfSublists, Sublists):-
  length(List, Length),
  Length > NumberOfSublists, !,
  succ(ReducedNumberOfSublists, NumberOfSublists),
  SizeOfSublists is Length div ReducedNumberOfSublists,
  split_list_by_size(List, SizeOfSublists, Sublists).
split_list_by_number_of_sublists(List, _NumberOfSublists, List).



%! split_list_by_size(
%!   +List:list,
%!   +MaxmimumLength:integer,
%!   -SubLists:list(list)
%! ) is det.
% Splits the given list into lists of maximally the given length.

% The last sublists is exactly of the requested size.
% The empty list indicates this.
split_list_by_size([], _SizeOfSublists, []):- !.
% The main case: use length/2 and append/3 to extract the list
% prefix that is one of the sublist results.
split_list_by_size(List, SizeOfSublists, [Sublist | Sublists]):-
  length(Sublist, SizeOfSublists),
  append(Sublist, NewList, List), !,
  split_list_by_size(NewList, SizeOfSublists, Sublists).
% The last sublist is not exactly of the requested size. Give back
% what remains.
split_list_by_size(LastSublist, _SizeOfSublists, [LastSublist]).

%! split_list_exclusive(+List:list, +Split:list, -Chunks:list(list)) is det.

split_list_exclusive(List, Split, Chunks):-
  split_list_exclusive(List, Split, [], Chunks).

% The final chunk.
split_list_exclusive([], _Split, Chunk, [Chunk]):- !.
% Process a split.
split_list_exclusive(
  [Match | List1],
  [Match | Split],
  Chunk,
  [Chunk | Chunks]
):-
  append(Split, List2, List1), !,
  split_list_exclusive(List2, [Match | Split], [], Chunks).
% Process a chunk.
split_list_exclusive([Part | List], Split, Chunk, Chunks):-
  split_list_exclusive(List, Split, [Part | Chunk], Chunks).



%! strict_sublist(?SubList:list, +List:list) is nondet.

strict_sublist(SubList, List):-
  sublist(SubList, List),
  SubList \== List.



%! sublist(?SubList:list, +List:list) is nondet.
% Returns sublists of the given list.
% Construction proceeds from smaller to greater sublists.

sublist([], []).
sublist(SubT, [_|T]):-
  sublist(SubT, T).
sublist([H|SubT], [H|T]):-
  sublist(SubT, T).

