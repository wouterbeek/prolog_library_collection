:- module(
  list_ext,
  [
    after/3,                % ?After, ?Before, ?L
    before/3,               % ?Before, ?After, ?L
    combination/2,          % +Ls, -L
    common_list_prefix/3,   % +L1, +L2, ?Sublist
    complement_list/4,      % +L1, +Len, +Fill, -L2
    directly_after/3,       % ?After, ?Before, ?L
    directly_before/3,      % ?After, ?Before, ?L
    element_cut/4,          % +L, +X, -L1, -L2
    empty_list/1,           % ?L
    ensure_list/2,          % +Term, -L
    first/2,                % +L, ?First
    first_duplicate/2,      % ?FirstDuplicate, +L
    is_singleton_list/1,    % @Term
    list_binary_term/3,     % ?L, ?Op, ?Term
    list_intersperse/3,     % +L1, +Sep, -L2
    list_replace/3,         % +L1, +Replacements:list(pair), -L2
    list_row/2,             % ?L, ?Row
    list_split/2,           % ?L, ?X
    list_split/3,           % ?L, ?X, ?Y
    list_split/4,           % ?L, ?X, ?Y, ?Z
    list_truncate/3,        % +L1, +Max, -L2
    member/3,               % ?X, ?Y, ?L
    member_default/3,       % ?X, +L, +Def
    nth0_minus/3,           % ?I, ?L, ?X
    nth0_minus/4,           % ?I, ?L, ?X
    nth1_minus/3,           % ?I, ?L, ?X
    nth1_minus/4,           % ?I, ?L, ?X
    nth0chk/3,              % ?I, ?L, ?X
    nth0chk/4,              % ?I, ?L, ?X
    nth1chk/3,              % ?I, ?L, ?X
    nth1chk/4,              % ?I, ?L, ?X
    postfix/2,              % ?Part, ?Whole
    postfix/3,              % ?Part, ?N, ?Whole
    prefix/3,               % ?Part, ?N, ?Whole
    remove_sublists/2,      % +Ls1, -Ls2
    repeating_list/3,       % ?X, ?N, ?L
    replace_nth/6,          % +Start, ?I, +L1, ?X1, +X2, -L2
    replace_nth0/5,         % ?I, +L1, ?X1, +X2, -L2
    replace_nth1/5,         % ?I, +L1, ?X1, +X2, -L2
    selectchk_eq/3,         % +X, +Whole, -Part
    shorter/3,              % +Comparator_2, +L1, +L2
    singleton_list/2,       % ?X, ?L
    sort_by_length/2,       % +ListOfLists, -ByLen
    split_list_exclusive/3, % +L, +Split, -Ls
    split_list_member/4,    % +L, -Before, ?X, -After
    split_list_nth0/5,      % +L, -Before, ?I, ?X, -After
    split_list_nth1/5,      % +L, -Before, ?I, ?X, -After
    split_list_number/3,    % +L, +NumLs, -Ls
    split_list_size/3,      % +L, +SizeOfSublists, -Ls
    strict_sublist/2,       % ?Part, +Whole
    sublist/2               % ?Part, +Whole
  ]
).
:- reexport(library(lists)).

/** <module> List extensions

Extensions to the set of list predicates in SWI-Prolog.

@author Wouter Beek
@version 2015/07, 2015/10-2015/11, 2016/05, 2016/09-2016/10
*/

:- use_module(library(apply)).
:- use_module(library(closure)).
:- use_module(library(error)).
:- use_module(library(pair_ext)).
:- use_module(library(typecheck)).





%! after(?After, ?Before, ?List) is nondet.
% Succeeds if After appears after Before in List.
%
% @see The inverse of before/3.

after(After, Before, L) :-
  before(Before, After, L).



%! before(?Before, ?After, ?L) is nondet.
% Succeeds if Before appears before After in L.
%
% @see The transitive closure of directly_before/3.

before(Before, After, L) :-
  closure0(directly_before0(L), Before, After).

directly_before0(L, Before, After) :-
  directly_before(Before, After, L).



%! combination(+Ls, -L) is nondet.
%
% List L is a combination of items from the lists Ls.
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

combination([], []).
combination([ListH|ListT], [H|T]) :-
  member(H, ListH),
  combination(ListT, T).



%! common_list_prefix(+L1, +L2, +Sublist) is semidet.
%! common_list_prefix(+L1, +L2, -Sublist) is det.
% 
% Sublist is the longest common prefix of lists L1 and L2.

common_list_prefix([H1|_], [H2|_], []) :-
  H1 \= H2, !.
common_list_prefix([H|T1], [H|T2], [H|T3]) :-
  common_list_prefix(T1, T2, T3).



%! complement_list(+L1, +Len, +Fill, -L2) is det.
%
% L2 is a list of length Len whose prefix is L1 and whose suffix
% consists of as many instances of Fill as are needed to fill the
% list.

complement_list(L, Len2, _, L) :-
  length(L, Len1),
  Len1 >= Len2, !.
complement_list(L1, Len2, Fill, L2) :-
  length(L1, Len1),
  Len is Len2 - Len1,
  repeating_list(Fill, Len, L3),
  append(L1, L3, L2).



%! directly_after(?After, ?Before, ?L) is nondet.
%
% Succeeds if After occurs directly after Before in List.
%
% @see Inverse of directly_before/3.

directly_after(After, Before, L) :-
  directly_before(Before, After, L).



%! directly_before(?Before, ?After, ?List) is nondet.
%
% Is not semi-deterministic for any instantiation.
%
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

directly_before(Before, After, List) :-
  nextto(Before, After, List).



%! element_cut(+L, +X, -L1, -L2) is det.
%
% Cuts list L at the given Element, returning the two cut lists L1 and
% L2.  The cut element is itself not part of any of the results.

element_cut([], _Element, [], []) :- !.
element_cut([Element | T], Element, [], T) :- !.
element_cut([OtherElement | L], Element, [OtherElement | L1], L2) :-
  element_cut(L, Element, L1, L2).



%! empty_list(@Term) is semidet.

empty_list([]).



%! ensure_list(+Term, -L) is det.

ensure_list(L, L) :-
  is_list(L), !.
ensure_list(Term, [Term]).



%! first(+L, ?X) is semidet.
%
% Succeeds if the given element is the head of the given list.  Fails
% if the list has no head.

first([H|_], H).



%! first_duplicate(+FirstDuplicate, +L) is semidet.
%! first_duplicate(-FirstDuplicate, +L) is semidet.
%
% Succeeds if FirstDuplicate is the first term that appears twice in
% List when reading from left to right.
%
% ### Use of dif/2
%
% The following naive implementation:
%
% ```prolog
% first_dup(E, [E|L]) :-
%   member(E, L).
% first_dup(E, [N|L]) :-
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

first_duplicate(X, [_|T]) :-
  member(X, T).
first_duplicate(X, [H|T]) :-
  maplist(dif(H), T),
  first_duplicate(X, T).



%! is_singleton_list(@Term) is semidet.

is_singleton_list(T) :-
  is_list(T),
  T = [_].



%! list_binary_term(+L, +Op, -Term) is det.
%! list_binary_term(-L, -Op, +Term) is det.

list_binary_term([H], _, H).
list_binary_term([H|T1], Op, L2) :-
  list_binary_term(T1, Op, T2),
  L2 =.. [Op,H,T2].



%! list_intersperse(+L1, +Sep, -L2)// is det.
%
% Returns a list that is based on the given list, but interspersed
% with copies of the separator term.
%
% If the length of the given list is `n`, then the length of the new list
% is `2n - 1` for `n > 0`.

list_intersperse([], _, []) :- !.
list_intersperse([H], _, [H]) :- !.
list_intersperse([H|T1], Sep, [H,Sep|T2]) :-
  list_intersperse(T1, Sep, T2).



%! list_replace(+L1, +Replacements:list(pair), -L2) is det.
%
% Returns the given list in which the given replacements have been made.

list_replace([], _, []) :- !.
% Match one of the replicants.
list_replace(L1, Maps, L2) :-
  member(From-To, Maps),
  append(From, Rest1, L1), !,
  list_replace(Rest1, Maps, Rest2),
  (   is_list(To)
  ->  append(To, Rest2, L2)
  ;   L2 = [To|Rest2]
  ).
% A non-matching element.
list_replace([H|T1], Maps, [H|T2]) :-
  list_replace(T1, Maps, T2).



%! list_row(+L, +Row) is semidet.
%! list_row(+L, -Row) is det.
%! list_row(-L, +Row) is det.

list_row(L, Row) :-
  Row =.. [row|L].



%! list_split(+L, +X) is semidet.
%! list_split(+L, -X) is det.
%! list_split(-L, +X) is det.
%! list_split(+L, +X, +Y) is semidet.
%! list_split(+L, -X, -Y) is det.
%! list_split(-L, +X, +Y) is det.
%! list_split(+L, +X, +Y, +Z) is semidet.
%! list_split(+L, -X, -Y, -Z) is det.
%! list_split(-L, +X, +Y, +Z) is det.

list_split([X], X).


list_split([X,Y], X, Y).


list_split([X,Y,Z], X, Y, Z).



%! list_truncate(+Whole, +Max:or([oneof([inf]),nonneg]), -Part) is det.
%
% Returns the truncated version of the given list.  The maximum length
% indicates the exact maximum.  Truncation will always result in a
% list which contains at most `Max` elements.

% Special value `inf`.
list_truncate(L, inf, L):- !.
% The list does not have to be truncated, it is not that long.
list_truncate(L, Max, L) :-
  length(L, Len),
  Len =< Max, !.
% The list exceeds the maximum length, it is truncated.
list_truncate(L1, Max, L2) :-
  length(L2, Max),
  append(L2, _, L1).



%! member(X, Y, L) is nondet.
%
% Pairs from a list.

member(X, Y, L) :-
  member(X, L),
  member(Y, L).



%! member_default(?X, ?L, +Def) is nondet.
%
% True if `Element` is a member of `List` or is `Default`.

member_default(X, L, _) :-
  member(X, L), !.
member_default(Def, _, Def).



%! nth0_minus(?I, ?L, ?X) is nondet.
%! nth0_minus(?I, ?L, ?X, ?Rest) is nondet.
%
% Succeeds if X occurs at `length(List) - I` in L.

nth0_minus(I, L1, X) :-
  reverse(L1, L2),
  nth0(I, L2, X).


nth0_minus(I, L1, X, Rest1) :-
  reverse(L1, L2),
  nth0(I, L2, X, Rest2),
  reverse(Rest1, Rest2).



%! nth1_minus(?I, ?L, ?X) is nondet.
%! nth1_minus(?I, ?L, ?X, Rest) is nondet.
% 
% Succeeds if element X occurs at `length(L) - I` in L.
%
% @see The inverse of default method nth1/3.

nth1_minus(I, L1, E) :-
  reverse(L1, L2),
  nth1(I, L2, E).


nth1_minus(I, L1, E, Rest1) :-
  reverse(L1, L2),
  nth1(I, L2, E, Rest2),
  reverse(Rest1, Rest2).



%! nth0chk(?I, ?L, ?X) is nondet.
%! nth0chk(?I, ?L, ?X, ?Rest) is nondet.

nth0chk(I, L, E) :-
  once(nth0(I, L, E)).


nth0chk(I, L, E, Rest) :-
  once(nth0(I, L, E, Rest)).



%! nth1chk(?I, ?L, ?X) is nondet.
%! nth1chk(?I, ?L, ?X, ?Rest) is nondet.

nth1chk(I, L, E) :-
  once(nth1(I, L, E)).


nth1chk(I, L, E, Rest) :-
  once(nth1(I, L, E, Rest)).



%! postfix(?Part, ?Whole) is nondet.
%! postfix(?Part, ?N, ?Whole) is nondet.
%
% Part is the length-N postfix of Whole.

postfix(Part, Whole) :-
  postfix(Part, _, Whole).


postfix(Part, N, Whole) :-
  length(Part, N),
  append(_, Part, Whole).



%! prefix(?Part, ?N, ?Whole) is det.
%
% Part is the length-N prefix of Whole.

prefix(Part, N, Whole) :-
  length(Part, N),
  append(Part, _, Whole).



%! remove_sublists(+Ls1, -Ls2) is det.
%
% ### Example
%
% ```prolog
% ?- remove_sublists([[a],[a,b],[a,b]], Ls).
% Ls = [[a, b], [a, b]].
% ```

remove_sublists(Ls1, Ls3) :-
  select(SubL, Ls1, Ls2),
  member(L, Ls1),
  strict_sublist(SubL, L), !,
  remove_sublists(Ls2, Ls3).
remove_sublists(Ls, Ls).



%! repeating_list(+X, +N, +L) is semidet.
%! repeating_list(+X, +N, -L) is det.
%! repeating_list(+X, -N, +L) is semidet.
%! repeating_list(-X, +N, +L) is semidet.
%! repeating_list(+X, -N, -L) is multi.
%! repeating_list(-X, +N, -L) is det.
%! repeating_list(-X, -N, +L) is det.
%! repeating_list(-X, -N, -L) is multi.
%
% Succeeds for lists L that repeat term X exactly N times.

repeating_list(X, N, L) :-
  length(L, N),
  maplist(=(X), L).



%! replace_nth(+Start, ?I, +L1, ?X1, +X2, -L2) is det.
%
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
%
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
replace_nth(I1, I, L1, E1, E2, L2) :-
  nonvar(I), !,
  I >= I1,
  replace_nth_(I1, I, L1, E1, E2, L2), !.
% If the index is not given, then there may be multiple answers.
replace_nth(I1, I, L1, E1, E2, L2) :-
  replace_nth_(I1, I, L1, E1, E2, L2).

replace_nth_(I, I, [E1|T], E1, E2, [E2|T]).
replace_nth_(I1, I, [H|T1], E1, E2, [H|T2]) :-
  I2 is I1 + 1,
  replace_nth_(I2, I, T1, E1, E2, T2).



%! replace_nth0(+I, +L1, +X1, +X2, -L2) is det.
%
% Performs rather advanced in-list replacements, counting from index
% 0.
%
% @see Wrapper around replace_nth/6.

replace_nth0(I, L1, E1, E2, L2) :-
  replace_nth(0, I, L1, E1, E2, L2).



%! replace_nth1(+I, +L1, +X1, +X2, -L2) is det.
%
% Performs rather advanced in-list replacements, counting from index
% 1.
%
% @see Wrapper around replace_nth/6.

replace_nth1(I, L1, E1, E2, L2) :-
  replace_nth(1, I, L1, E1, E2, L2).



%! selectchk_eq(+X, +Whole, -Part) is det.
% @see Inspired by memberchk_eq in hProlog.

selectchk_eq(X, [Y|Ys1], Zs) :-
  (   X == Y
  ->  Zs = Ys1
  ;   Zs = [Y|Ys2],
      selectchk_eq(X, Ys1, Ys2)
  ).



%! shorter(+Order, +L1, +L2) is semidet.
%
% Succeeds if L1 has relation Order to L2.
%
% Order is either </2, =/2, or >/2.

shorter(Order, L1, L2) :-
  length(L1, Len1),
  length(L2, Len2),
  compare(Order, Len2, Len1).



%! singleton_list(+X, +L) is semidet.
%! singleton_list(+X, -L) is det.
%! singleton_list(-X, +L) is det.

singleton_list(X, [X]).



%! sort_by_length(+ListOfLists, -ByLen) is det.

sort_by_length(ListOfLists, ByLen) :-
  map_list_to_pairs(length, ListOfLists, Pairs),
  asc_pairs_values(Pairs, ByLen).



%! split_list_exclusive(+List, +Split, -Chunks) is det.

split_list_exclusive(List, Split, Chunks) :-
  split_list_exclusive(List, Split, [], Chunks).

% The final chunk.
split_list_exclusive([], _Split, Chunk, [Chunk]) :- !.
% Process a split.
split_list_exclusive(
  [Match | List1],
  [Match | Split],
  Chunk,
  [Chunk | Chunks]
) :-
  append(Split, List2, List1), !,
  split_list_exclusive(List2, [Match | Split], [], Chunks).
% Process a chunk.
split_list_exclusive([Part | List], Split, Chunk, Chunks) :-
  split_list_exclusive(List, Split, [Part | Chunk], Chunks).



%! split_list_member(+L, -Before, ?X, -After) is nondet.
%
% Split L into members that appear Before and After one particular
% member X.

split_list_member([H|T], [], H, T).
split_list_member([H|T], [H|T1], X, L2) :-
  split_list_member(T, T1, X, L2).



%! split_list_nth0(+L, -Before, ?I, ?X, -After) is nondet.
%! split_list_nth1(+L, -Before, ?I, ?X, -After) is nondet.
%
% Split L into members that appear Before and After one particular
% member X that appears at index I.  The index is either count-by-zero
% or count-by-one.

split_list_nth0(L, L1, N, X, L2) :-
  split_list_nth(L, L1, 0, N, X, L2).


split_list_nth1(L, L1, N, X, L2) :-
  split_list_nth(L, L1, 1, N, X, L2).


split_list_nth([H|T], [], N, N, H, T).
split_list_nth([H|T], [H|T1], M1, N, X, L2) :-
  M2 is M1 + 1,
  split_list_nth(T, T1, M2, N, X, L2).



%! split_list_number(+L, +NumLs, -Ls) is det.

split_list_number(L, NumLs, Ls) :-
  length(L, Len),
  SizeL is ceil(Len / NumLs),
  split_list_size(L, SizeL, Ls).



%! split_list_size(+L, +MaxLen, -Ls) is det.
% Splits the given list into lists of maximally the given length.

% The last sublists is exactly of the requested size.
% The empty list indicates this.
split_list_size([], _SizeOfSublists, []) :- !.
% The main case: use length/2 and append/3 to extract the list
% prefix that is one of the sublist results.
split_list_size(List, SizeOfSublists, [Sublist | Sublists]) :-
  length(Sublist, SizeOfSublists),
  append(Sublist, NewList, List), !,
  split_list_size(NewList, SizeOfSublists, Sublists).
% The last sublist is not exactly of the requested size. Give back
% what remains.
split_list_size(LastSublist, _SizeOfSublists, [LastSublist]).



%! strict_sublist(?Part, +Whole) is nondet.

strict_sublist(Part, Whole) :-
  sublist(Part, Whole),
  Part \== Whole.



%! sublist(?Part, +Whole) is nondet.
%
% Returns sublists of the given list.  Construction proceeds from
% smaller to greater sublists.

sublist([], []).
sublist(Part, [_|T]) :-
  sublist(Part, T).
sublist([H|SubT], [H|T]) :-
  sublist(SubT, T).
