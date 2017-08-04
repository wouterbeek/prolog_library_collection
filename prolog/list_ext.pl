:- module(
  list_ext,
  [
    common_subsequence/2, % +Subsequences, -Subsequence
    list_intersperse/3,   % +List1, +Sep, -List2
    list_truncate/3,      % +List1, +MaxLength, -List2
    repeating_list/3,     % ?Elem, ?NunElems, ?List
    substring/2,          % ?Substring, +String
    subsequence/2         % ?Subsequence, ?Sequence
  ]
).
:- reexport(library(lists)).

/** <module> List extensions

@author Wouter Beek
@version 2017/05-2017/06
*/

:- use_module(library(apply)).





%! common_subsequence(+Subsequences, -Subsequence) is nondet.

common_subsequence(Xss, Xs) :-
  maplist(subsequence(Xs), Xss).



%! list_intersperse(+List1, +Sep, -List2)// is det.
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



%! substring(?Substring, +String) is nondet.
%
% Returns substrings of the given string.  Construction proceeds from
% smaller to greater substrings.
%
% `[1,3]' is not a substring of `[1,2,3]', but it is a subsequence.

substring([], _).
substring(Ys, Xs) :-
  Ys = [_|_],
  append(_, Zs, Xs),
  append(Ys, _, Zs).



%! subsequence(?Subsequence, ?Sequence) is nondet.

subsequence([], []).
subsequence(L, [_|T]) :-
  subsequence(L, T).
subsequence([H|T2], [H|T1]) :-
  subsequence(T2, T1).
