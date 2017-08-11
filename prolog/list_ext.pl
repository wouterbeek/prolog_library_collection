:- module(
  list_ext,
  [
    common_subsequence/2, % +Subsequences, -Subsequence
    list_intersperse/3,   % +L1, +Separator, -L2
    list_truncate/3,      % +Whole, +MaxLength, -Part
    postfix/2,            % ?Part, ?Whole
    postfix/3,            % ?Part, ?Length, ?Whole
   %prefix/2,             % ?Part, ?Whole
    prefix/3,             % ?Part, ?Length, ?Whole
    repeating_list/3,     % ?Elem, ?N, ?L
    substring/2,          % ?Substring, +String
    subsequence/2         % ?Subsequence, ?Sequence
  ]
).
:- reexport(library(lists)).

/** <module> List extensions

@author Wouter Beek
@version 2017/05-2017/08
*/

:- use_module(library(apply)).





%! common_subsequence(+Subsequences:list(list), -Subsequence:list) is nondet.

common_subsequence(Xss, Xs) :-
  maplist(subsequence(Xs), Xss).



%! list_intersperse(+L1:list, +Separator:term, -L2:list)// is det.
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



%! list_truncate(+Whole:list, +MaxLength:or([oneof([∞]),nonneg]),
%!               -Part:list) is det.
%
% Returns the truncated version of the given list.  The maximum length
% indicates the exact maximum.  Truncation will always result in a
% list which contains at most `MaxLength' elements.

list_truncate(Whole, ∞, Whole):- !.
% The list does not have to be truncated, it is not that long.
list_truncate(Whole, MaxLength, Whole) :-
  length(Whole, Length),
  Length =< MaxLength, !.
% The list exceeds the maximum length, it is truncated.
list_truncate(Whole, MaxLength, Part) :-
  prefix(Part, MaxLength, Whole).



%! postfix(?Part:list, ?Whole:list) is nondet.
%! postfix(?Part:list, ?N:nonneg, ?Whole:list) is nondet.
%
% Part is the length-N postfix of Whole.

postfix(Part, Whole) :-
  postfix(Part, _, Whole).


postfix(Part, Length, Whole) :-
  length(Part, Length),
  append(_, Part, Whole).



%! prefix(?Part:list, ?Length:nonneg, ?Whole:list) is det.
%
% Shorter prefixes are generated first.

prefix(Part, Length, Whole) :-
  length(Part, Length),
  append(Part, _, Whole).



%! repeating_list(+Elem, +N:nonneg, +L:list) is semidet.
%! repeating_list(+Elem, +N:nonneg, -L:list) is det.
%! repeating_list(+Elem, -N:nonneg, +L:list) is semidet.
%! repeating_list(-Elem, +N:nonneg, +L:list) is semidet.
%! repeating_list(+Elem, -N:nonneg, -L:list) is multi.
%! repeating_list(-Elem, +N:nonneg, -L:list) is det.
%! repeating_list(-Elem, -N:nonneg, +L:list) is det.
%! repeating_list(-Elem, -N:nonneg, -L:list) is multi.
%
% Succeeds for lists L that repeat term Elem exactly N times.

repeating_list(Elem, N, L) :-
  length(L, N),
  maplist(=(Elem), L).



%! substring(?Substring:list, +String:list) is nondet.
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



%! subsequence(?Subsequence:list, ?Sequence:list) is nondet.
%
% Returns substrings of the given string.  Construction proceeds from
% smaller to greater subsequences.

subsequence([], []).
subsequence(L, [_|T]) :-
  subsequence(L, T).
subsequence([H|T2], [H|T1]) :-
  subsequence(T2, T1).
