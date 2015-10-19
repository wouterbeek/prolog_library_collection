:- module(
  pair_ext,
  [
    inverse_pair/2, % ?Pair:pair
                    % ?Inverse:pair
    list_pair/2, % ?List:list
                 % ?Pair:pair
    merge_pairs_by_key/2, % +Pairs:list(pair)
                          % -Merged:list(pair)
    pair_element/2, % ?Pair:pair
                    % ?Element
    pair_first/2, % +Pair:pair
                  % ?First
    pair_list/2, % ?Pair:pair
                 % ?List:list
    pair_second/2, % +Pair:pair
                   % ?Second
    pairs_to_ascending_values/2, % +Pairs:list(pair)
                                 % -Values:list
    pairs_to_descending_values/2, % +Pairs:list(pair)
                                  % -Values:list
    read_pairs_from_file/2, % +File:atom
                            % -Pairs:ordset(pair(atom))
    remove_pairs/3, % +Original:ordset(pair)
                    % +Remove:ordset
                    % -Result:ordset(pair)
    store_pairs_to_file/2, % +Pairs:list(pair(atom))
                           % +File:atom
    subpairs/3, % +Pairs:ordset(pair)
                % +Subkeys:ordset
                % -Subpairs:ordset(pair)
    term_to_pair/2 % @Term
                   % -Pair:pair
  ]
).

/** <module> Pair extensions

Support predicates for working with pairs.

@author Wouter Beek
@version 2013/09-2013/10, 2013/12, 2014/03, 2014/05, 2014/07-2014/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).

% Used for loading pairs from file.
:- dynamic(pair/2).





%! inverse_pair(+Pair:pair, -Inverse:pair) is det.
%! inverse_pair(-Pair:pair, +Inverse:pair) is det.

inverse_pair(X-Y, Y-X).



%! list_pair(+List:list, +Pair:pair) is semidet.
%! list_pair(+List:list, -Pair:pair) is det.
%! list_pair(-List:list, +Pair:pair) is det.

list_pair([X,Y], X-Y).



%! merge_pairs_by_key(+Pairs:list(pair), -Merged:list(pair)) is det.

merge_pairs_by_key([], []).
merge_pairs_by_key([K-V|T1], [K-VMerge|T3]):-
  same_key(K, [K-V|T1], VMerge, T2),
  merge_pairs_by_key(T2, T3).

same_key(K, [K-V|L1], VMerge2, L2):- !,
  same_key(K, L1, VMerge1, L2),
  ord_union(VMerge1, V, VMerge2).
same_key(K, [_|L1], VMerge, L2):- !,
  same_key(K, L1, VMerge, L2).
same_key(_, L, [], L).



%! pair_element(+Pair:pair, +Element) is semidet.
%! pair_element(+Pair:pair, -Element) is multi.

pair_element(X-_, X).
pair_element(_-Y, Y).



%! pair_first(+Pair:pair, +First) is semidet.
%! pair_first(+Pair:pair, -First) is det.

pair_first(X-_, X).



%! pair_list(+Pair:pair, +List:list) is semidet.
%! pair_list(+Pair:pair, -List:list) is det.
%! pair_list(-Pair:pair, +List:list) is det.

pair_list(X-Y, [X,Y]).



%! pair_second(+Pair:pair, +Second) is semidet.
%! pair_second(+Pair:pair, -Second) is det.

pair_second(X-_, X).



%! pairs_to_ascending_values(+Pairs:list(pair), -Values:list) is det.

pairs_to_ascending_values(Pairs1, Values):-
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, Values).



%! pairs_to_descending_values(+Pairs:list(pair), -Values:list) is det.

pairs_to_descending_values(Pairs1, Values):-
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),
  pairs_values(Pairs3, Values).



%! read_pairs_from_file(+File:atom, -Pairs:ordset(pair(atom))) is det.

read_pairs_from_file(File, Pairs):-
  setup_call_cleanup(
    ensure_loaded(File),
    aggregate_all(
      set(From-To),
      pair(From, To),
      Pairs
    ),
    unload_file(File)
  ).



%! remove_pairs(
%!   +Original:ordset(pair),
%!   +RemoveKeys:ordset,
%!   -Result:ordset(pair)
%! ) is det.
% Assumes that the Original and Remove pairs are ordered in the same way.

remove_pairs(L1, RemoveKeys, L2):-
  pairs_keys(L1, Keys0),
  list_to_ord_set(Keys0, Keys),
  ord_subtract(Keys, RemoveKeys, RetainKeys),
  subpairs(L1, RetainKeys, L2).



%! store_pairs_to_file(+Pairs:list(pair(atom)), +File:atom) is det.

store_pairs_to_file(Pairs, File):-
  setup_call_cleanup(
    open(File, write, Stream),
    forall(
      member(From-To, Pairs),
      (
        writeq(Stream, pair(From,To)),
        write(Stream, .),
        nl(Stream)
      )
    ),
    close(Stream)
  ).



%! subpairs(
%!   +Pairs:ordset(pair),
%!   +Subkeys:ordset,
%!   -Subpairs:ordset(pair)
%! ) is det.
% This assumes that pairs and keys are ordered in the same way.

subpairs([], [], []).
subpairs([K-V|T1], [K|T2], [K-V|T3]):- !,
  subpairs(T1, T2, T3).
subpairs([_|T1], L2, L3):-
  subpairs(T1, L2, L3).



%! term_to_pair(@Term, -Pair:pair) is det.
% Returns the pair notation `First-Second` if the given term
% can be interpreted as a pair.
%
% The following pair notations are recognized:
%   1. `X-Y`
%   2. `X=Y`
%   3. `[X,Y]`
%   4. `X(Y)`

term_to_pair(X-Y, X-Y):- !.
term_to_pair(X=Y, X-Y):- !.
term_to_pair([X,Y], X-Y):- !.
term_to_pair(Compound, X-Y):-
  Compound =.. [X,Y].
