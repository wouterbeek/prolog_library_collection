:- module(
  pl_ext,
  [
    check_pl_version/1,     % +MinVersion
    git_version/1,          % -Version
    n_ary_term/3,           % +Pred, +Args, -Comp
    pl_version/1,           % -Version
    term_frequency_pairs/2, % +Terms, -TermFreqPairs
  ]
).

/** <module> Prolog extensions

@author Wouter Beek
@version 2015/07, 2015/10, 2016/08-2016/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).





%! check_pl_version(+MinVersion) is nondet.
%
% MinVersion is either a version compound term of the form [1], or a
% -- possibly nested -- compound term of the form [2].
%
% [1]   version  = version(major:nonneg,minor:nonneg,path:nonneg)
% [2]   versions = or(version,versions) | version

check_pl_version(MinVersion) :-
  pl_version(Version),
  check_pl_version(MinVersion, Version).


user_press_key_to_exit(Out) :-
  format(Out, "~nPress any key to exit> ", []),
  get_single_char(_),
  nl(Out),
  halt(1).


check_pl_version(or(MinVersion1,MinVersion2), Version) :-
  (   check_pl_version(MinVersion1, Version)
  ;   check_pl_version(MinVersion2, Version)
  ), !.
check_pl_version(
  version(MinMajor,MinMinor,MinPatch),
  version(Major,Minor,Patch)
) :-
  (   Major > MinMajor
  ;   Major =:= MinMajor,
      (   Minor > MinMinor
      ;   Minor =:= MinMinor,
          Patch >= MinPatch
      )
  ), !.
check_pl_version(MinVersion, Version) :-
  print_message(error, min_pl_version(MinVersion,Version)),
  user_press_key_to_exit(user_error).



%! git_version(-Version) is det.

git_version(Version) :-
  current_prolog_flag(version_git, Version).



%! n_ary_term(+Pred, +Args, -Comp) is det.
%
% Builds an $n$-ary term out of a binary predicate Pred.

n_ary_term(Pred, [H], Comp) :- !,
  Comp =.. [Pred,H].
n_ary_term(Pred, [H1,H2], Comp) :- !,
  Comp =.. [Pred,H1,H2].
n_ary_term(Pred, [H|T], Comp) :- !,
  n_ary_term(Pred, T, Comp0),
  Comp =.. [Pred,H,Comp0].



%! pl_version(-Version) is det.
%
% Version is of the form
% version(major:nonneg,minor:nonneg,patch:nonneg).

pl_version(version(Major,Minor,Patch)) :-
  current_prolog_flag(version_data, swi(Major, Minor, Patch, _)).






%! term_frequency_pairs(+Terms, -TermFreqPairs) is det.
%
% TermFrequencyPairs is a list of pairs Term-Count of unique terms
% Term, together with the number of times their occur in Terms.  Term
% equivalence is ==/2.  The pairs are sorted according to the standard
% order of terms.

term_frequency_pairs(Terms, Pairs) :-
  msort(Terms, Sorted),
  fpairs(Sorted, Pairs).

fpairs([], []).
fpairs([H|T0], [H-C|T]) :-
  % Identical terms appear in sequence, due to msort/2.
  pick_same(T0, T1, H, 1, C),
  fpairs(T1, T).

pick_same([H1|T0], L, H, F0, F) :-
  H == H1, !,
  F1 is F0 + 1,
  pick_same(T0, L, H, F1, F).
pick_same(L, L, _, F, F).





% MESSAGES %

:- multifile
    prolog:message/3.

prolog:message(min_pl_version(MinVersions,Version)) -->
  "This program requires SWI-Prolog ",
  versions(MinVersions), ".", nl,
  "while you are running version ",
  version(Version), ".", nl,
  "Please visit http://www.swi-prolog.org to upgrade.".


version(version(Major,Minor,Patch)) -->
  number(Major), ".",
  number(Minor), ".",
  number(Patch).


versions(or(Version,Versions)) --> !,
  version(Version),
  " or ",
  versions(Versions).
versions(Version) -->
  version(Version).
