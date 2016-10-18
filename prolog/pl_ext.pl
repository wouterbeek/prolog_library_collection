:- module(
  pl_ext,
  [
    check_pl_version/1, % +MinVersion
    git_version/1,      % -Version
    n_ary_term/3,       % +Pred, +Args, -Comp
    pl_version/1,       % -Version
    write_fact/1,       % @Term
    write_term/1        % @Term
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



%! write_fact(+Fact:compound) is det.

write_fact(Term) :-
  write_term(Term),
  write(.),
  nl.



%! write_term(@Term) is det.
% Alternative to write_canonical/[1,2] that lives up to the promise that
% "terms written with this predicate can always be read back".

write_term(Term) :-
  replace_blobs(Term, AtomBlobs),
  write_term(AtomBlobs, [numbervars(true),quoted(true)]).


%! replace_blobs(Term0, Term) is det.
% Copy Term0 to Term, replacing non-text blobs.
% This is required for error messages that may hold streams
% and other handles to non-readable objects.

replace_blobs([], []) :- !.
replace_blobs(Blob, Atom) :-
  blob(Blob, Type),
  Type \== text, !,
  format(atom(Atom), '~p', [Blob]).
replace_blobs(Term0, Term) :-
  compound(Term0), !,
  compound_name_arguments(Term0, Pred, Args0),
  maplist(replace_blobs, Args0, Args),
  compound_name_arguments(Term, Pred, Args).
replace_blobs(Term, Term).





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
