:- module(
  sort_ext,
  [
    predmsort/3,   % :Compare_2, +Original, -Sorted
    sort_stream/2, % +In, -Out
    sort_stream/3  % +In, -Out, +Options
  ]
).

/** <module> Supoort for sorting

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(process)).

:- use_module(library(dict)).
:- use_module(library(thread_ext)).

:- meta_predicate
    predmerge(3, +, +, -),
    predmerge(+, 3, +, +, -, -, -),
    predmsort(3, +, -),
    predmsort(3, ?, +, ?, -).





%! predmsort(:Compare_2, +Original:list, -Sorted:list) is det.
%
% @see Like predsort/3, but using msort/2.

predmsort(P, L, R) :-
  '$skip_list'(N, L, Tail),
  (Tail == [] -> predmsort(P, N, L, _, R1), R = R1 ; must_be(L, list)).

predmsort(P, 2, [X1,X2|L], L, R) :- !,
  call(P, Delta, X1, X2),
  msort_(Delta, X1, X2, R).
predmsort(_, 1, [X|L], L, [X]) :- !.
predmsort(_, 0, L, L, []) :- !.
predmsort(P, N, L1, L3, R) :-
  N1 is N // 2,
  plus(N1, N2, N),
  predmsort(P, N1, L1, L2, R1),
  predmsort(P, N2, L2, L3, R2),
  predmerge(P, R1, R2, R).

msort_(<, X1, X2, [X1,X2]).
msort_(=, X1, X2, [X1,X2]).
msort_(>, X1, X2, [X2,X1]).

predmerge(_, [], R, R) :- !.
predmerge(_, R, [], R) :- !.
predmerge(P, [H1|T1], [H2|T2], Result) :-
  call(P, Delta, H1, H2), !,
  predmerge(Delta, P, H1, H2, T1, T2, Result).

predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
  predmerge(P, [H1|T1], T2, R).
predmerge(=, P, H1, H2, T1, T2, [H1,H2|R]) :-
  predmerge(P, T1, T2, R).
predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
  predmerge(P, T1, [H2|T2], R).



%! sort_stream(+In:istream, -Out:ostream) is det.
%! sort_stream(+In:istream, -Out:ostream, +Options:options) is det.
%
% @arg Options The following options are supported:
%
%      * buffer_size(+nonneg)
%
%        Optionally, the size of the buffer in kilobytes.
%
%      * duplicates(+boolean)
%
%        Whether duplicates are allowed in the result.  Default is
%        `true'.
%
%      * numeric(+boolean)
%
%        Whether numberic sort is performed.  Default is `false'.
%
%      * output(+atom)
%
%        The name of the output file, as processed by
%        `absolute_file_name/[2,3]'.  Default is the input file.
%
%      * temporary_directory(+atom)
%
%        The directory that is used for storing intermediary results
%        of sorting.  Default is the value of setting
%        `temporary_directory'.
%
%      * threads(+positive_integer)
%
%        The number of threads that is used.  Default is the number of
%        available processors, but not larger than 8.  Larger numbers
%        have diminishing returns.  Using $n$ threads increases the
%        memory use by $\log n$.
%
%      * utf8(+boolean)
%
%        Whether the environment is set to UTF-8 encoding.  Default is
%        `false'.

sort_stream(In, Out) :-
  sort_stream(In, Out, []).


sort_stream(In, Out, Options1) :-
  dict_select(env, Options1, [], Options2, EnvT),
  dict_select(utf8, Options2, false, Options3, Utf8),
  (Utf8 == true -> Env = EnvT ; Env = ['LC_ALL'='C'|EnvT]),
  maplist(sort_flag, Options3, Flags),
  process_create(
    path(sort),
    Flags,
    [env(Env),stdin(pipe(ProcIn)),stdout(pipe(Out))]
  ),
  create_detached_thread(
    call_cleanup(
      copy_stream_data(In, ProcIn),
      close(ProcIn)
    )
  ).

% --buffer-size
sort_flag(buffer_size(Size), Flag) :-
  must_be(nonneg, Size),
  format(atom(Flag), '--buffer-size=~d', [Size]).
% -n, --numeric-sort
sort_flag(numeric(IsNumeric), '--numeric-sort') :-
  must_be(boolean, IsNumeric),
  IsNumeric == true.
% -o, --output
sort_flag(output(OutFileSpec), Flag) :-
  absolute_file_name(OutFileSpec, OutFile, [access(write)]),
  format(atom(Flag), '--output=~a', [OutFile]).
% --parallel
sort_flag(threads(NumberOfThreads), Flag) :-
  must_be(positive_integer, NumberOfThreads),
  NumberOfThreads > 0,
  format(atom(Flag), '--parallel=~d', [NumberOfThreads]).
% -T, --temporary-directory
sort_flag(temporary_directory(Dir), Flag) :-
  must_be(directory, Dir),
  format(atom(Flag), '--temporary-directory=~a', [Dir]).
% -u, --unique
sort_flag(duplicates(KeepDuplicates), '--unique') :-
  must_be(boolean, KeepDuplicates),
  KeepDuplicates == false.
