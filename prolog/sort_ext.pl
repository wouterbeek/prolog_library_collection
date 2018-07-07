:- module(
  sort_ext,
  [
    sort_stream/2, % +In, -Out
    sort_stream/3  % +In, -Out, +Options
  ]
).

/** <module> Sort extensions

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(thread_ext)).





%! sort_stream(+In:stream, -Out:stream) is det.
%! sort_stream(+In:stream, -Out:stream, +Options:list(compound)) is det.
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
  select_option(env(EnvT), Options1, Options2, []),
  select_option(utf8(Utf8), Options2, Options3, false),
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
