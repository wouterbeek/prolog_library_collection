:- module(
  gnu_sort,
  [
    gnu_sort/2,  % +File, +Opts
    sort_file/1, % +File
    sort_file/2  % +File, +Opts
  ]
).

/** <module> GNU sort

Support for calling GNU sort from within Prolog.

* gnu_sort/2 is the low-level version, performing a one-to-one mapping
  from Prolog options to CLI flags.
* sort_file/2 is the high-level version that simplifies the GNU Sort API.

@author Wouter Beek
@version 2015/08-2015/10, 2016/01
*/

:- use_module(library(cli_ext)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(os/process_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(typecheck)).

:- predicate_options(determine_sort_buffer_size/3, 3, [
     max_sort_buffer_size(+float)
   ]).
:- predicate_options(determine_sort_buffer_threads/3, 3, [
     max_sort_threads(+nonneg)
   ]).
:- predicate_options(gnu_sort/2, 2, [
     buffer_size(+nonneg),
     duplicates(+boolean),
     output(+atom),
     parallel(+positive_integer),
     temporary_directory(+atom),
     utf8(+boolean)
   ]).
:- predicate_options(sort_file/2, 2, [
     sort_dir(+atom),
     pass_to(determine_sort_buffer_size/3, 3),
     pass_to(determine_sort_buffer_threads/3, 3)
   ]).





%! gnu_sort(+File, +Opts) is det.

gnu_sort(File, Opts) :-
  must_be_file(read, File),
  % The UTF-8 encoding option is handled by an environment variable.
  (option(utf8(true), Opts) -> Env = [] ; Env = ['LC_ALL'='C']),
  gnu_sort_args(Opts, Args),
  run_process(sort, [file(File)|Args], [env(Env),program('GNU sort')]).

gnu_sort_args([], []).
gnu_sort_args([buffer_size(Size)|T1], [Arg|T2]) :- !,
  long_flag('buffer-size', Size, Arg),
  gnu_sort_args(T1, T2).
gnu_sort_args([duplicates(false)|T1], ['--unique'|T2]) :- !,
  gnu_sort_args(T1, T2).
gnu_sort_args([output(File)|T1], [Arg|T2]) :- !,
  must_be_file(write, File),
  long_flag(output, File, Arg),
  gnu_sort_args(T1, T2).
gnu_sort_args([parallel(Threads)|T1], [Arg|T2]) :- !,
  long_flag(parallel, Threads, Arg),
  gnu_sort_args(T1, T2).
gnu_sort_args([temporary_directory(Dir)|T1], [Arg|T2]) :- !,
  must_be_directory(Dir),
  long_flag('temporary-directory', Dir, Arg),
  gnu_sort_args(T1, T2).
gnu_sort_args([_|T1], L2) :-
  gnu_sort_args(T1, L2).



%! sort_file(+File) is det.
%! sort_file(+File, +Opts) is det.
% The following options are supported:
%   * max_sort_buffer_size(+float)
%     The maximum size of the sort buffer in Gigabytes.
%     Default is 1.0 GB.
%   * sort_dir(+atom)
%     The directory that is used for disk-based sorting.

sort_file(File) :-
  sort_file(File, []).

sort_file(File, Opts) :-
  % Determine the directory that is used for disk-based sorting.
  (option(sort_dir(Dir), Opts) -> true ; file_directory_name(File, Dir)),
  debug(gnu(sort), "Using directory ~a for disk-based softing.", [Dir]),

  % Determine the buffer size that is used for sorting.
  determine_sort_buffer_size(File, BufferSize, Opts),

  % Determine the number of threads that is used for sorting.
  determine_sort_threads(BufferSize, Threads, Opts),

  % Perform the actual sort.
  gnu_sort(
    File,
    [
      buffer_size(BufferSize),
      duplicates(false),
      output(File),
      parallel(Threads),
      temporary_directory(Dir),
      utf8(true)
    ]
  ).



%! determine_sort_buffer_size(+File, -BufferSize:nonneg, +Opts) is det.
% The following options are supported:
%   * max_sort_buffer_size(+float)
%     The maximum size of the buffer used for sorting.
%     Default is `1.0'.

determine_sort_buffer_size(File, BufferSize, Opts) :-
  calc_sort_buffer_size(File, Calc),
  option(max_sort_buffer_size(Max), Opts, 1.0),
  BufferSize is min(round(Max * (1024 ** 3)), Calc),
  BufferSize0 is BufferSize / (1024 ** 3),
  debug(gnu(sort), "Using buffer size ~2f GB for sorting.", [BufferSize0]).



%! determine_sort_buffer_threads(+BufferSize:nonneg, -Threads:nonneg, +Opts) is det.
% The following options are supported:
%   * max_sort_threads(+nonneg)
%     The maximum number of threads that is allowed to be used.
%     Default is the value of `current_prolog_flag(cpu_count, X)'.

determine_sort_threads(BufferSize, Threads, Opts) :-
  calc_sort_threads(BufferSize, Calc),
  default_number_of_threads(Def),
  option(max_sort_threads(Max), Opts, Def),
  Threads is min(Max, Calc),
  debug(gnu(sort), "Using ~D threads for sorting.", [Threads]).



%! calc_sort_threads(+BufferSize:nonneg, -Threads:nonneg) is det.
% Heuristically determine the number of threads to use for sorting
% a file with the given BufferSize.

% x > 6 GB
calc_sort_threads(BufferSize, 3) :-
  BufferSize > 6 * (1024 ** 3), !.
% 3 GB < x ⪬ 6 GB
calc_sort_threads(BufferSize, 2) :-
  BufferSize > 3 * (1024 ** 3), !.
% x ⪬ 3 GB
calc_sort_threads(_, 1).



%! calc_sort_buffer_size(+File, -BufferSize:nonneg) is det.
% Determines the BufferSize that will be used for sorting File
% according to a simple heuristic.

calc_sort_buffer_size(File, BufferSize) :-
  size_file(File, FileSize),
  (   FileSize =:= 0
  ->  BufferSize = 1024
  ;   BufferSize is round(FileSize * log(FileSize))
  ).
