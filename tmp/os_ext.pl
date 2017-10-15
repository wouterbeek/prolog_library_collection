:- module(
  os_ext,
  [
    os_root_prefix/1, % ?Prefix
    renice/2          % +Pid, +Nice
  ]
).
:- reexport(library(true)).

/** <module> OS extensions

Support for using external programs and other OS functions.

@author Wouter Beek

@version 2015/08-2017/01, 2017/04
*/

:- use_module(library(file_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(process)).





%! os_root_prefix(+Prefix) is semidet.
%! os_root_prefix(-Prefix) is multi.

:- if(os(unix)).
os_root_prefix(/).
:- endif.
:- if(os(windows)).
os_root_prefix('C:\\').
:- endif.



%! renice(+Pid:positive_integer, +Nice:between(-20,19)) is det.

renice(Pid, N) :-
  with_mutex(process_id,(
    (   process_id(Pid)
    ->  must_be(between(-20,19), N),
        process_create(
          path(renice),
          [10,Pid],
          [process(Pid0),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
        ),
        thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
        thread_create(copy_stream_data(ProcOut, user_output), _, [detached(true)]),
        read_stream_to_codes(ProcOut, Codes),
        process_wait(Pid, exit(Status)),
        (Status =:= 0 -> true ; print_message(warning, process_status(Status)))
    ;   existence_error(process, Pid)
    )
  )).
