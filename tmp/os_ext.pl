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
        process_create(renice, [Pid], []),
    ;   existence_error(process, Pid)
    )
  )).
