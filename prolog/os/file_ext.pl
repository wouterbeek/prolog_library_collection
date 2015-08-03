:- module(
  file_ext,
  [
    file_age/2, % +File:atom
                % -Age:float
    is_fresh_file/2, % +File:atom
                     % +FreshnessLifetime:between(0.0,inf)
    is_stale_file/2, % +File:atom
                     % +FreshnessLifetime:between(0.0,inf)
    touch/1, % +File:atom
    write_stream_to_file/2 % +Write:stream
                           % +File:atom
  ]
).

/** <module> File extensions

Extensions to the file operations in the standard SWI-Prolog libraries.

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(process)).





%! file_age(+File:atom, -Age:float) is det.

file_age(File, Age):-
  time_file(File, LastModified),
  get_time(Now),
  Age is Now - LastModified.



%! is_fresh_age(
%!   +Age:between(0.0,inf),
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.

is_fresh_age(_, inf):- !.
is_fresh_age(Age, FreshnessLifetime):-
  Age =< FreshnessLifetime.


%! is_fresh_file(+File:atom, +FreshnessLifetime:between(0.0,inf)) is semidet.

is_fresh_file(File, FreshnessLifetime):-
  file_age(File, Age),
  is_fresh_age(Age, FreshnessLifetime).



%! is_stale_age(
%!   +Age:between(0.0,inf),
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.

is_stale_age(_, inf):- !, fail.
is_stale_age(Age, FreshnessLifetime):-
  Age > FreshnessLifetime.



%! is_stale_file(+File:atom, +FreshnessLifetime:between(0.0,inf)) is semidet.

is_stale_file(File, FreshnessLifetime):-
  file_age(File, Age),
  is_stale_age(Age, FreshnessLifetime).



%! touch(+File:atom) is det.
% @tbd Windows support.

touch(File):-
  process_create(path(touch), [file(File)], []).



%! write_stream_to_file(+Read:stream, +File:atom) is det.

write_stream_to_file(Read, File):-
  setup_call_cleanup(
    open(File, write, Write, [type(binary)]),
    copy_stream_data(Read, Write),
    close(Write)
  ).
