:- module(
  file_ext,
  [
    file_age/2, % +File:atom
                % -Age:float
    touch/1 % +File:atom
  ]
).

/** <module> File extensions

Extensions to the file operations in the standard SWI-Prolog libraries.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(process)).





%! file_age(+File:atom, -Age:float) is det.

file_age(File, Age):-
  time_file(File, LastModified),
  get_time(Now),
  Age is Now - LastModified.



%! touch(+File:atom) is det.
% @tbd Windows support.

touch(File):-
  process_create(path(touch), [file(File)], []).
