:- module(
  os_ext,
  [
    os/1,     % ?Os
    os_path/1 % ?File
  ]
).

/** <module> OS extensions

Predicates for handling OS-specific settings.

@author Wouter Beek
@version 2015/08, 2017/01
*/

:- use_module(library(error)).
:- use_module(library(lists)).

:- multifile
    error:has_type/2.

error:has_type(os, T):-
  error:has_type(oneof([apple,unix,windows]), T).





%! os(+Os) is semidet.
%! os(-Os) is det.
%
% Succeeds if Os names the current Operating System.
%
% Supported values are:
%   * mac
%   * unix
%   * windows

os(mac):-
  current_prolog_flag(apple, true), !.
os(unix):-
  current_prolog_flag(unix, true), !.
os(windows):-
  current_prolog_flag(windows, true), !.



%! os_path(+File) is semidet.
%! os_path(-File) is nondet.
%
% Succeeds if File is on the OS path.

os_path(File):-
  getenv('PATH', Path),
  os_path_separator(Sep),
  atomic_list_concat(Files, Sep, Path),
  member(File0, Files),
  prolog_to_os_filename(File, File0).



%! os_path_separator(+Separator) is semidet.
%! os_path_separator(-Separator) is det.
% Suceeds if Separator is the OS path separator character.

os_path_separator(Sep):-
  os(Os),
  os_path_separator(Os, Sep).

%! os_path_separator(+Os:os, -Separator) is det.

os_path_separator(Os, Sep):-
  os(Os),
  (   memberchk(Os, [mac,unix])
  ->  Sep = (:)
  ;   Os == windows
  ->  Sep = (;)
  ).
