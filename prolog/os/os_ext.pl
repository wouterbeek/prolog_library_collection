:- module(
  os_ext,
  [
    os/1, % ?Os:is
    os_path/1 % ?Part:atom
  ]
).

/** <module> OS extensions

Predicates for handling OS-specific settings.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(error)).
:- use_module(library(lists)).

:- multifile(error:has_type/2).

error:has_type(os, T):-
  error:has_type(oneof([apple,unix,windows]), T).





%! os(+Os:os) is semidet.
%! os(-Os:os) is det.
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
os(X):-
  type_error(os, X).



%! os_path(+Part:atom) is semidet.
%! os_path(-Part:atom) is nondet.
% Succeeds if Part is on the OS path.

os_path(PlPart):-
  getenv('PATH', Path),
  os_path_separator(Sep),
  atomic_list_concat(Parts, Sep, Path),
  member(Part, Parts),
  prolog_to_os_filename(PlPart, Part).



%! os_path_separator(+Separator:atom) is semidet.
%! os_path_separator(-Separator:atom) is det.
% Suceeds if Separator is the OS path separator character.

os_path_separator(Sep):-
  os(Os),
  os_path_separator(Os, Sep).

%! os_path_separator(+Os:os, -Separator:atom) is det.

os_path_separator(Os, Sep):-
  os(Os),
  (   memberchk(Os, [mac,unix])
  ->  Sep = (:)
  ;   Os == windows
  ->  Sep = (;)
  ).
