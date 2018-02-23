:- module(
  dcg_file,
  [
    file_path//1 % -Path:atom
  ]
).

/** <module> DCG file

Grammar snippets for files.

@author Wouter Beek
@version 2015/08, 2015/11, 2017/01-2017/02
*/

:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(os_ext)).





%! file_path(-Path:atom)// is semidet.
%
% Parses legal directory names, where both Unix and Windows formats
% are supported.
%
% # Example
%
% Unix directory:
%
% ```
% /home/wbeek/Dropbox/IOTW
% ```
%
% Windows directory:
%
% ```
% C:\Users\Quirinus\Dropbox\IOTW
% ```

% Relative directory with respect to the home directory (Unix only).
file_path(Path) -->
  "~/", !,
  path_segments(T),
  {
    atomic_list_concat(T, /, RelPath),
    relative_file_name(Path, '~', RelPath)
  }.
% Absolute directory.
file_path(Path) -->
  root_prefix,
  path_segments(T),
  {atomic_list_concat([''|T], /, Path)}.



%! local_file_name(-Local:atom)// .

local_file_name(Local) -->
  *(local_file_char, Local).



%! root_prefix// .

root_prefix -->
  {os_root_prefix(RootPrefix)},
  atom(RootPrefix).





% HELPERS %

directory_char(C)   --> alphadigit(C).
directory_char(0'.) --> ".".
directory_char(0'-) --> "-".
directory_char(0'+) --> "+".
directory_char(0'_) --> "_".

:- if(os(unix)).
directory_sep --> "/".
:- endif.
:- if(os(windows)).
directory_sep --> "\\".
:- endif.

local_file_char(C) --> alphadigit(C).
local_file_char(0'.) --> ".".

path_segment(S) -->
  *(directory_char, Cs),
  {string_codes(S, Cs)}.

path_segments([H|T]) -->
  path_segment(H),
  *(sep_path_segment, T).
path_segments([]) --> "".

sep_path_segment(X) -->
  directory_sep,
  path_segment(X).
