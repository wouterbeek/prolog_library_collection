:- module(
  dcg_file,
  [
    file_path//1 % -Path:atom
  ]
).

/** <module> DCG file

Grammar snippets for files.

@author Wouter Beek
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/os_ext)).





%! file_path(-Path:atom)// is semidet.
% Parses legal directory names,
% where both Unix and Windows formats are supported.
%
% ## Example
%
% Unix directory input:
% ```
% /home/wbeek/Dropbox/IOTW
% ```
%
% Windows directory input:
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
  *(path_segment, T),
  {atomic_list_concat([''|T], /, Path)}.



%! local_file_name(-Local:atom)// .

local_file_name(Local) --> *(local_file_char, Local).



%! root_prefix// .

root_prefix -->
  {root_prefix(RootPrefix)},
  atom(RootPrefix).





% HELPERS %

dir_char(C)   --> alphadigit(C).
dir_char(0'.) --> ".".
dir_char(0'-) --> "-".
dir_char(0'+) --> "+".
dir_char(0'_) --> "_".


:- if(os(unix)).
dir_sep --> "/".
:- endif.
:- if(os(windows)).
dir_sep --> "\\".
:- endif.


local_file_char(C)   --> alphadigit(C).
local_file_char(0'.) --> ".".


path_segment(S) --> *(dir_char, Cs), {string_codes(S, Cs)}.


path_segments([H|T]) --> path_segment(H), *(sep_path_segment, T).
path_segments([])    --> "".


sep_path_segment(X) --> dir_sep, path_segment(X).
