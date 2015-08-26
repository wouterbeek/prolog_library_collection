:- module(
  dcg_file,
  [
    file_path//1 % -Path:atom
  ]
).

/** <module> DCG file

Grammar snippets for files.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_word)).
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
  path_segment*(Segments),
  {
    atomic_list_concat(Segments, /, RelPath),
    relative_file_name(Path, '~', RelPath)
  }.
% Absolute directory.
file_path(Path) -->
  root_prefix,
  path_segment*(Segments),
  {atomic_list_concat([''|Segments], /, Path)}.



%! local_file_name(-Local:atom)// .

local_file_name(Local) -->
  dcg_atom(local_file_char, Local).



%! root_prefix// .

root_prefix -->
  {root_prefix(RootPrefix)},
  atom(RootPrefix).





% HELPERS %

%! directory_char(?Code:nonneg)// .
% Character that are allowed to occur in a file path.

directory_char(C) --> ascii_letter(C).
directory_char(C) --> decimal_digit(C).
directory_char(C) --> dot(C).
directory_char(C) --> minus_sign(C).
directory_char(C) --> plus_sign(C).
directory_char(C) --> underscore(C).



%! 'path_segment*'(-Segments:list(atom))// .

path_segment*([H|T]) -->
  dcg_atom(path_segment, H), !,
  (   directory_separator
  ->  path_segment*(T)
  ;   {T = []}
  ).
path_segment*([]) --> "".



%! path_segment(-Segment:list(code))// .

path_segment([H|T]) -->
  directory_char(H), !,
  path_segment(T).
path_segment([]) --> "".



%! directory_separator// .

:- if(os(unix)).
directory_separator --> "/".
:- endif.
:- if(os(windows)).
directory_separator --> "\\".
:- endif.



%! local_file_char(?Code:nonneg)// .

local_file_char(C) --> ascii_alpha_numeric(C).
local_file_char(C) --> dot(C).
