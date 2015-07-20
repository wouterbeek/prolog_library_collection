:- module(
  dcg_file,
  [
    file_path//1, % -Dir:atom
    local_file_name//1, % % -Local:atom
    root_prefix//0
  ]
).

/** <module> DCG: File

Grammar snippets for files.

@author Wouter Beek
@version 2014/11-2014/12
*/

:- use_module(library(dcg/basics)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_meta)).
:- use_module(plc(generics/atom_ext)). % Meta-option.
:- use_module(plc(io/file_ext)).
:- use_module(plc(os/os_ext)).





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
  '*'(path_segment, Segments, [separator(directory_separator)]),
  {
    atomic_list_concat(Segments, /, RelPath),
    relative_file_path(Path, '~', RelPath)
  }.
% Absolute directory.
file_path(Path) -->
  root_prefix,
  '*'(path_segment, Segments, [separator(directory_separator)]),
  {atomic_list_concat([''|Segments], /, Path)}.



%! local_file_name(-Local:atom)// .

local_file_name(Local) -->
  dcg_atom(local_file_char, Local).



%! root_prefix// .

root_prefix -->
  {root_prefix(RootPrefix)},
  atom(RootPrefix).





% HELPERS

%! directory_char(?Code:nonneg)// .
% Character that are allowed to occur in a file path.

directory_char(Code) --> ascii_letter(Code).
directory_char(Code) --> decimal_digit(Code).
directory_char(Code) --> dot(Code).
directory_char(Code) --> minus_sign(Code).
directory_char(Code) --> plus_sign(Code).
directory_char(Code) --> underscore(Code).



%! path_segment(-Segment:atom)// .

path_segment(Segment) -->
  dcg_atom('*'(directory_char, []), Segment).



%! directory_separator// .

:- if(is_unix).
directory_separator --> "/".
:- endif.
:- if(is_windows).
directory_separator --> "\\".
:- endif.



%! local_file_char(?Code:nonneg)// .

local_file_char(Code) --> ascii_alpha_numeric(Code).
local_file_char(Code) --> dot(Code).

