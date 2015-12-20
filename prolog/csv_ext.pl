:- module(
  csv_ext,
  [
    csv_write_stream/2, % +Write:stream
                        % +Rows:list(compound)
    tsv_read_file/2, % +File:atom
                     % -Rows:list(compound)
    tsv_read_file/3, % +File:atom
                     % -Rows:list(compound)
                     % +Options:list(compound)
    tsv_write_stream/2, % +Write:stream
                        % +Rows:list(compound)
    tsv_write_stream/3 % +Write:stream
                       % +Rows:list(compound)
                       % +Options:list(compound)
  ]
).
:- reexport(library(csv)).

/** <module> CSV extensions

@author Wouter Beek
@version 2015/10-2015/11
*/

:- use_module(library(option)).

:- predicate_options(tsv_read_file/3, 3, [
     pass_to(csv_read_file/3, 3)
   ]).
:- predicate_options(tsv_write_stream/3, 3, [
     pass_to(csv_write_stream/3, 3)
   ]).





%! csv_write_stream(+Write:stream, +Rows:list(compound)) is det.
% Wrapper around csv_write_stream/3 with default options.

csv_write_stream(Write, Rows):-
  csv_write_stream(Write, Rows, []).



%! tsv_read_file(+File:atom, -Rows:list(compound)) is det.
% Wrapper around tsv_read_file/3 with default options.

tsv_read_file(File, Rows):-
  tsv_read_file(File, Rows, []).


%! tsv_read_file(
%!   +File:atom,
%!   -Rows:list(compound),
%!   +Options:list(compound)
%! ) is det.

tsv_read_file(File, Rows, Opts0):-
  merge_options([separator(9)], Opts0, Opts),
  csv_read_file(File, Rows, Opts).



%! tsv_write_stream(+Write:stream, +Rows:list(compound)) is det.
% Wrapper around tsv_write_stream/3 with default options.

tsv_write_stream(Write, Rows):-
  tsv_write_stream(Write, Rows, []).



%! tsv_write_stream(
%!   +Write:stream,
%!   +Rows:list(compound),
%!   +Options:list(compound)
%! ) is det.

tsv_write_stream(Write, Rows, Opts0):-
  merge_options([separator(9)], Opts0, Opts),
  csv_write_stream(Write, Rows, Opts).
