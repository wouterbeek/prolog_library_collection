:- module(
  csv_ext,
  [
    csv_to_file/2,      % +File, +Rows
    csv_to_file/3,      % +File, +Rows, +Opts
    tsv_read_file/2,    % +File, -Rows
    tsv_read_file/3,    % +File, -Rows, +Opts
    tsv_write_stream/2, % +Out,  +Rows
    tsv_write_stream/3  % +Out,  +Rows, +Opts
  ]
).
:- reexport(library(csv)).

/** <module> CSV extensions

@author Wouter Beek
@version 2015/10-2015/11, 2016/01, 2016/04
*/

:- use_module(library(io)).
:- use_module(library(option)).
:- use_module(library(yall)).





%! csv_to_file(+Sink, +Rows) is det.
%! csv_to_file(+Sink, +Rows, +Opts) is det.
%
% Options are passed to:
%
%   * call_to_stream/3
%   * csv_to_stream/3

csv_to_file(Sink, Rows) :-
  csv_to_file(Sink, Rows, []).


csv_to_file(Sink, Rows, Opts) :-
  call_to_stream(
    Sink,
    {Rows,Opts}/[Out]>>csv_write_stream(Out, Rows, Opts),
    Opts
  ).



%! tsv_read_file(+File, -Rows:list(compound)) is det.
%! tsv_read_file(+File, -Rows:list(compound), +Opts) is det.

tsv_read_file(File, Rows):-
  tsv_read_file(File, Rows, []).


tsv_read_file(File, Rows, Opts0):-
  merge_options([separator(9)], Opts0, Opts),
  csv_read_file(File, Rows, Opts).



%! tsv_write_stream(+Out, +Rows:list(compound)) is det.
%! tsv_write_stream(+Out, +Rows:list(compound), +Opts) is det.

tsv_write_stream(Out, Rows):-
  tsv_write_stream(Out, Rows, []).


tsv_write_stream(Out, Rows, Opts0):-
  merge_options([separator(9)], Opts0, Opts),
  csv_write_stream(Out, Rows, Opts).
