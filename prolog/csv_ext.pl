:- module(
  csv_ext,
  [
    csv_read_stream_row/2, % +In, -Row
    csv_read_stream_row/3, % +In, -Row, +Options
    csv_row_list/2
  ]
).
:- reexport(library(csv)).

/** <module> CSV extension

@author Wouter Beek
@version 2017/06
*/

:- use_module(library(error)).
:- use_module(library(option)).





%! csv_read_stream_row(+In, -Row) is nondet.
%! csv_read_stream_row(+In, -Row, +Options) is nondet.
%
% The following options are supported:
%
%   * skip_header(+boolean)
%
%     Default is `true'.
%
%   * Other options are passed to csv_read_row/3.

csv_read_stream_row(In, Row) :-
  csv_read_stream_row(In, Row, []).


csv_read_stream_row(In, Row, Options1) :-
  csv_options(Options2, Options1),
  (   option(skip_header(true), Options1, true)
  ->  csv_read_row(In, _, Options2)
  ;   true
  ),
  repeat,
  csv_read_row(In, Row, Options2),
  (Row == end_of_file -> !, fail ; true).



%! csv_row_list(+Row:compound, +List:list(term)) is semidet.
%! csv_row_list(+Row:compound, -List:list(term)) is det.
%! csv_row_list(-Row:compound, +List:list(term)) is det.

csv_row_list(Row, Args) :-
  compound_name_arguments(Row, row, Args).
