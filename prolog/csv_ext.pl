:- module(
  csv_ext,
  [
    csv_named_row/2, % +In, -Row
    csv_named_row/3, % +In, -Row, +Options
    csv_row/2,       % +In, -Row
    csv_row/3        % +In, -Row, +Options
  ]
).

/** <module> Enhanced CSV support

*/

:- use_module(library(csv)).
:- use_module(library(dict)).
:- use_module(library(pairs)).





%! csv_named_row(+In:istream, -Row:list(pair(atom,term))) is nondet.
%! csv_named_row(+In:istream, -Row:list(pair(atom,term)), +Options:options) is nondet.
%
% @param Options The following options are supported:
%
%        * header(+list(atom)) The list of header names.
%
%        * Other options are passed to csv_row/3.

csv_named_row(In, Row) :-
  csv_named_row(In, Row, options{}).


csv_named_row(In, Row, Options1) :-
  merge_dicts(options{convert: false}, Options1, Options2),
  (   dict_select(header, Options2, Options3, Keys)
  ->  csv:csv_options(Options, Options3)
  ;   csv:csv_options(Options, Options2),
      csv:csv_read_row(In, Header, Options),
      compound_name_arguments(Header, row, Keys)
  ),
  csv_row_(In, Values, Options),
  pairs_keys_values(Row, Keys, Values).



%! csv_row(+In:istream, -Row:list(term)) is nondet.
%! csv_row(+In:istream, -Row:list(term), +Options:options) is nondet.

csv_row(In, Row) :-
  csv_row(In, Row, []).

csv_row(In, Row, Options1) :-
  csv:csv_options(Options2, Options1),
  csv_row_(In, Row, Options2).





% HELPERS %

csv_row_(In, Row, Options) :-
  repeat,
  csv:csv_read_row(In, Data, Options),
  (Data == end_of_file -> !, fail ; compound_name_arguments(Data, row, Row)).
