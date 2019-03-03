:- module(
  csv_ext,
  [
    csv_named_row/2, % +In, -Row
    csv_row/2        % +In, -Row
  ]
).

/** <module> CSV extension

@author Wouter Beek
@version 2017-2019
*/

:- use_module(library(pairs)).





%! csv_named_row(+In:stream, -Row:list(pair(atom,term))) is nondet.

csv_named_row(In, Row) :-
  csv_options_(Options),
  csv:csv_read_row(In, Header, Options),
  compound_name_arguments(Header, row, Keys),
  csv_row_(In, Values, Options),
  pairs_keys_values(Row, Keys, Values).



%! csv_row(+In:stream, -Row:list(term)) is nondet.

csv_row(In, Data) :-
  csv_options_(Options),
  csv_row_(In, Data, Options).





% HELPERS %

%! csv_options_(-Options:list(compound)) is det.

csv_options_(Options) :-
  csv:csv_options(Options, []).



%! csv_row_(+In:stream, -Data:list(term), +Options:list(compound)) is nondet.

csv_row_(In, Data, Options) :-
  repeat,
  csv:csv_read_row(In, Row, Options),
  (Row == end_of_file -> !, fail ; compound_name_arguments(Row, row, Data)).
