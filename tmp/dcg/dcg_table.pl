:- module(
  dcg_table,
  [
    dcg_table//3, % :Caption
                  % +Rows:list(list)
                  % +Options:list(nvpair)
    dcg_table//4 % :Caption
                 % :Cell
                 % +Rows:list(list)
                 % +Options:list(nvpair)
  ]
).

/** <module> DCG: Table

Generates tables for text-based display.

@author Wouter Beek
@tbd Implemented highlighted cells.
@version 2014/02, 2014/11
*/

:- use_module(library(dcg/basics)).
:- use_module(library(option)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_arrow)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_meta)).

:- meta_predicate(dcg_table(//,+,?,?,:)).
:- meta_predicate(dcg_table(//,3,+,?,?,:)).
:- meta_predicate(dcg_table_caption(//,?,?)).
:- meta_predicate(dcg_table_cell(+,3,+,?,?)).
:- meta_predicate(dcg_table_data_row(+,+,1,3,+,?,?)).
:- meta_predicate(dcg_table_header(+,+,3,+,-,?,?)).
:- meta_predicate(dcg_table_header_row(3,+,?,?)).
:- meta_predicate(dcg_table_index_cell(+,+,3,+,?,?)).

is_meta(highlighted_row).

:- predicate_options(dcg_table//3, 3, [
  pass_to(dcg_table//4, 4)
]).
:- predicate_options(dcg_table//4, 4, [
  header_column(+boolean),
  header_row(+boolean),
  highlighted_row(+callable),
  indexed(+boolean)
]).





dcg_cell_border -->
  " | ".



%! dcg_table(
%!   :Caption,
%!   +Rows:list(list),
%!   +Options:list(nvpair)
%! )// is det.
%! dcg_table(
%!   :Caption,
%!   :Cell,
%!   +Rows:list(list),
%!   +Options:list(nvpair)
%! )// is det.
% Generates the HTML markup for a table.
%
% The following options are supported:
%   1. `header_column(boolean)`
%      Uses `th` tags for cells in the first column.
%      Default: `false`.
%   2. `header_row(boolean)`
%      Whether or not the first row should be
%      displayed as the table header row.
%      Default is `false`.
%   3. `highlighted_row(:HighlightedRow)`
%      A semidet predicate term that is missing its last parameter.
%      Default: `false` for no row highlighted.
%   4. `indexed(+Indexed:boolean)`
%      Whether or not each row should begin with a row index.
%      Counts starts at 0. The header row, if included, is not counted.
%      Default is `false`.

dcg_table(Caption, Rows, Options) -->
  dcg_table(Caption, atom, Rows, Options).

dcg_table(Caption, Cell, Rows, Options1) -->
  {
    flag(table_row, _, 0),
    meta_options(is_meta, Options1, Options2),
    option(header_column(HasHeaderColumn), Options2, false),
    option(header_row(HasHeaderRow), Options2, false),
    option(highlighted_row(HighlightedRow), Options2, fail),
    option(indexed(IsIndexed), Options2, false)
  },
  dcg_table_caption(Caption),
  horizontal_line(60),
  nl,
  dcg_table_header(HasHeaderRow, IsIndexed, Cell, Rows, DataRows),
  '*'(
    dcg_table_data_row(HasHeaderColumn, IsIndexed, HighlightedRow, Cell),
    DataRows,
    [copy_term(true)]
  ),
  horizontal_line(60),
  nl.



%! dcg_table_caption(:Caption)// is det.
% Generates the table caption,
%  where the content of the caption element is set by a DCG rule.
%
% @arg Caption A DCG rule generating the content of the caption element,
%      or uninstantiated, in which case no caption is generated at all.

dcg_table_caption(VAR) -->
  {var(VAR)}, !,
  "".
dcg_table_caption(Caption) -->
  dcg_call_cp(Caption),
  nl.



%! dcg_table_cell(
%!   +Type:oneof([data,header]),
%!   :Cell,
%!   +Element:ground
%! )// is det.
% Generated an the content for a table cell (both header and data).

dcg_table_cell(data, Cell, Element) -->
  dcg_call_cp(Cell, Element),
  horizontal_tab.
dcg_table_cell(header, Cell, Element) -->
  dcg_between("*", dcg_call_cp(Cell, Element)),
  horizontal_tab.



%! dcg_table_data_row(
%!   +HasHeaderColumn:boolean,
%!   +IsIndexed:boolean,
%!   :Highlighted,
%!   :Cell,
%!   +DataRow:list
%! )// is det.
% @tbd Set whether the row is highlighted or not.

dcg_table_data_row(
  HasHeaderColumn,
  IsIndexed,
  Highlighted,
  Cell,
  DataRow
) -->
  {flag(table_row, RowNumber, RowNumber + 1)},
  (   {call(Highlighted, RowNumber)}
  ->  "" % @tbd
  ;   "" % @tbd
  ),

  ({
    HasHeaderColumn == true,
    IsIndexed == false,
    DataRow = [HeaderCell|DataRow0]
  }->
    dcg_table_cell(header, Cell, HeaderCell),
    dcg_between(
      dcg_cell_border,
      '*'(dcg_table_cell(data, Cell), DataRow0, [copy_term(true)])
    )
  ;
    dcg_table_index_cell(HasHeaderColumn, IsIndexed, Cell, RowNumber),
    dcg_between(
      dcg_cell_border,
      '*'(dcg_table_cell(data, Cell), DataRow, [copy_term(true)])
    )
  ),
  nl.



%! dcg_table_header(
%!   +HasHeaderRow:boolean,
%!   +IsIndexed:boolean,
%!   :Cell,
%!   +Rows:list(list),
%!   -DataRows:list(list)
%! )// is det.

% Options state a header row should be included.
% We take the first row, and return the other rows for later processing.
% Only add a header if the corresponding option says so.
dcg_table_header(true, IsIndexed, Cell, [HeaderRow1|DataRows], DataRows) --> !,
  % If the indexed option is set, then include a first header cell
  % indicating the index number column.
  {(  IsIndexed == true
  ->  HeaderRow2 = ['#'|HeaderRow1]
  ;   HeaderRow2 = HeaderRow1
  )},
  dcg_table_header_row(Cell, HeaderRow2).
% In case the header option is not set, simply return the given rows.
dcg_table_header(false, _, _, DataRows, DataRows) --> "".



%! dcg_table_header_row(:Cell, +HeaderRow:list)// is det.
% Generates the HTML table header row with given contents.

dcg_table_header_row(Cell, HeaderRow) -->
  dcg_between(
    dcg_cell_border,
    '*'(dcg_table_cell(header, Cell), HeaderRow, [copy_term(true)])
  ),
  nl,
  horizontal_line(60),
  nl.



%! dcg_table_index_cell(
%!   +HasHeaderColumn:boolean,
%!   +IsIndexed:boolean,
%!   :Cell,
%!   +Index:ground
%! )// is det.

dcg_table_index_cell(HasHeaderColumn, true, Cell, Index) -->
  {(  HasHeaderColumn == true
  ->  Type = header
  ;   Type = data
  )},
  dcg_table_cell(Type, Cell, Index).
dcg_table_index_cell(_, false, _, _) --> "".

