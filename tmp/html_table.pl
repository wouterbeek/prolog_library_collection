:- module(
  html_table,
  [
    html_direct_table//1, % +Rows
    html_direct_table//2  % +Rows:list(list), +Opts
  ]
).

/** <module> HTML direct tables

Utilities for generating and HTML tables containing data (not layout).

@author Wouter Beek
@version 2015/08, 2015/11-2015/12, 2016/02, 2016/08
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/http_wrapper), [http_current_request/1]).
:- use_module(library(option)).

:- html_meta
   direct_table_body(+, +, +, +, html, ?, ?),
   direct_table_cells(+, +, html, ?, ?),
   direct_table_header(+, +, html, -, ?, ?),
   html_direct_table(+, :, ?, ?).

is_meta(caption).
is_meta(cell).
is_meta(header).





%! html_direct_table(+Rows:list(list))// is semidet.
%! html_direct_table(+Rows:list(list), +Opts)// is semidet.
% Given a list, each of whose members is either a list
% or a term head(List), outputs an HTML table representing the data.
%
% Upon encountering a `head(+list)` term the elements of List are
% output as TH tags.
%
% Bare lists are output as TD tags,
%
% Fails silently if the data cannot be parsed.
%
% The generated HTML sets the class of body rows to even or odd
% alternately to allow alternate row styling.

html_direct_table(Data) -->
  html_direct_table(Data, []).

html_direct_table(Data0, Opts0) -->
  {
    meta_options(is_meta, Opts0, Opts),
    option(attributes(Attrs), Opts, []),
    option(caption(Caption), Opts, _),
    option(cell(Cell_1), Opts, html_hook),
    option(indexed(Indexed), Opts, false),
    option(maximum_number_of_rows(Max), Opts, inf)
  }, !,
  html(
    table([class=[table,'table-bordered','table-condensed']|Attrs], [
      \table_caption(Caption),
      \direct_table_header(Indexed, Data0, Cell_1, Data),
      tbody(\direct_table_body(1, Max, Indexed, Data, Cell_1))
    ])
  ).


direct_table_header(Indexed, [head(H)|T], Cell_1, T) --> !,
  html(
    thead(
      tr([
        \header_index_cell(Indexed),
        \direct_table_cells(header, H, Cell_1)
      ])
    )
  ).
direct_table_header(_, T, _, T) --> [].

direct_table_body(_, _, _, [], _) --> [].
direct_table_body(Row, Max, Indexed, [H|T], Cell_1) -->
  {
    Row @=< Max,
    NewRow is Row + 1
  },
  html(
    tr([
      \index_cell(Row, Indexed),
      \direct_table_cells(data, H, Cell_1)
    ])
  ),
  direct_table_body(NewRow, Max, Indexed, T, Cell_1).

direct_table_cells(_, [], _) --> [].
direct_table_cells(Tag, [H|T], Cell_1) -->
  (   {Tag == data}
  ->  html(td(\html_call(Cell_1, H)))
  ;   {Tag == header}
  ->  html(th(\html_call(Cell_1, H)))
  ),
  direct_table_cells(Tag, T, Cell_1).





% HELPERS %

%! header_index_cell(+Indexed:boolean)// is det.

header_index_cell(true) --> html(th('#')).
header_index_cell(false) --> [].


%! index_cell(+Row:nonneg, +Indexed:boolean)// is det.

index_cell(_, false) --> [].
index_cell(Row, true) --> html(td(Row)).


%! succ_inf(
%!   +X:or([nonneg,oneof([inf])]),
%!   -Y:or([nonneg,oneof([inf])])
%! ) is det.

succ_inf(inf, inf):- !.
succ_inf(X, Y):- succ(X, Y).


%! table_caption(:Caption)// is det.
% Generates the HTML table caption,
% where the content of the caption element is set by a DCG rule.
%
% @arg Caption A DCG rule generating the content of the caption element,
%              or uninstantiated, in which case no caption is generated.

table_caption(NoCaption) --> {var(NoCaption)}, !, [].
table_caption(Caption)   --> html(caption(\html_call(Caption))).
