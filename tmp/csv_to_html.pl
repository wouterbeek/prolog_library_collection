:- module(
  csv_to_html,
  [
    csv_to_html/2, % +CsvFile, ?HtmlFile
    csv_to_html/3  % +CsvFile, ?HtmlFile, +Opts
  ]
).

/** <module> CSV to HTML conversion

@author Wouter Beek
@version 2015/11-2012/12, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(csv_ext)).
:- use_module(library(default)).
:- use_module(library(list_ext)).
:- use_module(library(http/html_write)).





%! csv_to_html(+CsvFile ?HtmlFile) is det.
%! csv_to_html(+CsvFile ?HtmlFile, +Opts) is det.
%
% Options are passed to read_csv/3 and open/4.

csv_to_html(File1, File2):-
  csv_to_html(File1, File2, []).


csv_to_html(File1, File2, Opts):-
  defgoal(add_file_extension(File1, html), File2),
  csv_read_file(File1, Rows, Opts),
  maplist(list_row, Lists, Rows),
  phrase(html_direct_table(Lists), Tokens),
  setup_call_cleanup(
    open(File2, write, Write, Opts),
    print_html(Write, Tokens),
    close(Write)
  ).





% HELPERS %

add_file_extension(File1, Ext, File2):-
  atomic_list_concat([File1,Ext], ., File2).
