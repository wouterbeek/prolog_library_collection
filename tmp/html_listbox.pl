:- module(
  html_listbox,
  [
    html_listbox//1, % +L
    html_listbox//2  % +L, +Opts
  ]
).

/** <module> HTML listbox

Support for generating HTML drop-down lists.

@author Wouter Beek
@version 2015/08, 2015/12, 2016/06, 2016/08
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- meta_predicate
    html_listbox(+, :, ?, ?),
    html_options(3, +, ?, ?).

is_meta(item_writer).





%! html_listbox(+Items:list(compound))// is det.
% Wrapper around html_listbox//2 with default options.

html_listbox(L) -->
  html_listbox(L, []).


%! html_listbox(+Items:list(compound), +Options:list(compound))// is det.
% Generates an HTML listbox.
% Items have the following form:
%
% ```prolog
% option(+Value:atom, +Label:atom, +Selected:boolean)
% ```
%
% ### Options
%
% The following options are supported:
%   - attributes(+list)
%     HTML attributes of the SELECT element.
%   - item_writer(+callable)
%     The HTML DCG rules that is used to generate each item.

html_listbox(L, Opts0) -->
  {
    meta_options(is_meta, Opts0, Opts),
    option(attributes(Attrs0), Opts, []),
    option(item_writer(ItemWriter), Opts, html_option),
    (   option(multiple(_), Attrs0)
    ->  Attrs = Attrs0
    ;   selectchk(option(_,_,true), L, L0),
        memberchk(option(_,_,true), L0)
    ->  merge_options([multiple(true)], Attrs0, Attrs)
    ;   Attrs = Attrs0
    )
  },
  html(select(Attrs, \html_options(ItemWriter, L))).

% html_options(:ItemWriter, +Items:list(compound))// is det.

html_options(_, []) --> [].
html_options(Goal, [H|T]) -->
  html_call(Goal, H),
  html_options(Goal, T).

%! html_option(+Item:compound)// is det.

html_option(option(Value,Label,Selected)) -->
  html(option([selected=Selected,value=Value], Label)).
