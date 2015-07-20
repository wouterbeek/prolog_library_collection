:- module(
  dcg_list,
  [
    dcg_list//2 % +Elements:list
                % +Options:list(nvpair)
  ]
).

/** <module> DCG list

Support for generating lists using DCG rules.

@author Wouter Beek
@version 2014/03, 2014/07, 2015/02
*/

:- use_module(library(dcg/basics)).
:- use_module(library(option)).

:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_meta)).

:- meta_predicate(dcg_list(+,:,?,?)).
:- meta_predicate(dcg_list_item(3,+,+,+,?,?)).

is_meta(element_writer).



%! dcg_list(+Elements:list, +Options:list(nvpair))// is det.
% The following options are supported:
%   * `element_writer(+atom)`
%     Default: `pl_term`.
%   * `first_count(+integer)`
%     Default: `1`.
%   * `indent(+nonneg)`
%     Default: `0`.
%   * `style(+oneof([enumerate,itemize]))`
%     Default: `itemize`.

dcg_list(Elements, Options1) -->
  {
    meta_options(is_meta, Options1, Options2),
    option(element_writer(Dcg), Options2, pl_term),
    option(first_count(Count), Options2, 1),
    option(indent(Indent), Options2, 0),
    option(style(Style), Options2, itemize)
  },
  dcg_list(Dcg, Indent, Style, Count, Elements).

dcg_list(_, _, _, _, []) --> !, [].
dcg_list(Dcg, Indent, Style, Count1, [Element|Elements]) -->
  indent(Indent, dcg_list_item(Dcg, Style, Count1, Element)), nl,
  {Count2 is Count1 + 1},
  dcg_list(Dcg, Indent, Style, Count2, Elements).

dcg_list_item(Dcg, Style, Count, Element) -->
  dcg_list_item_style(Style, Count),
  dcg_call_cp(Dcg, Element).

dcg_list_item_style(enumerate, Count) --> !,
  integer(Count),
  `. `.
dcg_list_item_style(itemize, _) -->
  `* `.

