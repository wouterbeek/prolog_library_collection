:- module(
  dcg_list,
  [
    dcg_list//2, %            +L, +Opts
    dcg_list//3  % :Writer_3, +L, +Opts
  ]
).

/** <module> DCG list

Print numbered and unnumbered lists using DCGs.

@author Wouter Beek
@version 2015/10, 2016/03-2016/04
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(option)).

:- meta_predicate
    dcg_list(3, +, +, ?, ?),
    dcg_list(3, +, +, +, +, ?, ?).





%! dcg_list(+L, +Opts)// is det.
%! dcg_list(:Writer_3, +L, +Opts)// is det.
% The following options are supported:
%   - first_count(+nonneg)
%     The number of the first list item.
%     Only used when `style == enumerate`.
%     Default is `1`.
%   - indent(+nonneg)
%     Default is `0`.
%   - style(+oneof([enumerate,itemize]))
%     Default is `itemize`.

dcg_list(L, Opts) -->
  dcg_list(term, L, Opts).


dcg_list(Writer_3, L, Opts) -->
  {
    option(first_count(N), Opts, 1),
    option(indent(I), Opts, 0),
    option(style(Style), Opts, itemize)
  },
  dcg_list(Writer_3, I, Style, N, L).


dcg_list(_, _, _, _, []) --> !, [].
dcg_list(Writer_3, I, Style, M, [H|T]) -->
  tab(I),
  dcg_list_item_prefix(Style, M),
  dcg_call_cp(Writer_3, H),
  nl,
  {succ(M, N)},
  dcg_list(Writer_3, I, Style, N, T).

dcg_list_item_prefix(enumerate, M) --> !,
  thousands(M),
  ". ".
dcg_list_item_prefix(itemize, _) -->
  "* ".
