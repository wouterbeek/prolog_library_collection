:- module(
  dcg_list,
  [
    dcg_list//2, % +Elements:list
                 % +Options:list(compound)
    dcg_list//3 % :Writer_1
                % +Elements:list
                % +Options:list(compound)
  ]
).

/** <module> DCG list

Print numbered and unnumbered lists using DCGs.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_pl_term)).
:- use_module(library(option)).

:- meta_predicate(dcg_list(3,+,+,?,?)).
:- meta_predicate(dcg_list(3,+,+,+,+,?,?)).





%! dcg_list(+Elements:list, +Options:list(nvpair))// is det.
% Wrapper around dcg_list//3 using the default writer.

dcg_list(L, Opts) -->
  dcg_list(pl_term, L, Opts).


%! dcg_list(:Writer_1, +Elements:list, +Options:list(nvpair))// is det.
% The following options are supported:
%   * first_count(+nonneg)
%     The number of the first list item.
%     Only used when `style == enumerate`.
%     Default is `1`.
%   * indent(+nonneg)
%     Default is `0`.
%   * style(+oneof([enumerate,itemize]))
%     Default is `itemize`.

dcg_list(Writer_1, L, Opts) -->
  {
    option(first_count(N), Opts, 1),
    option(indent(I), Opts, 0),
    option(style(Style), Opts, itemize)
  },
  dcg_list(Writer_1, I, Style, N, L).


dcg_list(_, _, _, _, []) --> !, [].
dcg_list(Writer_1, I, Style, M, [H|T]) -->
  indent(I),
  (   {Style == enumerate}
  ->  integer(M),
      ". "
  ;   "* "
  ),
  dcg_list_item_prefix(Style, M),
  dcg_call_cp(Writer_1, H).
  nl,
  {succ(M, N)},
  dcg_list(Writer_1, I, Style, N, T).
