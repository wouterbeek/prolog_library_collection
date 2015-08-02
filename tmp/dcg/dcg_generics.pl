:- module(
  dcg_generics,
  [
    dcg_sep_list//2 % :Separator
                    % ?Codess:list(list(code))
  ]
).

:- meta_predicate(dcg_sep_list(//,?,?,?)).



%! dcg_sep_list(:Separator, ?Codess:list(list(code)))// .
% Succeeds when the code lists in Codess are processed
% and each is interspersed with Separators.

dcg_sep_list(Sep, [H|T]) -->
  dcg_sep_item(Sep, H), !,
  dcg_sep_list(Sep, T).
dcg_sep_list(_, []) --> "".

dcg_sep_item(Sep, []) -->
  Sep, !.
dcg_sep_item(Sep, [H|T]) -->
  [H],
  dcg_sep_item(Sep, T).
