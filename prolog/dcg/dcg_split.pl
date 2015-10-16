:- module(
  dcg_split,
  [
    dcg_split//2 % :Separator
                 % -Substring:list(code)
  ]
).

:- use_module(library(dcg/dcg_content)).

:- meta_predicate(dcg_split(//,-,?,?)).





%! dcg_sep_list(:Separator, -Substring:list(code))// is nondet.
% Succeeds when the code lists in Codess are processed
% and each is interspersed with Separators.

dcg_split(:Sep, Cs) -->
  ...(Cs0), Sep, !,
  (dcg_done, Cs = Cs0 ; dcg_split(Sep, Cs)).
dcg_split(_, Cs) -->
  ...(Cs),
  eos.
