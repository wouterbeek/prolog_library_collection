:- module(
  dcg_split,
  [
    dcg_split//2 % :Separator_2
                 % -Substring:list(code)
  ]
).

:- use_module(library(dcg/dcg_content)).

:- meta_predicate(dcg_split(//,-,?,?)).





%! dcg_sep_list(:Separator_2, -Substring:list(code))// is nondet.
% Succeeds when the code lists in Codess are processed
% and each is interspersed with Separators.

dcg_split(Sep_2, Cs) -->
  ...(Cs0),
  Sep_2, !,
  (   dcg_done,
      {Cs = Cs0}
  ;   dcg_split(Sep_2, Cs)
  ).
dcg_split(_, Cs) -->
  ...(Cs),
  eos.
