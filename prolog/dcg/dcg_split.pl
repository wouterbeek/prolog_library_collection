:- module(
  dcg_split,
  [
    dcg_split//2 % :Sep_0, -Substring
  ]
).

:- use_module(library(dcg/dcg_ext)).

:- meta_predicate
    dcg_split(//, -, ?, ?).





%! dcg_sep_list(:Sep_0, -Substring)// is nondet.
%
% Succeeds when Substrings are interspersed with separators Sep_0.

dcg_split(Sep_0, Cs) -->
  ...(Cs0),
  Sep_0, !,
  (   done,
      {Cs = Cs0}
  ;   dcg_split(Sep_0, Cs)
  ).
dcg_split(_, Cs) -->
  ...(Cs),
  eos.
