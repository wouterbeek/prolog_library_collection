:- module(
  dcg_abnf,
  [
    dcg_once//1  % :Dcg_0
  ]
).

:- meta_predicate
    dcg_once(//, ?, ?).





%! dcg_once(:Dcg_0)// is det.
%
% @see once/1

dcg_once(Dcg_0, X, Y) :-
  once(dcg_call(Dcg_0, X, Y)).
