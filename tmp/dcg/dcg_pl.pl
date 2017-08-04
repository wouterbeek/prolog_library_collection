:- module(
  dcg_pl,
  [
    pl_date_time//1,       % +DT
    pl_dict//1,            % +Dict
    pl_dict//2,            % +Dict, +Indent
    pl_list//1,            % +L
    pl_list//2,            % +L, +Indent
    pl_predicate//1,       % +Predicate
    pl_stream_position//1, % +Pos
    pl_term//1,            % @Term
    pl_term//2             % @Term, +Indent
  ]
).

/** <module> DCG Prolog terms

DCG rules for printing terms.

@author Wouter Beek
@version 2015/08, 2015/10-2015/12, 2016/02, 2016/05, 2016/07-2016/08, 2016/11
*/

:- use_module(library(date_time/date_time)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(list_ext)).
:- use_module(library(typecheck)).

:- multifile
    dcg:dcg_hook//1.

dcg:dcg_hook(I) -->
  {integer(I)}, !,
  thousands(I).
dcg:dcg_hook(F) -->
  {float(F)}, !,
  float(F).
dcg:dcg_hook(S) -->
  {string(S)}, !,
  atom(S).
dcg:dcg_hook(A) -->
  {atom(A)}, !,
  atom(A).
dcg:dcg_hook(DT) -->
  pl_date_time(DT), !.




%! pl_date_time(+DT)// is det.

pl_date_time(DT, X, Y):-
  'date_time_to_dt-pl'(DT, D),
  format_time(codes(X,Y), "%FT%T%z", D).



%! pl_dict(+Dict)// is det.
%! pl_dict(+Dict, +Indent)// is det.

pl_dict(Dict) -->
  pl_dict(Dict, 0).


pl_dict(Dict, I) -->
  pl_term(Dict, I).



%! pl_list(+L)// is det.
%! pl_list(+L, +Indent)// is det.

pl_list(L) -->
  pl_list(L, 0).


pl_list(L, I) -->
  dcg_list(pl_term, L, I).



%! pl_predicate(+Predicate)// is det.

pl_predicate(Mod:Pred/Arity) -->
  atom(Mod), ":", atom(Pred), "/", integer(Arity).



%! pl_stream_position(+Position:compound)// is det.

pl_stream_position(stream(_,Row,Col,_)) -->
  "[Row: ", thousands(Row), ", Col: ", thousands(Col), "]".



%! pl_term(@Term)// is det.
%! pl_term(@Term, +Indent)// is det.

pl_term(Term) -->
  pl_term(Term, 0).


pl_term(Term, I) -->
  pl_term0(pl_term, Term, I).


pl_term0(Dcg_1, Dict, I) -->
  {is_dict(Dict)}, !,
  dcg_dict(Dcg_1, Dict, I).
pl_term0(Dcg_1, L, I) -->
  {is_list(L)}, !,
  dcg_list(Dcg_1, L, I).
pl_term0(_, Term, I) -->
  tab(I),
  dcg:dcg_hook(Term), !.
pl_term0(Dcg_1, Term, I1) -->
  {compound(Term)}, !,
  {Term =.. [Pred|Args]},
  tab(I1), atom(Pred), "(", nl,
  {I2 is I1 + 1},
  *(pl_term00(Dcg_1, I2), Args),
  tab(I1), ")", nl.
pl_term0(_, Term, _) -->
  {term_to_atom(Term, A)},
  atom(A).

pl_term00(Dcg_1, I, Term) -->
  pl_term0(Dcg_1, Term, I).
