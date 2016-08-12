:- module(
  dcg_pl,
  [
    pl_date_time//1,       % +DT
    pl_dict//1,            % +Dict
    pl_dict//2,            % +Dict, +Opts
    pl_list//1,            % +L
    pl_list//2,            % +L, +Opts
    pl_predicate//1,       % +Predicate
    pl_stream_position//1, % +Pos
    pl_term//1,            % @Term
    pl_term//2             % @Term, +Opts
  ]
).

/** <module> DCG Prolog terms

DCG rules for printing terms.

@author Wouter Beek
@version 2015/08, 2015/10-2015/12, 2016/02, 2016/05, 2016/07
*/

:- use_module(library(date_time/date_time)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(list_ext)).
:- use_module(library(typecheck)).





%! pl_date_time(+DT)// is det.

pl_date_time(DT, X, Y):-
  'date_time_to_dt-pl'(DT, D),
  format_time(codes(X,Y), "%FT%T%z", D).



%! pl_dict(+Dict)// is det.
%! pl_dict(+Dict, +Opts)// is det.

pl_dict(Dict) -->
  pl_dict(Dict, _{}).


pl_dict(Dict, Opts) -->
  pl_term(Dict, Opts).



%! pl_list(+L)// is det.
%! pl_list(+L, +Opts)// is det.

pl_list(L) -->
  pl_list(L, []).


pl_list(L, Opts) -->
  dcg_list(pl_term, L, Opts).



%! pl_predicate(+Predicate)// is det.

pl_predicate(Mod:Pred/Arity) -->
  atom(Mod), ":", atom(Pred), "/", integer(Arity).



%! pl_stream_position(+Position:compound)// is det.

pl_stream_position(stream(_,Row,Col,_)) -->
  "[Row: ", thousands(Row), ", Col: ", thousands(Col), "]".



%! pl_term(@Term)// is det.
%! pl_term(@Term, +Opts)// is det.

pl_term(Term) -->
  pl_term(Term, _{indent: 0}).


pl_term(Term, Opts) -->
  pl_term0(pl_term, Term, Opts).


pl_term0(Dcg_1, Dict, Opts) -->
  {is_dict(Dict)}, !,
  dcg_dict(Dcg_1, Dict, Opts.indent).
pl_term0(Dcg_1, L, Opts) -->
  {is_list(L)}, !,
  dcg_list(Dcg_1, L, Opts.indent).
pl_term0(_, Term, Opts) -->
  tab(Opts.indent),
  pl_simple_term0(Term).


pl_simple_term0(I) -->
  {integer(I)}, !,
  thousands(I).
pl_simple_term0(F) -->
  {float(F)}, !,
  float(F).
pl_simple_term0(S) -->
  {string(S)}, !,
  atom(S).
pl_simple_term0(A) -->
  {atom(A)}, !,
  atom(A).
pl_simple_term0(DT) -->
  pl_date_time(DT), !.
pl_simple_term0(Term) -->
  {term_to_atom(Term, A)},
  atom(A).
