:- module(
  dcg_pl,
  [
    date_time//1,       % +DT
    dict//2,            % +Dict, +Indent
    predicate//1,       % +Predicate
    stream_position//1, % +Pos
    term//1,            % @Term
    term//2             % @Term, +Indent
  ]
).

/** <module> DCG Prolog term

DCG rules for printing terms.

@author Wouter Beek
@version 2015/08, 2015/10-2015/12, 2016/02
*/

:- use_module(library(date_time/date_time)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(list_ext)).
:- use_module(library(typecheck)).

:- meta_predicate
    term_entries(4, +, +, ?, ?).





%! date_time(+DT)// is det.

date_time(DT, X, Y):-
  date_time_to_date(DT, D),
  format_time(codes(X,Y), "%FT%T%z", D).



dict(D, I1) -->
  {dict_pairs(D, _, L)},
  % An empty dictionary does not use lucious spacing.
  (   {is_empty_term(D)}
  ->  tab(I1), "{}"
  ;   % A singleton dictionary does not use lucious spacing if
      % its member is singleton.
      {is_singleton_term(D)}
  ->  tab(I1), "{", {L = [Key-Val]}, dict_entry(Key-Val, 1), tab(1), "}"
  ;   tab(I1), "{", nl,
      {I2 is I1 + 1},
      term_entries(dict_entry, L, I2),
      tab(I1), "}"
  ).



%! dict_entry(+Pair, +Indent)// is det.

dict_entry(Key-Val, I1) -->
  tab(I1),
  term(Key),
  ": ",
  % Newline and additional indentation before dictionary or list values
  % that are non-empty and non-singleton.
  (   {(is_empty_term(Val) ; is_singleton_term(Val))}
  ->  {I2 = 0}
  ;   {succ(I1, I2)}, nl
  ),
  term(Val, I2).



%! is_empty_term(@Term) is semidet.

is_empty_term(D):- is_dict(D), !, is_empty_dict(D).
is_empty_term(L):- is_list(L), !, empty_list(L).



%! is_singleton_term(@Term) is semidet.

is_singleton_term(D):-
  is_dict(D), !,
  dict_pairs(D, _, L),
  L = [_-Val],
  is_singleton_term(Val).
is_singleton_term(L):-
  is_list(L), !,
  L = [X],
  is_singleton_term(X).
is_singleton_term(_).



list0(L, I1) -->
  (   % The empty list does not use lusious spacing.
      {is_empty_term(L)}
  ->  tab(I1), list(L)
  ;   % A singleton list does not use lucious spacing
      % if its member is singleton.
      {is_singleton_term(L)}
  ->  tab(I1), list(L)
  ;   tab(I1), "[", nl,
      {I2 is I1 + 1},
      term_entries(term, L, I2),
      tab(I1), "]"
  ).



%! predicate(+Predicate)// is det.

predicate(Mod:Pred/Arity) -->
  atom(Mod), ":", atom(Pred), "/", integer(Arity).



%! simple_term(@Term)// is det.

simple_term(I)    --> {integer(I)},     !, thousands(I).
simple_term(F)    --> {float(F)},       !, float(F).
simple_term(S)    --> {string(S)},      !, "\"", atom(S), "\"".
simple_term(A)    --> {atom(A)},        !, atom(A).
simple_term(DT)   -->                      date_time(DT), !.
simple_term(Comp) --> {compound(Comp)}, !, {term_to_atom(Comp, A)}, atom(A).
simple_term(X)    --> {gtrace},            simple_term(X).



%! stream_position(+Position:compound)// is det.

stream_position(stream(_,Row,Col,_)) -->
  "[Row: ", thousands(Row), ", Col: ", thousands(Col), "]".



%! term(@Term)// is det.
%! term(@Term, +Indent)// is det.

term(T) --> term(T, 0).


term(D, I) --> {is_dict(D)}, !, dict(D, I).
term(L, I) --> {is_list(L)}, !, list0(L, I).
term(T, I) --> tab(I), simple_term(T).



%! term_entries(:Goal_4, +Entries, +Indent)// is det.

term_entries(_, [], _) --> !, "".
term_entries(Goal_4, [H], I) --> !,
  dcg_call(Goal_4, H, I),
  nl.
term_entries(Goal_4, [H|T], I) -->
  dcg_call(Goal_4, H, I),
  ",", nl,
  term_entries(Goal_4, T, I).
