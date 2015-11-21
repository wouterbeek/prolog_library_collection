:- module(
  dcg_pl,
  [
    pl_dateTime//1, % +DateTime:compound
    pl_pair//1, % +Pair:pair
    pl_pair//2, % :Name_2
                % :Value_2
    pl_predicate//1, % +Predicate:compound
    pl_stream_position//1, % +Stream:compound
    pl_term//1, % @Term
    pl_term//2 % @Term
               % +Indent:nonneg
  ]
).

/** <module> DCG Prolog term

DCG rules for printing SWI-Prolog 7 terms.

@author Wouter Beek
@version 2015/08, 2015/10-2015/11
*/

:- use_module(library(date_ext)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_cardinal)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(dict_ext)).
:- use_module(library(list_ext)).
:- use_module(library(pl/pl_term)).
:- use_module(library(typecheck)).

:- meta_predicate(pl_pair(//,//,?,?)).
:- meta_predicate(term_entries(4,+,+,?,?)).





%! pl_dateTime(+DateTime:compound)// is det.

pl_dateTime(DT1, X, Y):-
  dateTime_date(DT1, DT2),
  format_time(codes(X,Y), "%FT%T%z", DT2).



%! pl_pair(+Pair:pair)// is det.

pl_pair(N-V) -->
  pl_pair(pl_term(N), pl_term(V)).


%! pl_pair(:Name_2, :Value_2)// is det.

pl_pair(N_2, V_2) -->
  N_2,
  "-",
  V_2.



%! pl_predicate(+Predicate:compound)// is det.

pl_predicate(Mod:PredLet/Arity) -->
  atom(Mod),
  ":",
  atom(PredLet),
  "/",
  integer(Arity).



%! pl_stream_position(+Position:compound)// is det.

pl_stream_position(stream(_,Row,Col,_)) -->
  "[",
  row(Row),
  ", ",
  column(Col),
  "]".

row(N) -->
  "Row: ",
  thousands_integer(N).

column(N) -->
  "Col: ",
  thousands_integer(N).



%! pl_term(@Term)// is det.
% Wrapper around pl_term//2 with no indentation.

pl_term(Term) -->
  pl_term(Term, 0).


%! pl_term(@Term, +Indent:nonneg)// is det.

% 1. Dictionary.
pl_term(D, I1) -->
  {is_dict(D)}, !,
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
% 2. List.
pl_term(L, I1) -->
  {is_list(L)}, !,
  (   % The empty list does not use lusious spacing.
      {is_empty_term(L)}
  ->  tab(I1), list(L)
  ;   % A singleton list does not use lucious spacing
      % if its member is singleton.
      {is_singleton_term(L)}
  ->  tab(I1), list(L)
  ;   tab(I1), "[", nl,
      {I2 is I1 + 1},
      term_entries(pl_term, L, I2),
      tab(I1), "]"
  ).
% 3. Other term.
pl_term(T, I) -->
  tab(I), pl_term0(T).

pl_term0(I)   --> {integer(I)}, !, thousands_integer(I).
pl_term0(S)   --> {string(S)}, !, "\"", atom(S), "\"".
pl_term0(A)   --> {is_iri(A)}, !, iri(A).
pl_term0(A)   --> {atom(A)}, !, atom(A).
pl_term0(N-V) --> !, pl_pair(N-V).
pl_term0(DT)  --> pl_dateTime(DT), !.
pl_term0(X)   --> {gtrace}, pl_term0(X).



%! term_entries(:Dcg_4, +Entries:list, +Indent:nonneg)// is det.

term_entries(_, [], _) --> !, "".
term_entries(Dcg_4, [H], I) --> !,
  dcg_call(Dcg_4, H, I),
  nl.
term_entries(Dcg_4, [H|T], I) -->
  dcg_call(Dcg_4, H, I),
  ",", nl,
  term_entries(Dcg_4, T, I).



%! dict_entry(+Pair:pair, +Indent:nonneg)// is det.

dict_entry(Key-Val, I1) -->
  tab(I1), pl_term(Key),
  ": ",
  % Newline and additional indentation before dictionary or list values
  % that are non-empty and non-singleton.
  (   {(is_empty_term(Val) ; is_singleton_term(Val))}
  ->  {I2 = 0}
  ;   {succ(I1, I2)}, nl
  ),
  pl_term(Val, I2).



%! is_empty_term(@Term) is semidet.

is_empty_term(D):-
  is_dict(D), !,
  is_empty_dict(D).
is_empty_term(L):-
  is_list(L), !,
  empty_list(L).



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
