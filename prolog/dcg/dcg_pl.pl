:- module(
  dcg_pl,
  [
    date_time//1,       % +DT
    dict//2,            % +Dict, +Opts
    predicate//1,       % +Predicate
    stream_position//1, % +Pos
    term//1,            % @Term
    term//2             % @Term, +Opts
  ]
).

/** <module> DCG Prolog term

DCG rules for printing terms.

@author Wouter Beek
@version 2015/08, 2015/10-2015/12, 2016/02, 2016/05
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



%! dict(+D, +Opts)// is det.

dict(D, Opts0) -->
  {dict_put_default(indent, Opts0, 0, Opts)},
  {dict_pairs(D, _, L)},
  {I = Opts.indent},
  (   % Empty dictionaries do not use lucious spacing.
      {is_empty_term(D)}
  ->  tab(I), "{}"
  ;   % Singleton dictionaries do not use lucious spacing if
      % the member is singleton.
      {is_singleton_term(D)}
  ->  tab(I),
      "{",
      {L = [Key-Val]},
      {EntryOpts = Opts.put(_{indent: 0})},
      dict_entry(Key-Val, EntryOpts),
      "}"
  ;   tab(I), "{", nl,
      {dict_inc(indent, Opts)},
      term_entries(dict_entry, L, Opts),
      tab(I), "}"
  ).



%! dict_entry(+Pair, +Opts)// is det.

dict_entry(Key-Val, Opts1) -->
  tab(Opts1.indent), term(Key), ": ",
  % Newline and additional indentation before dictionary or list values
  % that are non-empty and non-singleton.
  (   {(is_empty_term(Val) ; is_singleton_term(Val))}
  ->  {I = 0}
  ;   {I is Opts1.indent + 1}
  ),
  {Opts2 = Opts1.put(_{indent: I})},
  term(Val, Opts2).



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



list0(L, Opts) -->
  (   % The empty list does not use lusious spacing.
      {is_empty_term(L)}
  ->  tab(Opts.indent), list(L)
  ;   % A singleton list does not use lucious spacing
      % if its member is singleton.
      {is_singleton_term(L)}
  ->  tab(Opts.indent), list(L)
  ;   tab(Opts.indent), "[", nl,
      {dict_inc(indent, Opts)},
      term_entries(term, L, Opts),
      {dict_dec(indent, Opts)},
      tab(Opts.indent), "]"
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
%! term(@Term, +Opts)// is det.

term(T) -->
  term(T, _{indent: 0}).


term(D, Opts) --> {is_dict(D)}, !, dict(D, Opts).
term(L, Opts) --> {is_list(L)}, !, list0(L, Opts).
term(T, Opts) --> tab(Opts.indent), simple_term(T).



%! term_entries(:Goal_4, +Entries, +Opts)// is det.

term_entries(_, [], _) --> !, "".
term_entries(Goal_4, [H], Opts) --> !,
  dcg_call(Goal_4, H, Opts),
  nl.
term_entries(Goal_4, [H|T], Opts) -->
  dcg_call(Goal_4, H, Opts),
  ",", nl,
  term_entries(Goal_4, T, Opts).
