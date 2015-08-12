:- module(
  dcg_pl_term,
  [
    pl_term//1, % @Term
    pl_term//2 % @Term
               % +Indent:nonneg
  ]
).

/** <module> DCG Prolog term

DCG rules for printing SWI-Prolog 7 terms.

@author Wouter Beek
@see http://www.swi-prolog.org/pldoc/man?section=dicts
@version 2015/08
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(pl/pl_term)).

:- meta_predicate(term_entries(4,+,+,?,?)).





%! pl_term(@Term)// is det.

pl_term(Term) -->
  pl_term(Term, 0).

%! pl_term(@Term, +Indent:nonneg)// is det.

% 1. Dictionary.
pl_term(Dict, I1) -->
  {is_dict(Dict), dict_pairs(Dict, _, Pairs)}, !,
  % The empty dictionary does not use lusious spacing.
  (   {Pairs == []}
  ->  indent(I1, bracketed(curly, dcg_void))
  ;   indent(I1, opening_curly_bracket),
      nl,
      {I2 is I1 + 1},
      term_entries(dict_entry, Pairs, I2),
      indent(I1, closing_curly_bracket)
  ).
% 2. List.
% The empty list does not use lusious spacing.
pl_term(List, I1) -->
  {is_list(List)}, !,
  (   {List == []}
  ->  indent(I1, bracketed(square, dcg_void))
  ;   indent(I1, opening_square_bracket),
      nl,
      {I2 is I1 + 1},
      term_entries(list_entry, List, I2),
      indent(I1, closing_square_bracket)
  ).
% 3. Other term.
pl_term(Term, I) -->
  indent(I, nondict(Term)).


%! term_entries(:Dcg, +Entries:list, +Indent:nonneg)// is det.

% Revert back to previous indentation level.
term_entries(_, [], _) --> !, [].
term_entries(Dcg, [H|T], I) -->
  dcg_call_cp(Dcg, H, I),
  % Whether to include the comma or not.
  (   {T \== []}
  ->  ","
  ;   ""
  ),
  nl,
  term_entries(Dcg, T, I).


%! dict_entry(+Pair:pair, +Indent:nonneg)// is det.

dict_entry(Key-Val, I1) -->
  indent(I1, nondict(Key)),
  ": ",
  % Newline and additional indentation
  % before non-empty dictionary or non-empty list value.
  ({    (   is_empty_dict(Val)
        ;   is_empty_list(Val)
        )}
    ->  {I2 = 0}
    ;   {I2 is I1 + 1},
        nl
  ),
  pl_term(Val, I2).


%! is_empty_dict(@Term) is semidet.

is_empty_dict(T):-
  is_dict(T),
  dict_pairs(T, _, L),
  L == [].


%! is_empty_list(@Term) is semidet.

is_empty_list(T):-
  is_list(T),
  T == [].


%! list_entry(@Term, +Indent:nonneg)// is det.

list_entry(Term, I) -->
  pl_term(Term, I).


%! nondict(@Term)// is det.

nondict(T) -->
  {with_output_to(codes(Cs), write_canonical_blobs(T))},
  Cs.
