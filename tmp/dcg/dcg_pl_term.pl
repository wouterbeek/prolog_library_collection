:- module(
  dcg_pl_term,
  [
    dcg_pl_term//1, % @Term
    dcg_pl_term//2 % @Term
                   % +Indent:nonneg
  ]
).

/** <module> DCG: Prolog term

DCG rules for printing SWI-Prolog 7 terms.

@author Wouter Beek
@see http://www.swi-prolog.org/pldoc/man?section=dicts
@version 2014/10-2014/11
*/

:- use_module(library(dcg/basics)).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_bracket)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_meta)).

:- meta_predicate(dcg_entries(4,+,+,?,?)).





%! dcg_pl_term(@Term)// is det.

dcg_pl_term(Term) -->
  dcg_pl_term(Term, 0).

%! dcg_pl_term(@Term, +Indent:nonneg)// is det.

% 1. Dictionary.
dcg_pl_term(Dict, I1) -->
  {is_dict(Dict), dict_pairs(Dict, _, Pairs)}, !,
  % The empty dictionary does not use lusious spacing.
  (   {Pairs == []}
  ->  indent(I1, bracketed(curly, dcg_void))
  ;   indent(I1, opening_curly_bracket),
      nl,
      {I2 is I1 + 1},
      dcg_entries(dcg_dict_entry, Pairs, I2),
      indent(I1, closing_curly_bracket)
  ).
% 2. List.
% The empty list does not use lusious spacing.
dcg_pl_term(List, I1) -->
  {is_list(List)}, !,
  (   {List == []}
  ->  indent(I1, bracketed(square, dcg_void))
  ;   indent(I1, opening_square_bracket),
      nl,
      {I2 is I1 + 1},
      dcg_entries(dcg_list_entry, List, I2),
      indent(I1, closing_square_bracket)
  ).
% 3. Other term.
dcg_pl_term(Term, I) -->
  indent(I, write_nondict(Term)).


%! dcg_entries(:Dcg, +Entries:list, +Indent:nonneg)// is det.

% Revert back to previous indentation level.
dcg_entries(_, [], _) --> !, [].
dcg_entries(Dcg, [H|T], I) -->
  dcg_call_cp(Dcg, H, I),

  % Whether to include the comma or not.
  (   {T \== []}
  ->  ","
  ;   ""
  ),
  nl,

  dcg_entries(Dcg, T, I).


%! dcg_dict_entry(+KeyValue:pair, +Indent:nonneg)// is det.

dcg_dict_entry(Key-Value, I1) -->
  indent(I1, write_nondict(Key)),
  ": ",
  % Newline and additional indentation
  % before non-empty dictionary or non-empty list value.
  ({    (   is_nonempty_dict(Value)
        ;   is_nonempty_list(Value)
        )}
    ->  {I2 is I1 + 1},
        nl
    ;   {I2 = 0}
  ),
  dcg_pl_term(Value, I2).


%! dcg_list_entry(@Term, +Indent:nonneg)// is det.

dcg_list_entry(Term, I) -->
  dcg_pl_term(Term, I).


%! is_empty_dict(@Term) is semidet.

is_empty_dict(Term):-
  is_dict(Term),
  dict_pairs(Term, _, Pairs),
  Pairs == [].


%! is_nonempty_dict(@Term) is semidet.

is_nonempty_dict(Term):-
  is_dict(Term),
  dict_pairs(Term, _, Pairs),
  Pairs \== [].


%! is_nonempty_list(@Term) is semidet.

is_nonempty_list(Term):-
  is_list(Term),
  Term \== [].


%! write_nondict(@Term)// is det.

write_nondict(Term) -->
  {with_output_to(atom(Atom), writeq(Term))},
  atom(Atom).
