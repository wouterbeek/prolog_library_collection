:- module(
  dcg_generic,
  [
    conj//1, % ?Lang:atom
    dcg_atom_until//2, % +End:list(code)
                       % -Atom:atom
    dcg_all//0,
    dcg_all//1, % -L:list(code)
    dcg_debug//0,
    dcg_peek//1, % -X:code
    dcg_separated_list//2, % +Separator:dcg
                           % -List:list
    dcg_string_until//2, % +End:list(code)
                         % -Codes:list(code)
    disj//1, % ?Lang:atom
    language//1, % ?Lang:atom
    uncertainty//1 % ?Lang:atom
  ]
).

/** <module>

Generic DCG clauses.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dcg(dcg_ascii)).
:- reexport(library(dcg/basics)).
:- reexport(library(lists)).

:- meta_predicate(dcg_separated_list(//,-,+,-)).



dcg_atom_until(End, Atom) -->
  dcg_string_until(End, Codes),
  {atom_codes(Atom, Codes)}.

conj(en) --> "and".
conj(nl) --> "en".
conj(_Lang) --> comma.

dcg_all --> [].
dcg_all --> [_], dcg_all.

dcg_all([]) --> [].
dcg_all([H|T]) --> [H], dcg_all(T).

dcg_debug(L, []):-
  format(user_output, '~w\n', [L]).

dcg_peek(X), [X] -->
  [X].

dcg_separated_list(Separator, [H|T]) -->
  string(H),
  blank, Separator, blank,
  !,
  dcg_separated_list(Separator, T).
dcg_separated_list(_Separator, [H]) -->
  string(H).

disj(nl) --> "of".

language(nl) --> "Latijn".

dcg_string_until([H|End], [], [H|L], [H|L]):-
  append(End, _, L),
  !.
dcg_string_until(End, [H|T], [H|L], R):-
  dcg_string_until(End, T, L, R).

% Three dots uncertainty representation.
uncertainty(_Lang) --> "...".
% Question mark uncertainty representation.
uncertainty(Lang) -->
  opening_round_bracket,
  uncertainty(Lang),
  closing_round_bracket.
uncertainty(Lang) -->
  opening_square_bracket,
  uncertainty(Lang),
  closing_square_bracket.
uncertainty(_Lang) --> question_mark.
uncertainty(_Lang) --> "ca.".

