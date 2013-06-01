:- module(
  dcg_generic,
  [
    conj//1, % ?Lang:atom
    dcg_atom_all//1, % -Atom:atom
    dcg_atom_until//2, % :End:dcg
                       % -Atom:atom
    dcg_debug//0,
    dcg_peek//1, % -X:code
    dcg_phrase/2, % :DCGBody:dcg
                  % ?In:list(code)
    dcg_phrase/3, % :DCGBody:dcg
                  % ?In:list(code)
                  % ?Out:list(code)
    dcg_separated_list//2, % :Separator:dcg
                           % -Codess:list(list(codes))
    dcg_string_all//1, % -Codes:list(code)
    dcg_string_until//2, % :End:dcg
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

:- meta_predicate(dcg_atom_until(//,-,+,-)).
:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,?,?)).
:- meta_predicate(dcg_separated_list(//,-,+,-)).
:- meta_predicate(dcg_string_until(//,-,+,-)).



dcg_atom_until(End, Atom) -->
  dcg_string_until(End, Codes),
  {atom_codes(Atom, Codes)}.

dcg_atom_all(Atom) -->
  dcg_string_all(Codes),
  {atom_codes(Atom, Codes)}.

conj(en) --> "and".
conj(nl) --> "en".
conj(_Lang) --> comma.

dcg_debug(L, []):-
  format(user_output, '~w\n', [L]).

dcg_peek(X), [X] -->
  [X].

dcg_phrase(DCGBody, In):-
  dcg_phrase(DCGBody, In, []).

dcg_phrase(DCGBody, In, Out):-
  is_list(In),
  !,
  phrase(DCGBody, In, Out).
dcg_phrase(DCGBody, In1, Out):-
  atomic(In1),
  !,
  atom_codes(In1, In2),
  dcg_phrase(DCGBody, In2, Out).

dcg_separated_list(Separator, [H|T]) -->
  string(H),
  % Allow symmetric spaces.
  (Separator ; blank, Separator, blank),
  !,
  dcg_separated_list(Separator, T).
dcg_separated_list(_Separator, [H]) -->
  string(H).

dcg_string_all([]) --> [].
dcg_string_all([H|T]) --> [H], dcg_string_all(T).

dcg_string_until(End, []), End -->
  End,
  !.
dcg_string_until(End, [H|T]) -->
  [H],
  dcg_string_until(End, T).

disj(nl) --> "of".

language(nl) --> "Latijn".

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

