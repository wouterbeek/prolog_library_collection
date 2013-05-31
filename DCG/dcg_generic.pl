:- module(
  dcg_generic,
  [
    conj//1, % ?Lang:atom
    disj//1, % ?Lang:atom
    language//1, % ?Lang:atom
    string_until//2, % +End:list(code)
                     % -Codes:list(code)
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



conj(en) --> "and".
conj(nl) --> "en".
conj(_Lang) --> comma.

disj(nl) --> "of".

language(nl) --> "Latijn".

string_until([H|End], [], [H|L], [H|L]):-
  append(End, _, L),
  !.
string_until(End, [H|T], [H|L], R):-
  string_until(End, T, L, R).

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

