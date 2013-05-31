:- module(
  dcg_generic,
  [
    conj//1, % ?Lang:atom
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



conj(en) --> "and".
conj(nl) --> "en".
conj(_Lang) --> comma.

language(nl) --> "Latijn".

% Three dots uncertainty representation.
uncertainty(_Lang) -->
  "...".
% Question mark uncertainty representation.
uncertainty(Lang) -->
  opening_round_bracket,
  uncertainty(Lang),
  closing_round_bracket.
uncertainty(Lang) -->
  opening_square_bracket,
  uncertainty(Lang),
  closing_square_bracket.
uncertainty(_Lang) -->
  question_mark.
uncertainty(nl) -->
  "ca.".

