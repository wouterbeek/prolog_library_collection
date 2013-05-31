:- module(
  dcg_ordinal,
  [
    ordinal//2 % ?Lang:atom
               % ?Ordinal:integer
  ]
).

/** <module> DCG_ORGINAL

DCGs for ordinal numbers.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).



%! ordinal(?Lang:atom, ?Ordinal:integer)//
% @tbd Exclude certain combinations: 1de, 2ste,

ordinal(Lang, Ordinal) -->
  integer(Ordinal),
  ordinal_noun(Lang).

ordinal_noun(de) --> dot.
ordinal_noun(nl) --> "e".
ordinal_noun(nl) --> "de".
ordinal_noun(nl) --> "ste".

