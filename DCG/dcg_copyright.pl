:- module(
  dcg_copyright,
  [
    copyright//2 % -Holders:list(atom)
                 % -Year:oneof([integer,pair(integer)])
  ]
).

/** <module> DCG_COPYRIGHT

DCGs for parsing copyright information.

@author Wouter Beek
@version 2013/06
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_year)).
:- use_module(library(dcg/basics)).



copyright(Holders, Year) -->
  (copyright ; ""), blank,
  year(Year), blank,
  holders(Holders),

holders([H]) --> last_holder(H).
holders([H|T]) --> middle_holder(H), holders(T).

middle_holder(H) --> dcg_atom_until(" /", H), blank, forward_slash, blank.
middle_holder(H) --> dcg_atom_until(" &", H), blank, ampersand, blank.

last_holder(H) --> dcg_atom_all(H).

