:- module(
  dcg_atom,
  [
    atom_capitalize//0,
    atom_ci//1, % ?Atom:atom
    atom_ellipsis//2, % +Atom:atom
                      % +Ellipsis:positive_integer
    atom_lower//1, % ?Atom:atom
    atom_title//1, % ?Atom:atom
    atom_upper//1 % ?Atom:atom
  ]
).

/** <module> DCG atom

Grammar rules for processing atoms.

@author Wouter Beek
@version 2014-2015
*/

:- use_module(library(dcg/basics)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_code)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_meta)).
:- use_module(plc(dcg/dcg_unicode),
  [letter_lowercase//1,letter_uppercase//1]
).
:- use_module(plc(generics/atom_ext)).





%! atom_capitalize// .

atom_capitalize, [Upper] -->
  [Lower],
  {code_type(Upper, to_upper(Lower))}, !,
  dcg_cp.
atom_capitalize --> "".



%! atom_ci(?Atom:atom)// .

atom_ci(Atom) -->
  dcg_atom('*'(code_ci, []), Atom).



%! atom_ellipsis(+Atom:atom, +Ellipsis:positive_integer)// .

atom_ellipsis(Atom1, Ellipsis) -->
  {atom_truncate(Atom1, Ellipsis, Atom2)},
  atom(Atom2).



%! atom_lower(?Atom:atom)// .

atom_lower(Atom) -->
  dcg_atom('*'(code_lower, []), Atom).



%! atom_title(?Atom:atom) // .

atom_title(Atom) -->
  {var(Atom)}, !,
  letter_uppercase(H),
  '*'(letter_lowercase, T, []),
  {atom_codes(Atom, [H|T])}.
atom_title('') --> "".
atom_title(Atom) -->
  {atom_codes(Atom, [H|T])},
  letter_uppercase(H),
  '*'(letter_lowercase, T, []).



%! atom_upper(?Atom:atom)// .

atom_upper(Atom) -->
  dcg_atom('*'(code_lower, []), Atom).
