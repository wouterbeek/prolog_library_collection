:- module(
  dbnl_extract,
  [
    dbnl_extract_author/2, % +Atom:atom
                           % -Author:atom
    dbnl_extract_copyright/3, % +Atom:atom
                              % -Organization:atom
                              % -Year:integer
    dbnl_extract_editor/2, % +Atom:atom
                           % -Editor:atom
    dbnl_extract_page/2, % +Atom:atom
                         % -Page:integer
    dbnl_extract_year/2 % +Atom:atom
                        % -Year:oneof([integer,pair(integer)])
  ]
).

/** <module> DBNL EXTRACT

Predicates for extracting information from atoms in DBNL.

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(atom_ext)).
:- use_module(library(apply)).



dbnl_extract_author(Author, Author).

dbnl_extract_copyright(Atom, Organization, Year):-
  split_atom_exclusive(' ', Atom, ['©', Year1, Organization]),
  atom_number(Year1, Year).

dbnl_extract_editor(Atom, EditorName):-
  atom_concat('editie ', EditorName, Atom).

dbnl_extract_page(Atom1, Page):-
  atom_concat('[p. ', Atom2, Atom1),
  atom_concat(Atom3, ']', Atom2),
  atom_number(Atom3, Page).

%! dbnl_extract_year(+Atom:atom, -Year:oneof(integer,pair(integer))) is det.

dbnl_extract_year(Atom, StartYear-EndYear):-
  split_atom_exclusive('-', Atom, [StartYearAtom, EndYearAtom]),
  maplist(dbnl_extract_year, [StartYearAtom, EndYearAtom], [StartYear, EndYear]).
dbnl_extract_year(Atom, Year):-
  sub_atom(Atom, _Before, 4, _After, Temp),
  atom_number(Temp, Year).
dbnl_extract_year(Atom, Year):-
  sub_atom(Atom, 0, 4, _After, Temp),
  atom_number(Temp, Year).

