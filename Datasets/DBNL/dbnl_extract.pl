:- module(
  dbnl_extract,
  [
    extract_author/2, % +Atom:atom
                      % -Author:atom
    extract_editor/2, % +Atom:atom
                      % -Editor:atom
    extract_page/2, % +Atom:atom
                    % -Page:integer
    extract_year/2 % +Atom:atom
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



extract_author(Author, Author).

extract_editor(Atom, EditorName):-
  atom_concat('editie ', EditorName, Atom).

extract_page(Atom1, Page):-
  atom_concat('[p. ', Atom2, Atom1),
  atom_concat(Atom3, ']', Atom2),
  atom_number(Atom3, Page).

%! extract_year(+Atom:atom, -Year:oneof(integer,pair(integer))) is det.

extract_year(Atom, StartYear-EndYear):-
  split_atom_exclusive('-', Atom, [StartYearAtom, EndYearAtom]),
  maplist(extract_year, [StartYearAtom, EndYearAtom], [StartYear, EndYear]).
extract_year(Atom, Year):-
  sub_atom(Atom, _Before, 4, _After, Temp),
  atom_number(Temp, Year).
extract_year(Atom, Year):-
  sub_atom(Atom, 0, 4, _After, Temp),
  atom_number(Temp, Year).

