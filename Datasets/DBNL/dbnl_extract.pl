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
    dbnl_extract_year/2, % +Atom:atom
                         % -Year:oneof([integer,pair(integer)])
    dbnl_extract_year_end/3 % +Atom1:atom
                            % -Year:oneof([integer,pair(integer)])
                            % -Atom2:atom
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
dbnl_extract_editor(Atom, EditorName):-
  atom_concat('hoofdredactie ', EditorName, Atom).

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

% Skip years that are 'z.j.'.
% @tbd What does this mean?
dbnl_extract_year_end(Atom1, _Year, Atom2):-
  sub_atom(Atom1, _Before, 4, 1, '(z.j.)'),
  !,
  sub_atom(Atom1, 0, _Length1, 7, Atom2).
% Year.
dbnl_extract_year_end(Atom1, Year, Atom2):-
  sub_atom(Atom1, _Before1, 1, 0, ')'),
  sub_atom(Atom1, _Before2, 4, 1, YearAtom),
  sub_atom(Atom1, _Before3, 1, 5, '('),
  !,
  atom_number(YearAtom, Year),
  sub_atom(Atom1, 0, _Length, 7, Atom2).
% Year interval.
dbnl_extract_year_end(Atom1, Year1-Year2, Atom2):-
  sub_atom(Atom1, _Before1, 1, 0, ')'),
  sub_atom(Atom1, _Before2, 4, 1, YearAtom1),
  sub_atom(Atom1, _Before3, 1, 5, '-'),
  sub_atom(Atom1, _Before4, 4, 6, YearAtom2),
  !,
  atom_number(YearAtom1, Year1),
  atom_number(YearAtom2, Year2),
  sub_atom(Atom1, 0, _Length, 12, Atom2).
% No year detected.
dbnl_extract_year_end(Atom, _Year, Atom).

