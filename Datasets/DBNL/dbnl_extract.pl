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

% DCG
    dbnl_year//2, % ?Language:atom
                  % -Year:oneof([integer,pair(integer)])
    handwritten//2, % ?Language:atom
                    % ?Handwritten:boolean
    print_number//3 % ?Language:atom
                    % ?Number:integer
                    % ?Changed:boolean
  ]
).

/** <module> DBNL EXTRACT

Predicates for extracting information from atoms in DBNL.

---+ Parsing problems

---++ Problem 1

The use of the question mark for expressing the uncertainty of publication
years cannot be disambiguated. The following two examples show this.

==
Mentor, Raadgever voor de Nederlandsche jeugd., 1867-1873?
Volkszangdag, 1922-19??
==

In the former the question mark should be interpreted as expressing
uncertainty of an *expressed* digit (the 3 in this case). What is meant
is the interval 1870-1879 (not 1870-18799).

In the latter the question mark should be interpreted as expressing
uncertainty of an *unexpressed* digit. What is means is the interval
1922-1999.

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(meta_ext)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_year)).



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



% DCG %

commercial_edition(Lang) -->
  opening_round_bracket,
  commercial_edition(Lang),
  closing_round_bracket.
commercial_edition(nl) -->
  atom(handelseditie).

facsimile(_Lang) -->
  [].
facsimile(nl) -->
  atom(facsimile),
  blank.

handwritten(nl, true) -->
  atom('(handschrift)').
handwritten(_Lang, fail) -->
  [].

skip -->
  [].
skip -->
  comma,
  skip.
skip -->
  blank,
  skip.



% YEARS %

% Between round brackets.
dbnl_year(Lang, Year) -->
  opening_round_bracket,
  dbnl_year(Lang, Year),
  closing_round_bracket.
% Hacked year interval.
dbnl_year(Lang, Year1-Year2) -->
  year_interval(Lang, Year1-Year2),
  % This is an arbitrary disambiguation criterion for problem 1 (see header).
  (
    ""
  ;
    {atom_number(Atom, Year2)},
    {atom_length(Atom, 4)},
    uncertainty(Lang)
  ).
% Plain year point.
dbnl_year(Lang, Year) -->
  year_point(Lang, Year).
% Context-dependent indicator. Not resolved and asserted yet.
dbnl_year(Lang, _Year) -->
  "z.j.",
  (blanks, uncertainty(Lang) ; "").

