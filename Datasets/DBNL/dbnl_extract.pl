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
    dbnl_publication_print//3, % ?Lang:atom
                               % ?Number:integer
                               % ?Changes:boolean
    dbnl_year//2, % ?Language:atom
                  % -Year:oneof([integer,pair(integer)])
    handwritten//2 % ?Language:atom
                   % ?Handwritten:boolean
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

:- use_module(dbnl(dbnl_db)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_print)).
:- use_module(dcg(dcg_year)).
:- use_module(generics(atom_ext)).



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

dbnl_publication_print(Lang, Number, Changes) -->
  publication_print(Lang, Number, Changes).
dbnl_publication_print(nl, _Number, _Changes) -->
  "(volksuitgave, 1ste vijfduizendtal)".
dbnl_publication_print(nl, _Number, _Changes) -->
  "(20ste-30ste duizendtal)".

handwritten(nl, true) --> "(handschrift)".



% YEARS %

dbnl_year(Graph, Title) -->
  dbnl_year0(_Lang, Year),
  {dbnl_assert_year(Graph, Title, Year)}.

dbnl_year0(Lang, Year) -->
  year(Lang, Year),
  (blanks, uncertainty(Lang) ; "").
% Hacked year interval.
dbnl_year0(Lang, Year1-Year2) -->
  year_interval(Lang, Year1-Year2),
  % This is an arbitrary disambiguation criterion for problem 1 (see header).
  (
    ""
  ;
    {atom_number(Atom, Year2)},
    {atom_length(Atom, 4)},
    uncertainty(Lang)
  ).
% Context-dependent indicator. Not resolved and asserted yet.
dbnl_year0(Lang, _Year) -->
  "z.j.",
  (blanks, uncertainty(Lang) ; "").
dbnl_year0(_Lang, _Year) -->
  question_mark.
dbnl_year0(_Lang, Year1-Year2) -->
  year(Lang, Year1), comma, blank,
  year(Lang, Year2),
  opening_square_bracket, integer(_), closing_square_bracket.
dbnl_year0(Lang, Year1-Year2) -->
  integer(Year1), blanks, conj(Lang), blanks, integer(Year2).
dbnl_year0(Lang, Year1-Year3) -->
  integer(Year1), hyphen_minus, integer(_Year2),
  blanks, conj(Lang), blanks,
  integer(Year3).
% @tbd Not yet parsed. What is the implication of status 'written' for the
%      print information?
dbnl_year0(nl, 1919) --> "ná aug. 1919 geschreven".
dbnl_year0(nl, 1928) --> "? [1928] (?)".
dbnl_year0(nl, 1626) --> "1626, herdr. 1638".

