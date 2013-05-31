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
    print_number//3, % ?Language:atom
                     % ?Number:integer
                     % ?Changed:boolean
    title_year//2 % -Title:atom
                  % -Year:oneof([integer,pair(integer)])
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

dbnl_number_counter(Lang) -->
  number_counter(Lang).
% Probably a typo.
dbnl_number_counter(nl) -->
  atom(e).

% Can occur between spaces.
print_number(Lang, Print, Changes) -->
  opening_round_bracket,
  print_number(Lang, Print, Changes),
  closing_round_bracket.
print_number(Lang, Print, Changes) -->
  facsimile(Lang),
  integer(Print),
  dbnl_number_counter(Lang),
  skip,
  print_changes(Lang, Changes).
% Information for different volumes are given.
% @tbd This information is now skipped.
print_number(Lang, Print, Changes) -->
  print_number_volumes(Lang, Print, Changes).
% No number-of-print, only a conjunction of adjectives.
print_number(Lang, fail, Changes) -->
  print_changes(Lang, Changes).
% No number-of-print informat occurs.
print_number(_Lang, fail, fail) -->
  [].

print_number_volume(Lang, Print, Changes) -->
  part_word(Lang),
  blank,
  integer(_Volume),
  colon,
  blank,
  print_number(Lang, Print, Changes).

%! print_number_volumes(Lang, Print, Changes)//
% @tbd The print number and change boolean are only returned if
%      they are the same for all volumes.

print_number_volumes(Lang, Print, Changes) -->
  print_number_volume(Lang, Print, Changes).
print_number_volumes(Lang, Print, Changes) -->
  print_number_volume(Lang, Print1, Changes1),
  blank,
  forward_slash,
  blank,
  print_number_volume(Lang, Print2, Changes2),
  {if_then_else(
    Print1 == Print2,
    Print = Print1,
    Print = _
  )},
  {if_then_else(
    Changes1 == Changes2,
    Changes = Changes1,
    Changes = _
  )}.

% Indicators that a print contains changes w.r.t. a previous print.
print_changes(Lang, Boolean) -->
  print_changes_conj(Lang, Boolean),
  blank,
  print_word(Lang),
  uncertainty(Lang).
print_changes(Lang, Boolean) -->
  print_changes_conj(Lang, Boolean),
  blank,
  print_word(Lang),
  blank,
  commercial_edition(Lang).
print_changes(Lang, Boolean) -->
  print_changes_conj(Lang, Boolean),
  blank,
  print_word(Lang),
  blank,
  atom(van),
  blank,
  print_word(Lang),
  blank,
  year(Lang, _Year).

print_changes_adj(Lang, Boolean) -->
  print_changes_adj0(Lang, Boolean).
% Modifier on adjectives.
print_changes_adj(Lang, Boolean) -->
  print_changes_modifier(Lang),
  blank,
  print_changes_adj0(Lang, Boolean).
% Language preposition.
print_changes_adj(Lang, Boolean) -->
  {Lang = nl},
  atom('naar het'),
  blank,
  language(Lang),
  blank,
  print_changes_adj0(Lang, Boolean).

print_changes_adj0(nl, true) -->
  atom(bewerkte).
print_changes_adj0(nl, true) -->
  atom(bijgewerkte).
% @tbd This could be relevant to assert as well.
print_changes_adj0(nl, fail) -->
  atom(fotomechanische).
% @tbd This is a relevant aspect that should be asserted!
print_changes_adj0(nl, fail) -->
  atom(goedkoope).
% Abbreviation of 'gewijzigde'.
print_changes_adj0(nl, true) -->
  atom('gew.').
print_changes_adj0(nl, true) -->
  atom(gewijzigde).
% Abbreviation of 'herziene'.
print_changes_adj0(nl, true) -->
  atom('herz.').
print_changes_adj0(nl, true) -->
  atom(herziene).
% Meaningless adjective.
print_changes_adj0(nl, fail) -->
  atom(nieuwe).
% Abbreviation for 'omgewerkte'?
print_changes_adj0(nl, true) -->
  atom('omgew.').
print_changes_adj0(nl, true) -->
  atom(omgewerkte).
print_changes_adj0(nl, fail) -->
  atom(onveranderde).
% Strange word.
print_changes_adj0(nl, fail) -->
  atom(veelveranderde).
% Abbreviated version of 'verbeterde'.
print_changes_adj0(nl, true) -->
  atom('verb.').
print_changes_adj0(nl, true) -->
  atom(verbeterde).
% Abbreviation for 'vermeerderde'.
print_changes_adj0(nl, fail) -->
  atom('verm.').
print_changes_adj0(nl, fail) -->
  atom(vermeerderde).
% Probably a typo.
print_changes_adj0(nl, fail) -->
  atom(vermeerde).
% Meaningless adjective.
print_changes_adj0(nl, fail) -->
  atom(volledige).

print_changes_conj(_Lang, _Boolean) -->
  [].
print_changes_conj(Lang, Boolean) -->
  print_changes_adj(Lang, Boolean).
print_changes_conj(Lang, Boolean) -->
  print_changes_adj(Lang, Boolean1),
  conjunct(Lang),
  print_changes_conj(Lang, Boolean2),
  {boolean_or(Boolean1, Boolean2, Boolean)}.

print_changes_modifier(nl) -->
  atom(geheel).
print_changes_modifier(nl) -->
  atom(ingrijpend).

print_word(de) -->
  atom('Auflage').
print_word(nl) -->
  atom(druk).
print_word(nl) -->
  atom(herdruk).
print_word(nl) -->
  atom(uitgave).

boolean_or(fail, fail, fail):-
  !.
boolean_or(true, _, true):-
  !.
boolean_or(_, true, true).

skip -->
  [].
skip -->
  comma,
  skip.
skip -->
  blank,
  skip.

title_year([]) -->
  [].
title_year([H | T]) -->
  any(H),
  title_year(T).

title_year(Title, Year) -->
  title_year(L),
  dbnl_year(Year),
  {atom_codes(Title, L)}.



% YEARS %

% Yet another uncertainty modifier with unknown semantics.
dbnl_year(Lang, Year) -->
  dbnl_year0(Lang, Year),
  blank,
  uncertainty(Lang).
% Uncertainty sometimes preceeds temporal term.
dbnl_year(Lang, Year) -->
  uncertainty(Lang),
  blank,
  dbnl_year0(Lang, Year).

% Skip blanks.
dbnl_year0(Lang, Year) -->
  blanks,
  dbnl_year0(Lang, Year).
% Skip uncertainty modifiers.
dbnl_year0(Lang, Year) -->
  uncertainty(Lang),
  dbnl_year0(Lang, Year).
% Between round brackets.
dbnl_year0(Lang, Year) -->
  opening_round_bracket,
  dbnl_year0(Lang, Year),
  closing_round_bracket.
dbnl_year0(Lang, Year1-Year2) -->
  dcg_year:year_interval(Lang, Year1-Year2),
  % This is an arbitrary disambiguation criterion for problem 1 (see header).
  (
    ""
  ;
    {atom_number(Atom, Year2)},
    {atom_length(Atom, 4)},
    uncertainty(Lang)
  ).
dbnl_year0(Lang, Year) -->
  dcg_year:dbnl_year_point(Lang, Year).

% A single year.
dbnl_year_point(Lang, Year) -->
  year(Year),
  blank,
  uncertainty(Lang).
% Context-dependent indicator. Not processed yet.
dbnl_year_point(Lang, _Year) -->
  atom('z.j.'),
  skip,
  uncertainty(Lang).

