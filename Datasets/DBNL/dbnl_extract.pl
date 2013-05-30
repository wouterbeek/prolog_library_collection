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
    handwritten//1, % -Handwritten:boolean
    decimal_digit//1, % -Digit:code
    decimal_integer//1, % -Integer:integer
    print_number//1, % -Number:integer
    year//1 % -PointOrInterval:oneof([integer,pair(integer)])
  ]
).

/** <module> DBNL EXTRACT

Predicates for extracting information from atoms in DBNL.

@author Wouter Beek
@version 2013/05
*/

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

%! decimal_digit(-Digit:code)//
%
% @author Jan Wielemaker

decimal_digit(D) -->
  [D],
  {code_type(D, digit)}.

decimal_digits([D|T]) -->
  decimal_digit(D),
  !,
  decimal_digits(T).
decimal_digits([]) -->
  [].

%! decimal_integer(-Number:integer)//
%
% @author Jan Wielemaker

decimal_integer(I) -->
  decimal_digit(D0),
  decimal_digits(D),
  {number_codes(I, [D0|D])}.

handwritten(true) --> ['(handschrift)'].
handwritten(fail) --> [].

print_number(Print) -->
  ['('],
  print_number(Print),
  [')'].
print_number(Print) -->
  decimal_integer(Print).

% Skip spaces.
year(PointOrInterval) -->
  [' '],
  year(PointOrInterval).
% Do not consider uncertainty modifiers.
year(PointOrInterval) -->
  ['ca.'],
  year(PointOrInterval).
% Between round brackets.
year(PointOrInterval) -->
  ['('],
  year(PointOrInterval),
  [')'].
year(PointOrInterval) -->
  year_point_or_interval(PointOrInterval).

% A pair of years.
year_interval(Point1-Point2) -->
  year_point(Point1),
  ['-'],
  year_point(Point2).

% A single year.
year_point(Year) -->
  {between(1, 4, Length)},
  {length(Digits, Length)},
  decimal_digits(Digits),
  {number_codes(Year, Digits)}.
% Context-dependent indicator. Not processed yet.
year_point(_) --> ['z.j.'].

year_point_or_interval(Point) -->
  year_point(Point).
year_point_or_interval(Interval) -->
  year_interval(Interval).
