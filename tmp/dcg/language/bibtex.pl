:- module(
  bibtex,
  [
    bibtex/2 % +Input:compound
             % -Entries:list(compound)
  ]
).

/** <module> BibTeX grammar

@author Wouter Beek
@version 2015/05
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(pure_input)).

:- use_module(plc(dcg/dcg_abnf_common)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_meta)).

:- dynamic(user:prolog_file_type/2).
:- multifile(user:prolog_file_type/2).

user:prolog_file_type(bib, bibtex).





bibtex(atom(Atom), Entries):- !,
  atom_phrase(bibtex(Entries), Atom).
bibtex(file(File), Entries):- !,
  flag(tick, _, 0),
  phrase_from_file(bibtex(Entries), File),
  maplist(validate_entry, Entries).
bibtex(string(String), Entries):- !,
  string_phrase(bibtex(Entries), String).
bibtex(Input, _):-
  type_error(compound, Input).





% GRAMMAR %

bibtex([H|T]) --> skip, entry(H), !, {tick}, bibtex(T).
bibtex([]) --> skip.

tick:-
  flag(tick, X, X + 1),
  writeln(X),
  (   X =:= 18544
  ->  gtrace
  ;   true
  ).

entry(entry(Class,Name,Pairs)) -->
  class(Class), "{", skip, name(Name), skip, ",", skip,
  pairs(Pairs), skip,
  "}".

pairs([H|T]) -->
  pair(H),
  (   skip, ","
  ->  pairs(T)
  ;   {T = []}
  ).
pairs([]) --> "".

pair(Key-Value) -->
  skip, name(Key), skip, "=", skip, value(Value).

value(Value) -->
  "{", !,
  value(Value, 0).
value(Value) -->
  "\"", !,
  '...'(Codes),
  "\"",
  {atom_codes(Value, Codes)}.
value(Value) -->
  name(Value).

value(Value, I) -->
  dcg_atom(value0(I), Value).

value0(I, [H|T]) -->
  opening_curly_bracket(H), !,
  {NewI is I + 1},
  value0(NewI, T).
value0(0, []) --> "}", !.
value0(I, [H|T]) -->
  closing_curly_bracket(H), !,
  {NewI is I - 1},
  value0(NewI, T).
value0(I, [H|T]) -->
  [H], !,
  value0(I, T).
value0(_, []) --> "".

class(Class) -->
  "@",
  dcg_atom('[a-zA-Z]+', Class0),
  {validate(Class0, Class, _, _)}.

name(Name) --> dcg_atom(name0, Name).

name0([H|T]) --> char(H), name0(T), !.
name0([H]) --> char(H).

char(C) --> ascii_letter(C).
char(C) --> decimal_digit(C).
char(C) --> colon(C).
char(C) --> hyphen(C).
char(C) --> underscore(C).

%skip --> comment, !, skip.
skip --> white, !, skip.
skip --> "".

comment --> "%", '...', eol.

eol --> "\r\n".
eol --> "\n".

white --> " ".
white --> "\t".
white --> eol.





% VALIDATION %

validate_entry(entry(Class,_,Pairs)):-
  validate(_, Class, Required, _),
  forall(
    member(Key, Required),
    check_required(Class, Key, Pairs)
  ).

check_required(_, Key, Pairs):-
  memberchk(Key-_, Pairs), !.
check_required(Class, Key, _):-
  print_message(warning, missing_bibtex_key(Class,Key)).



validate(
  article,
  'Article',
  [author,journal,title,year],
  [month,note,number,pages,volume]
).
validate(
  book,
  'Book',
  [or(author,editor),publisher,title,year],
  [address,edition,month,note,number,series,volume]
).
validate(
  booklet,
  'Booklet',
  [title],
  [address,author,howpublished,month,note,year]
).
validate(
  conference,
  'Conference',
  [author,booktitle,title,year],
  [address,editor,month,note,number,organization,pages,publisher,series,volume]
).
validate(
  inbook,
  'InBook',
  [or(author,editor),or(chapter,pages),pusblisher,year],
  [address,edition,month,note,number,series,type,volume]
).
validate(
  incollection,
  'InCollection',
  [author,booktitle,publisher,title,year],
  [address,chapter,edition,editor,month,note,number,pages,series,type,volume]
).
validate(
  inproceedings,
  'InProceedings',
  [author,booktitle,title,year],
  [address,editor,month,note,number,organization,pages,publisher,series,volume]
).
validate(
  manual,
  'Manual',
  [title],
  [address,author,edition,month,note,organization,year]
).
validate(
  mastersthesis,
  'MastersThesis',
  [author,school,title,year],
  [address,month,note,type]
).
validate(
  misc,
  'Misc',
  [],
  [author,howpublished,month,note,title,year]
).
validate(
  phdthesis,
  'PhDThesis',
  [author,school,title,year],
  [address,keywords,month,note]
).
validate(
  proceedings,
  'Proceedings',
  [title,year],
  [address,editor,month,note,number,organization,publisher,series,volume]
).
validate(
  techreport,
  'TechReport',
  [author,institution,title,year],
  [address,month,note,number,type]
).
validate(
  unpublished,
  'Unpublished',
  [author,note,title],
  [month,year]
).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(missing_bibtex_key(Class,Key)) -->
  [
    'Key ',
    Key,
    ' is not allowed to be missing for BibTeX entry type ',
    Class,
    '.'
  ].
