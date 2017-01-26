:- module(
  bibtex,
  [
    bibtex_load/2, % +Source, -Entries
    bibtex_load/3  % +Source, -Entries, +Opts
  ]
).

/** <module> BibTeX parsing

@author Wouter Beek
@version 2015/12-2016/01, 2016/02, 2016/04
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(io)).
:- use_module(library(pure_input)).
:- use_module(library(yall)).

:- dynamic
    user:prolog_file_type/2.

:- multifile
    user:prolog_file_type/2.

user:prolog_file_type(bib, bibtex).





%! bibtex_load(+Source, -Entries) is det.
%! bibtex_load(+Source, -Entries, +Opts) is det.

bibtex_load(Source, Entries):-
  bibtex_load(Source, Entries, []).


bibtex_load(Source, L, Opts):-
  call_on_stream(
    Source,
    {L}/[In,Meta,Meta]>>phrase_from_stream(In, bibtex(L)),
    Opts
  ).



bibtex([H|T]) --> blanks, entry(H), blanks, !, bibtex(T).
bibtex([])    --> blanks.

entry(entry(Class,Name,Pairs)) -->
  class(Class), "{", blanks, name(Name), blanks, ",", blanks,
  pairs(Pairs), blanks, "}".

pairs([H|T]) --> pair(H), !, (blanks, "," -> pairs(T) ; {T = []}).
pairs([])    --> "".

pair(Key-Val) --> blanks, name(Key), blanks, "=", blanks, value(Val).

value(Val) --> "{",  !, ...(Cs), "}",  {string_codes(Val, Cs)}.
value(Val) --> "\"", !, ...(Cs), "\"", {string_codes(Val, Cs)}.
value(Val) --> name(Val).

class(Class) -->
  "@", +(alpha, Cs), {string_codes(Class0, Cs)},
  {validate(Class0, Class, _, _)}.

name(Name) --> +(name_code, Cs), {string_codes(Name, Cs)}.
name_code(C)   --> alphadigit(C).
name_code(0':) --> ":".
name_code(0'-) --> "-".
name_code(0'_) --> "_".

comment --> "%", ..., eol.





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
    "Key ",
    Key,
    " is not allowed to be missing for BibTeX entry type ",
    Class,
    "."
  ].
