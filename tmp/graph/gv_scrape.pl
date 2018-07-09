:- module(
  gv_scrape,
  [
    gv_scrape_attrs/1, % +File
    gv_scrape_color/1  % +File
  ]
).

/** <module> GraphViz: Scrape content

Writes compound terms of the following form to file:

```prolog
gv_attr(
  ?Name,
  ?UsedBy:list(oneof([cluster,edge,graph,node,subgraph])),
  ?Types:list(atom),
  ?Default,
  ?Minimum,
  ?Notes
) is nondet.
```

@author Wouter Beek
@version 2015/10, 2016/07, 2017/08, 2017/11
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(sgml)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(term_ext)).
:- use_module(library(xml_ext)).
:- use_module(library(xml/xpath_ext)).





%! gv_scrape_attrs(+FileSpec:term) is det.

gv_scrape_attrs(FileSpec) :-
  call_to_file(FileSpec, gv_scrape_attrs).


gv_scrape_attrs(Out, M1, M1) :-
  load_html('http://www.graphviz.org/doc/info/attrs.html', Dom),
  xpath_chk(Dom, //table(@align=lower_case(center)), Table),
  xpath_table(Table, Rows),
  member([Name,UsedBy0,Types0,Default0,Minimum,Notes], Rows),
  atom_phrase(usedby(UsedBy), UsedBy0),
  once(atom_phrase(types(Types), Types0)),
  sort(UsedBy2, UsedBy3),
  translate_default(Default1, Default2),
  with_output_to(
    Out,
    write_fact(gv_attr(Name, UsedBy3, Types2, Default2, Minimum, Notes))
  ).



%! gv_scrape_color(+FileSpec:term) is det.

gv_scrape_color(FileSpec) :-
  call_to_file(FileSpec, gv_scrape_color).

gv_scrape_color(Out, M1, M1) :-
  load_html('http://www.graphviz.org/doc/info/colors.html', Dom, []),
  xpath_chk(Dom, //table(1), Table1),
  xpath_chk(Dom, //table(2), Table2),
  maplist(write_color_table(Out), [x11,svg], [Table1,Table2]).

write_color_table(Out, Colorscheme, Table) :-
  xpath_table(Table, Rows),
  append(Rows, Cells),
  forall(
    member(Cell, Cells),
    with_output_to(Out, write_fact(gv_color(Colorscheme, Cell)))
  ).





% HELPERS %

%! translate_default(+Default1, -Default2) is det.

% The empty string is represented by the empty atom.
translate_default('""', '') :- !.
% The absence of a default value is represented by an uninstantiated variable.
translate_default('<none>', _) :- !.
translate_default(Default, Default).



%! types(-Types:ordset(atom))// is det.

types(Types) -->
  types_([], Types).

types_(T, L) -->
  type_(H), !,
  types_([H|T], L).
types_(L1, L2) -->
  {sort(L1, L2)}.

type_(Atom) -->
  string(Codes),
  blank, !,
  blanks,
  {atom_codes(Atom, Codes)}.



%! usedby(-Categories:ordset(oneof([cluster,edge,graph,node,subgraph])))// is det.

usedby(Categories) -->
  usedby_([], Categories).

usedby_(T, L) -->
  usedby__(H), !,
  usedby_([H|T], L).
usedby_(L1, L2) -->
  {sort(L1, L2)}.

usedby__([cluster|T]) --> "C".
usedby__([edge|T]) --> "E".
usedby__([graph|T]) --> "G".
usedby__([node|T]) --> "N".
usedby__([subgraph|T]) --> "S".
