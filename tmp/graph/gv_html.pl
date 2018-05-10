:- module(
  gv_html,
  [
    gv_html_like_label//1 % +Content:compound
  ]
).

/** <module> GraphViz: HTML-like labels

@author Wouter Beek
@see http://www.graphviz.org/content/node-shapes#html
@version 2015/07, 2015/12, 2017/08
*/

:- use_module(library(dcg)).
:- use_module(library(html/html_dcg)).





%! gv_html_like_label(+Content:compound)// is det.

gv_html_like_label(Content) --> "<", label(Content), ">".



%! cell(+Contents:compound)// is det.

cell(td(Contents)) --> !,
  cell(td([],Contents)).
cell(td(Attrs1,Image)) -->
  {(Image =.. [img,Attrs2] -> true ; Image == img -> Attrs2 = [])}, !,
  html_element(td, Attrs1, html_element(img,Attrs2)).
cell(td(Attrs,Contents)) -->
  html_element(td, Attrs, label(Contents)).



%! cells(+Contents:list(compound))// is det.

cells([H,vr|T]) --> !, cell(H), html_element(vr), cells(T).
cells([H|T])    --> !, cell(H), cells(T).
cells([])       --> "".



%! label(+Content:compound)// is det.

label(Content) --> table(Content), !.
label(Content) --> text(Content).



%! row(+Contents:compound)// is det.

row(tr(Contents)) --> html_element(tr, [], cells(Contents)).



%! rows(+Contents:list)// is det.

rows([hr|T]) --> !, html_element(hr), rows(T).
rows([H|T]) --> row(H), !, rows(T).
rows([]) --> "".



%! table(+Contents:compound)// is det.

table(table(Contents)) --> !,
  table(table([],Contents)).
table(table(Attrs,Contents)) --> !,
  html_element(table, Attrs, rows(Contents)).
table(font(Table)) --> !,
  table(font([],Table)).
table(font(Attrs1,Table)) -->
  {(  Table =.. [table,Attrs2,Contents]
  ->  true
  ;   Table =.. [table,Contents]
  ->  Attrs2 = []
  )},
  html_element(font, Attrs1, table(table(Attrs2,Contents))).



%! text(+Contents:list)// .

text(Contents) -->
  {is_list(Contents)}, !,
  +(textitem, Contents).
text(Content) -->
  text([Content]).



%! textitem(+Content:compound)// .

textitem(br(Attrs)) --> !,
  html_element(br, Attrs).
% Compound term: parser.
textitem(Compound) -->
  {var(Compound)}, !,
  html_element(Name, _, text(Content)),
  {
    supported_html_element(Name),
    Compound =.. [Name,Content]
  }.
% Compound term: generator.
textitem(Compound) -->
  {
    Compound =.. [Name,Content], !,
    supported_html_element(Name)
  },
  html_element(Name, _, text(Content)).
textitem(String) -->
  html_string(String).





% HELPERS %

%! supported_html_element(+Name:atom) is semidet.
%! supported_html_element(-Name:atom) is multi.

supported_html_element(b).
supported_html_element(font).
supported_html_element(i).
supported_html_element(o).
supported_html_element(s).
supported_html_element(sub).
supported_html_element(sup).
supported_html_element(u).
