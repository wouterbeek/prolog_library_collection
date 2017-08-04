:- module(
  html_dom,
  [
    atom_to_html_dom/2, % +A, -Dom
    atom_to_html_dom/2, % +A, -Dom, +Opts
    html_insert_dom//1  % +Dom
  ]
).

/** <module> HTML DOM

@author Wouter Beek
@version 2015/07, 2016/03, 2016/05
*/

:- use_module(library(http/html_write)).
:- use_module(library(memfile)).
:- use_module(library(sgml)).





%! atom_to_html_dom(+Atom, -Dom) is det.
%! atom_to_html_dom(+Atom, -Dom, +Opts) is det.

atom_to_html_dom(A, Dom) :-
  atom_to_html_dom(A, Dom, []).


atom_to_html_dom(A, Dom, Opts) :-
  setup_call_cleanup(
    atom_to_memory_file(A, Handle),
    setup_call_cleanup(
      open_memory_file(Handle, read, Read),
      load_html(Read, Dom, Opts),
      close(Read)
    ),
    free_memory_file(Handle)
  ).



%! html_insert_dom(+Dom)// is det.
%
% Includes the given XML DOM inside a generated HTML page.
%
% DOM is either a list or compound term or an atom.

html_insert_dom(Dom) -->
  {rdf11:in_xml_literal(html, Dom, A)},
  html(\[A]).
