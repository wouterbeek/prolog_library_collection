:- module(
  html_dom,
  [
    atom_to_html_dom/2, % +Atom:atom
                        % -Dom:compound
    html_insert_dom/1 % +Dom:compound
  ]
).

/** <module> HTML download

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(http/html_write)).
:- use_module(library(memfile)).
:- use_module(library(sgml)).
:- use_module(library(xml/xml_dom)).





%! atom_to_html_dom(+Atom:atom, -Dom:compound) is det.

atom_to_html_dom(A, Dom):-
  setup_call_cleanup(
    atom_to_memory_file(A, Handle),
    setup_call_cleanup(
      open_memory_file(Handle, read, Read),
      load_html(Read, Dom, []),
      close(Read)
    ),
    free_memory_file(Handle)
  ).



%! html_insert_dom(+Dom:list(compound))// is det.
% Includes the given XML DOM inside a generated HTML page.
%
% DOM is either a list or compound term or an atom.

html_insert_dom(Dom) -->
  {xml_dom_to_atom(Dom, A)},
  html(\[A]).
