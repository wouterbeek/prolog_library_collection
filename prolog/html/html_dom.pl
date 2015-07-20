:- module(
  html_dom,
  [
    atom_to_html_dom/2 % +Atom:atom
                       % -Dom:compound
  ]
).

/** <module> HTML download

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(memfile)).
:- use_module(library(sgml)).





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
