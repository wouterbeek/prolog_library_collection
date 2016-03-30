:- module(
  xml_dom,
  [
    atom_to_xml_dom/2,  % +Atom, -Dom
    xml_dom_as_atom//1, % +Dom
    xml_serve_dom/1     % +Dom
  ]
).

/** <module> XML DOM

@author Wouter Beek
@version 2015/07, 2015/10, 2016/03
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_request)).
:- use_module(library(memfile)).
:- use_module(library(semweb/rdf11), []).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).





%! atom_to_xml_dom(+Atom, Dom) is det.

atom_to_xml_dom(A, Dom):-
  setup_call_cleanup(
    atom_to_memory_file(A, Handle),
    setup_call_cleanup(
      open_memory_file(Handle, read, Read),
      load_xml(Read, Dom, []),
      close(Read)
    ),
    free_memory_file(Handle)
  ).



%! xml_dom_as_atom(+Dom)// is det.
% Includes the given DOM inside the generated HTML page.
%
% DOM is either a list or compound term or an atom.

xml_dom_as_atom(Dom) -->
  {rdf11:in_xml_literal(xml, Dom, A)},
  html(\[A]).



%! xml_serve_dom(+Dom) is det.
% Serves the given XML DOM.

xml_serve_dom(Dom) :-
  rdf11:in_xml_literal(xml, Dom, A),
  % The User Agent needs to know the content type and encoding.
  % If the UTF-8 encoding is not given here explicitly,
  % Prolog throws an IO exception on `format(XML)`.
  format("Content-type: application/xml; charset=utf-8~n~n"),
  format(A).
