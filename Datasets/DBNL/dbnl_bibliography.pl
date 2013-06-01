:- module(
  dbnl_bibliography,
  [
    dbnl_bibliography/3 % +Graph:atom
                        % +URI:uri
                        % -Bibliography:uri
  ]
).

/** <module> DBNL BIBLIOGRAPHY

Predicates for parsing DBNL bibliography texts.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_markup)).
:- use_module(dbnl(dbnl_text)).
:- use_module(standards(xpath_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_bibliography(+Graph:atom, +URI:uri, -Bibliography:uri) is det.

dbnl_bibliography(Graph, URI, Bibliography):-
  dbnl_assert_bibliography(Graph, URI, Bibliography),
  dbnl_uri_to_html(URI, DOM),
  dbnl_dom_center(DOM, Contents1),
  xpath_chk2(Contents1, //p(content), Contents2),
  split_list_exclusive(
    Contents2,
    [element(br, _, []), element(br, _, [])],
    Chunks
  ),
  maplist(dbnl_bibliography0(Graph, Bibliography), Chunks).

dbnl_bibliography0(Graph, Bibliography):-
  dbnl_markup([graph(Graph), text(Bibliography)], XML_DOM),
  dbnl_text_content(Graph, Bibliography, XML_DOM).

