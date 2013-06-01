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
  dbnl_dom_center(DOM, Content),
  forall(
    xpath_chk2(Content, //p(content), Paragraph),
    (
      split_list_exclusive(
        Paragraph,
        [element(br, _, []), element(br, _, [])],
        Entries
      ),
      maplist(dbnl_bibliography(Graph, Bibliography), Entries)
    )
  ).

%! dbnl_bibliography(+Graph:atom, +Bibliography:uri, +HTML:dom):-

dbnl_bibliography(Graph, Bibliography, HTML):-
  dbnl_markup([graph(Graph), text(Bibliography)], HTML, XML),
  dbnl_text_content(Graph, Bibliography, XML).

