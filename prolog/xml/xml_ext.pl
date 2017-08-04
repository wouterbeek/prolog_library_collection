:- module(
  xml_ext,
  [
    call_on_xml/3, % +UriSpec, +RecordName, :Goal_1
    call_on_xml/4  % +UriSpec, +RecordName, :Goal_1, +Options
  ]
).

/** <module> XML extensions

@author Wouter Beek
@version 2016/06, 2017/02, 2017/07
*/

:- use_module(library(atom_ext)).
:- use_module(library(sgml)).
:- use_module(library(uri/uri_ext)).

:- meta_predicate
    call_on_xml(+, +, 1),
    call_on_xml(+, +, 1, +),
    call_on_xml_stream(+, 1, +, +, -).





%! call_on_xml(+UriSpec:term, +RecordName:atom, :Goal_1) is det.
%! call_on_xml(+UriSpec:term, +RecordName:atom, :Goal_1,
%!             +Options:list(compound)) is det.
%
% Call Goal_1 on an XML stream, where the argument supplied to Goal_1
% is a subtree that starts with an element called RecordName.

call_on_xml(UriSpec, RecordName, Goal_1) :-
  call_on_xml(UriSpec, RecordName, Goal_1, []).


call_on_xml(UriSpec, RecordName, Goal_1, Options) :-
  call_on_uri(UriSpec, call_on_xml_stream(RecordName, Goal_1), Options).

call_on_xml_stream(RecordName, Goal_1, In, Metadata, Metadata) :-
  b_setval(xml_stream_goal, Goal_1),
  b_setval(xml_stream_record_name, RecordName),
  setup_call_cleanup(
    new_sgml_parser(Parser, []),
    (
      set_sgml_parser(Parser, space(remove)),
      sgml_parse(Parser, [call(begin,on_begin0),source(In)])
    ),
    free_sgml_parser(Parser)
  ).

on_begin0(Elem, Attr, Parser) :-
  b_getval(xml_stream_goal, Goal_1),
  b_getval(xml_stream_record_name, Elem), !,
  sgml_parse(Parser, [document(Dom1),parse(content)]),
  xml_clean_dom(Dom1, Dom2),
  call(Goal_1, [element(Elem,Attr,Dom2)]).

xml_clean_dom([element(Elem,Attr,Dom1)|T1], [element(Elem,Attr,Dom2)|T2]) :- !,
  xml_clean_dom(Dom1, Dom2),
  xml_clean_dom(T1, T2).
xml_clean_dom([H|T1], T2) :-
  is_empty_atom(H), !,
  xml_clean_dom(T1, T2).
xml_clean_dom([H1|T1], [H2|T2]) :- !,
  atom_strip(H1, H2),
  xml_clean_dom(T1, T2).
xml_clean_dom([], []).
