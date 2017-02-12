:- module(
  xml_stream,
  [
    xml_stream_record/3 % +Source, +RecordNames, :Goal_1
  ]
).

/** <module> XML stream

@author Wouter Beek
@version 2016/06, 2017/02
*/

:- use_module(library(atom_ext)).
:- use_module(library(io)).
:- use_module(library(sgml)).

:- meta_predicate
    xml_stream_record(+, +, 1).





%! xml_stream_record(+Source, +RecordNames, :Goal_1) is det.
%
% Call Goal_1 on an XML stream, where the argument supplied to Goal_1
% is a subtree that starts with an elements within RecordNames.

xml_stream_record(Source, RecordNames, Goal_1) :-
  b_setval(xml_stream_goal, Goal_1),
  b_setval(xml_stream_record_names, RecordNames),
  call_on_stream(Source, xml_stream_record_stream0).

xml_stream_record_stream0(In, Meta, Meta) :-
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
  b_getval(xml_stream_record_names, RecordNames),
  memberchk(Elem, RecordNames), !,
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
  strip_atom(H1, H2),
  xml_clean_dom(T1, T2).
xml_clean_dom([], []).
