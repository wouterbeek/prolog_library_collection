:- module(
  xml_ext,
  [
    call_on_xml/3,     % +UriSpec, +RecordName, :Goal_1
    call_on_xml/4,     % +UriSpec, +RecordName, :Goal_1, +Options
    html_download/2,   % +UriSpec, -Dom
    html_download/3,   % +UriSpec, -Dom, +Options
    html_insert_dom//1 % +Dom
  ]
).

/** <module> XML extensions

@author Wouter Beek
@version 2016/06-2017/08
*/

:- use_module(library(atom_ext)).
:- use_module(library(c14n2)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
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



%! html_download(+UriSpec:term, -Dom:list(compound)) is det.
%! html_download(+UriSpec:term, -Dom:list(compound),
%!               +Options:list(compound)) is det.

html_download(UriSpec, Dom) :-
  html_download(UriSpec, Dom, []).


html_download(UriSpec, Dom, Options) :-
  call_on_uri(UriSpec, html_download_(Dom, Options), Options).

html_download_(Dom2, Options1, In, Meta, Meta) :-
  merge_options([encoding('utf-8'),max_errors(-1)], Options1, Options2),
  load_html(In, Dom1, Options2),
  clean_dom(Dom1, Dom2).



%! html_insert_dom//1 % +Dom

html_insert_dom(Dom) -->
  {with_output_to(atom(Atom), xml_write_canonical(current_output, Dom, []))},
  html(\[Atom]).





% HELPERS %

%! clean_dom(+Dom1:list(compound), -Dom2:list(compound)) is det.
%
% Clean the given DOM tree in the following two ways:
%
%   1. Strip all blanks from the beginning and end of all strings.
%
%   2. Remove all strings that are empty under (1) from the DOM tree.

clean_dom([H1|T1], L2) :-
  atom(H1), !,
  % Strip all blanks from strings that appear in the DOM.
  atom_strip(H1, H2),
  % Remove empty strings from the DOM.
  (H2 == '' -> L2 = T2 ; L2 = [H2|T2]),
  clean_dom(T1, T2).
clean_dom([element(N,As,Contents1)|T1], [element(N,As,Contents2)|T2]) :- !,
  clean_dom(Contents1, Contents2),
  clean_dom(T1, T2).
clean_dom([], []).
