:- module(
  marcxml,
  [
    marcxml_assert/0,
    marcxml_assert/1, % +Source
    marcxml_field/2,  % ?Key, -NumFields
    marcxml_field/6   % ?Id, ?Key, ?A, ?B, ?Code, ?Val
  ]
).

/** <module> MARCXML

@author Wouter Beek
@see https://www.loc.gov/standards/marcxml/
@version 2016/06, 2017/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(nlp/nlp_guess)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(solution_sequences)).
:- use_module(library(xml/xml_stream)).
:- use_module(library(xpath)).
:- use_module(library(yall)).

:- dynamic
    field/6.

:- rdf_create_alias(iisg, 'https://iisg.amsterdam/').





%! marcxml_assert is det.
%! marcxml_assert(+Source) is det.

marcxml_assert :-
  marcxml_assert(file('iish.evergreen.authorities.xml')).


marcxml_assert(Source) :-
  rdf_call_to_ntriples('marc.nt.gz', marc_assert0(Source)).

marc_assert0(Source, State, Out) :-
  xml_stream_record(
    Source,
    ['marc:record'],
    marcxml_dom_assert(stream(State,Out))
  ).



%! marcxml_dom_assert(+M, +Dom) is det.

marcxml_dom_assert(M, [element('marc:record',_,Doms)]) :-
  forall(
    member(Dom, Doms),
    (
      flag(marcxml_record, Id, Id + 1),
      atom_number(SLocal, Id),
      marcxml_dom_field(Dom, Field),
      rdf_marcxml_assert_field(M, SLocal, Field)
    )
  ).

rdf_marcxml_assert_field(M, SLocal, field(Key,A,B,Code,Val)) :-
  rdf_global_id(iisg:SLocal, S),
  atomic_list_concat([Key,A,B,Code], -, PLocal),
  rdf_global_id(iisg:PLocal, P),
  rdf_assert(M, S, P, Val^^xsd:string).



%! marcxml_dom_controlfield(+Dom, -Field) is det.

marcxml_dom_controlfield(
  element('marc:controlfield',[tag=Key1],[Val1]),
  field(Key2,'','','',Val2)
) :-
  maplist(strip_atom, [Key1,Val1], [Key2,Val2]).



%! marcxml_dom_datafield(+Dom, -Field) is det.

marcxml_dom_datafield(
  element('marc:datafield',[ind1=A1,ind2=B1,tag=Tag1],Doms),
  Field
) :-
  member(Dom, Doms),
  maplist(strip_atom, [Tag1,A1,B1], [Tag2,A2,B2]),
  marcxml_dom_subfield(Tag2, A2, B2, Dom, Field).



%! marcxml_dom_field(+Dom, -Field) is nondet.
%
% Control- or datafield.

marcxml_dom_field(
  element(datestamp,_,[Val1]),
  field(datestamp,'','','',Val2)
) :- !,
  strip_atom(Val1, Val2).
marcxml_dom_field(
  element('marc:leader',_,[Val1]),
  field(leader,'','','',Val2)
) :- !,
  strip_atom(Val1, Val2).
marcxml_dom_field(Dom, Field) :-
  marcxml_dom_controlfield(Dom, Field), !.
marcxml_dom_field(Dom, Field) :-
  marcxml_dom_datafield(Dom, Field).



%! marcxml_dom_subfield(+Tag, +A, +B, +Dom, -Pair) is det.

marcxml_dom_subfield(
  Tag1,
  A1,
  B1,
  element('marc:subfield',[code=Code1],[Val1]),
  field(Tag2,A2,B2,Code2,Val2)
) :-
  maplist(strip_atom, [Tag1,A1,B1,Code1,Val1], [Tag2,A2,B2,Code2,Val2]).




%! marcxml_field(?Key, -NumFields) is nondet.

marcxml_field(Key, NumFields) :-
  distinct(Key, marcxml_field(_, Key, _, _, _, _)),
  aggregate_all(count, marcxml_field(_, Key, _, _, _, _), NumFields).



%! marcxml_field(?Id, ?Key, ?A, ?B, ?Code, ?Val) is nondet.

marcxml_field(Id, Key, A, B, Code, Val) :-
   field(Id, Key, A, B, Code, Val).



%! marcxml_field_pair(+Field, -Pair) is det.
%! marcxml_field_pair(-Field, +Pair) is det.

marcxml_field_pair(field(Key0,A,B,Code,Val), Key-Val) :-
  atomic_list_concat([Key0,A,B,Code], -, Key).
