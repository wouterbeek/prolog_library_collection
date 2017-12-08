:- module(
  gml,
  [
    gml_edge/4, % +Out, +FromId, +ToId, +Attributes
    gml_id/2,   % +Term, -Id
    gml_node/3  % +Out, +Id, +Attributes
  ]
).

/** <module> Graph Markup Language (GML)

@author Wouter Beek
@version 2017/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(hash_ext)).





%! gml_attributes(+Attributes:list(compound), -String:string) is det.

gml_attributes(Attrs, String) :-
  option(label(Label), Attrs), !,
  gml_label(Label, EncLabel),
  format(string(String), " label \"~s\"", [EncLabel]).
gml_attributes([], "").



%! gml_edge(+Out:stream, +FromId:atom, +ToId:atom,
%!          +Attributes:list(compound)) is det.

gml_edge(Out, FromId, ToId, Attrs) :-
  gml_attributes(Attrs, String),
  format_debug(gml, Out, "  edge [ source ~a target ~a~s ]", [FromId,ToId,String]).



%! gml_id(@Term, -Id:atom) is det.
%
% Id is a GML-compatible unique identifier for Term.

gml_id(Term, Id) :-
  md5(Term, Hash),
  atomic_concat(n, Hash, Id).



%! gml_label(+Label:string, -EncodedLabel:string) is det.

gml_label(Label, EncLabel) :-
  string_phrase(gml_encode_label, Label, EncLabel).

% ASCII characters, excluding double quote (34) and ampersand (38).
gml_encode_label, [Code] -->
  [Code],
  {(between(0, 33, Code) ; between(35, 37, Code) ; between(39, 127, Code))}, !,
  gml_encode_label.
% Other characters are &-encoded.
gml_encode_label, Escape -->
  [Code], !,
  {format(codes(Escape), "&#~d", [Code])},
  gml_encode_label.
gml_encode_label --> "".



%! gml_node(+Out:stream, +Id:atom, +Attributes:list(compound)) is det.

gml_node(Out, Id, Attrs) :-
  gml_attributes(Attrs, String),
  format_debug(gml, Out, "  node [ id ~a~s ]", [Id,String]).
