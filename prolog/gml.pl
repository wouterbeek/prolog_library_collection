:- module(
  gml,
  [
    gml_edge/3,  % +Out, +FromTerm, +ToTerm
    gml_edge/4,  % +Out, +FromTerm, +ToTerm, +Options
    gml_graph/2, % +Out, :Goal_1
    gml_graph/3, % +Out, :Goal_1, +Options
    gml_node/2,  % +Out, +Term
    gml_node/3   % +Out, +Term, +Options
  ]
).

/** <module> Support for the Graph Markup Language (GML)

```bnf
GML ::= List
List ::= (whitespace * Key whitespace + Value) *
Value ::= Integer | Real | String | [ List ]
Key ::= [ a-z A-Z ] [ a-z A-Z 0-9 ] *
Integer ::= sign digit +
Real ::= sign digit * . digit * mantissa
String ::= " instring "
sign ::= empty | + | -
digit ::= [0-9]
Mantissa ::= empty | E sign digit
instring ::= ASCII - {&,"} | & character + ;
whitespace ::= space | tabulator | newline
```

*/

:- use_module(library(apply)).

:- use_module(library(call_ext)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(term_ext)).

:- meta_predicate
    gml_graph(+, 1),
    gml_graph(+, 1, +).





%! gml_attributes(+Options:options, -String:string) is det.

gml_attributes(Options, String) :-
  dict_pairs(Options, Pairs),
  maplist(gml_attribute_, Pairs, Strings),
  atomics_to_string(Strings, " ", String).

gml_attribute_(Key-Value1, String) :-
  number(Value1), !,
  format(string(String), "~a ~w", [Key,Value1]).
gml_attribute_(Key-Value1, String) :-
  (   Key == label
  ->  string_phrase(gml_encode_label, Value1, Value2)
  ;   Value2 = Value1
  ),
  format(string(String), "~a \"~w\"", [Key,Value2]).



%! gml_edge(+Out:stream, +FromTerm:term, +ToTerm:term) is det.
%! gml_edge(+Out:stream, +FromTerm:term, +ToTerm:term, +Options:options) is det.

gml_edge(Out, FromTerm, ToTerm) :-
  gml_edge(Out, FromTerm, ToTerm, options{}).


gml_edge(Out, FromTerm, ToTerm, Options) :-
  maplist(ascii_id, [FromTerm,ToTerm,FromTerm-ToTerm], [FromId,ToId,Id]),
  gml_attributes(Options, String),
  format_debug(gml, Out, "  edge [ id \"~a\" source \"~a\" target \"~a\" ~s ]", [Id,FromId,ToId,String]).



%! gml_graph(+Out:stream, :Goal_1) is det.
%! gml_graph(+Out:stream, :Goal_1, +Options:options) is det.
%
% @arg Options The following options are supported:
%
%      * directed(+boolean)
%
%        Whether the graph is directed (`true`) or undirected
%        (`false`, default).

gml_graph(Out, Goal_1) :-
  gml_graph(Out, Goal_1, options{}).


gml_graph(Out, Goal_1, Options) :-
  dict_get(directed, false, Options, Directed),
  must_be(boolean, Directed),
  boolean_value(Directed, DirectedN),
  format_debug(gml, Out, "graph [ directed ~d", [DirectedN]),
  call(Goal_1, Out),
  format_debug(gml, Out, "]").



%! gml_node(+Out:stream, +Term:term) is det.
%! gml_node(+Out:stream, +Term:term, +Options:options) is det.

gml_node(Out, Term) :-
  gml_node(Out, Term, []).


gml_node(Out, Term, Options) :-
  ascii_id(Term, Id),
  gml_attributes(Options, String),
  format_debug(gml, Out, "  node [ id \"~a\" ~s ]", [Id,String]).





% HELPERS %

%! boolean_value(+Directed:boolean, +N:between(0,1)) is semidet.
%! boolean_value(+Directed:boolean, -N:between(0,1)) is det.
%! boolean_value(-Directed:boolean, +N:between(0,1)) is det.
%! boolean_value(-Directed:boolean, -N:between(0,1)) is multi.

boolean_value(false, 0).
boolean_value(true, 1).
