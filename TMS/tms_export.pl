:- module(
  tms_export,
  [
    export_argument/1, % +Node:node
    export_tms/1, % +TMS:atom
    export_tms/2 % +TMS:atom
                 % +Justifications:list(justification)
  ]
).

/** <module> TMS EXPORT

Exports TMS belief states,

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(os_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_read)).
:- use_module(standards(graphviz)).
:- use_module(tms(tms)).

:- rdf_register_ns(doyle, 'http://www.wouterbeek.com/doyle.owl#').



%! export_argument(+Node:node) is det.

export_argument(Node):-
  % Retrieve the TMS in which the node appears.
  node(TMS, Node),
  % The argument for the node consists of a list of justifications.
  tms_argument(Node, Justifications),
  % Export the justifications that constitute the argument.
  export_tms(TMS, Justifications).

%! export_file(-File:atom, -Stream:stream) is det.

export_file(File, Stream):-
  flag(tms_export, ID, ID + 1),
  format(atom(FileName), 'export_~w', [ID]),
  absolute_file_name(
    personal(FileName),
    File,
    [access(write), file_type(dot)]
  ),
  open(File, write, Stream, []).

%! export_pdf(+GV_File:atom) is det.

export_pdf(GV_File):-
  convert_graphviz(GV_File, dot, pdf, PDF_File),
  open_pdf(PDF_File).

%! export_tms(+TMS:atom) is det.
% Exports the TMS using GraphViz.

export_tms(TMS):-
  is_registered_tms(TMS),
  export_file(GV_File, Stream),
  tms_to_graphviz(TMS, Stream),
  close(Stream),
  export_pdf(GV_File).

%! export_tms(+TMS:atom, +Justifications:list(justification)) is det.
% Exports the TMS using GraphViz.

export_tms(TMS, Justifications):-
  is_registered_tms(TMS),
  export_file(GV_File, Stream),
  tms_to_graphviz(TMS, Justifications, Stream),
  close(Stream),
  export_pdf(GV_File).

tms_to_graphviz(TMS, Stream):-
  % Type checking.
  is_registered_tms(TMS),
  !,

  setoff(Node, node(TMS, Node), Nodes),
  setoff(Justification, justification(TMS, Justification), Justifications),

  tms_to_graphviz(TMS, Nodes, Justifications, Stream).

tms_to_graphviz(TMS, Justifications, Stream):-
  setoff(
    Node,
    (
      member(Justification, Justifications),
      (
        rdf_has(Justification, tms:has_antecedent, Node)
      ;
        rdf_has(Justification, tms:has_consequent, Node)
      )
    ),
    Nodes
  ),

  tms_to_graphviz(TMS, Nodes, Justifications, Stream).

tms_to_graphviz(TMS, Nodes, Justifications, Stream):-
  % Begin of graph.
  format(Stream, 'digraph circuit {\n', []),

  % Nodes
  forall(
    member(Node, Nodes),
    (
      rdf_datatype(Node, tms:has_id, int, NodeID, TMS),
      rdfs_label(Node, L),
      if_then_else(
        debug,
        format(atom(Label), '~w: ~w', [NodeID, L]),
        Label = L
      ),
      % The color indicates the support status of the node.
      (
        is_in_node(TMS, Node)
      ->
        Color = green
      ;
        is_out_node(TMS, Node)
      ->
        Color = red
      ;
        Color = black
      ),
      format(
        Stream,
        '  n~w [color="~w", fontsize="11", label="~w", shape="ellipse", style="solid"];\n',
        [NodeID, Color, Label]
      )
    )
  ),

  % Justifications
  forall(
    member(Justification, Justifications),
    (
      rdf_datatype(Justification, tms:has_id, int, JustificationID, TMS),
      rdfs_label(Justification, L),
      if_then_else(
        debug,
        format(atom(Label), '~w: ~w', [JustificationID, L]),
        Label = L
      ),
      format(
        Stream,
        '  j~w [color="blue", fontsize="11", label="~w", shape="rectangle", style="solid"];\n',
        [JustificationID, Label]
      )
    )
  ),

  format(Stream, '\n', []),

  % _In_list
  forall(
    (
      member(Justification, Justifications),
      rdf(Justification, tms:has_in, Node, TMS)
    ),
    (
      rdf_datatype(Node, tms:has_id, int, NodeID, TMS),
      rdf_datatype(Justification, tms:has_id, int, JustificationID, TMS),
      format(
        Stream,
        '  n~w -> j~w [color="black", style="solid"];\n',
        [NodeID, JustificationID]
      )
    )
  ),

  % _Out_list
  forall(
    (
      member(Justification, Justifications),
      rdf(Justification, tms:has_out, Node, TMS)
    ),
    (
      rdf_datatype(Node, tms:has_id, int, NodeID, TMS),
      rdf_datatype(Justification, tms:has_id, int, JustificationID, TMS),
      format(
        Stream,
        '  n~w -> j~w [color="black", style="dashed"];\n',
        [NodeID, JustificationID]
      )
    )
  ),

  % Consequences
  forall(
    (
      member(Justification, Justifications),
      rdf(Justification, tms:has_consequent, Node, TMS)
    ),
    (
      rdf_datatype(Node, tms:has_id, int, NodeID, TMS),
      rdf_datatype(Justification, tms:has_id, int, JustificationID, TMS),
      format(
        Stream,
        '  j~w -> n~w [color="black", style="solid"];\n',
        [JustificationID, NodeID]
      )
    )
  ),

  % Graph properties.
  format(Stream, '\n', []),
  format(Stream, '  charset="UTF-8"\n', []),
  format(Stream, '  fontsize="11"\n', []),
  format(Stream, '  label="~w"\n', [TMS]),
  format(Stream, '  overlap=false\n', []),

  % End of graph.
  format(Stream, '}\n', []).

