:- module(
  tms_export,
  [
    export_tms/1 % +TMS:atom
  ]
).

/** <module> TMS EXPORT

Exports TMS belief states,

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(os_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_read)).
:- use_module(standards(graphviz)).
:- use_module(tms(tms)).

:- rdf_register_ns(doyle, 'http://www.wouterbeek.com/doyle.owl#').



%% export_tms(+TMS:atom) is det.
% Exports the TMS using GraphViz.

export_tms(TMS):-
  absolute_file_name(
    personal(export),
    GV_File,
    [access(write), file_type(dot)]
  ),
  open(GV_File, write, Stream, []),
  tms_to_graphviz(TMS, Stream),
  close(Stream),
  %open_dot(GV_File),
  %convert_graphviz(GV_File, dot, svg, _SVG_File),
  convert_graphviz(GV_File, dot, pdf, PDF_File),
  open_pdf(PDF_File).

tms_to_graphviz(TMS, Stream):-
  % Begin of graph.
  format(Stream, 'digraph circuit {\n', []),
  
  % Nodes
  forall(
    node(TMS, Node),
    (
      rdf_datatype(Node, tms:has_id, int, NodeID, TMS),
      rdfs_label(Node, Label),
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
    justification(TMS, Justification),
    (
      rdf_datatype(Justification, tms:has_id, int, JustificationID, TMS),
      rdfs_label(Justification, Label),
      format(
        Stream,
        '  j~w [color="blue", fontsize="11", label="~w", shape="diamond", style="solid"];\n',
        [JustificationID, Label]
      )
    )
  ),
  
  format(Stream, '\n', []),
  
  % _In_list
  forall(
    rdf(Justification, tms:has_in, Node, TMS),
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
    rdf(Justification, tms:has_out, Node, TMS),
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
    rdf(Justification, tms:has_consequence, Node, TMS),
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

