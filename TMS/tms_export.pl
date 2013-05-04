:- module(
  tms_export,
  [
    export_tms/0
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

:- rdf_register_ns(doyle, 'http://www.wouterbeek.com/doyle.owl#').



%% export_tms is det.
% Exports the TMS using GraphViz.

export_tms:-
  absolute_file_name(data(export), GV_File, [access(write), file_type(dot)]),
  open(GV_File, write, Stream, []),
  tms_to_graphviz(Stream),
  close(Stream),
  open_dot(GV_File),
  convert_graphviz(GV_File, dot, svg, _SVG_File),
  convert_graphviz(GV_File, dot, pdf, PDF_File),
  open_pdf(PDF_File).


tms_to_graphviz(Stream):-
  format(Stream, 'digraph circuit {\n', []),
  forall(
    rdfs_individual_of(Node, doyle:'Node'),
    (
      rdf_datatype(Node, doyle:has_id, int, NodeID, doyle),
      format(
        Stream,
        '  n~w [color="green", fontsize="11", label="~w", shape="ellipse", style="solid"];\n',
        [NodeID, NodeID]
      )
    )
  ),
  forall(
    rdfs_individual_of(Justification, doyle:'Justification'),
    (
      rdf_datatype(
        Justification,
        doyle:has_id,
        int,
        JustificationID,
        doyle
      ),
      format(
        Stream,
        '  j~w [color="blue", fontsize="11", label="~w", shape="box", style="solid"];\n',
        [JustificationID, JustificationID]
      )
    )
  ),
  format(Stream, '\n', []),
  forall(
    rdf(Justification, doyle:has_in, Node, doyle),
    (
      rdf_datatype(Node, doyle:has_id, int, NodeID, doyle),
      rdf_datatype(
        Justification,
        doyle:has_id,
        int,
        JustificationID,
        doyle
      ),
      format(
        Stream,
        '  n~w -> j~w [color="black"];\n',
        [NodeID, JustificationID]
      )
    )
  ),
  forall(
    rdf(Justification, doyle:has_out, Node, doyle),
    (
      rdf_datatype(Node, doyle:has_id, int, NodeID, doyle),
      rdf_datatype(
        Justification,
        doyle:has_id,
        int,
        JustificationID,
        doyle
      ),
      format(
        Stream,
        '  n~w -> j~w [color="red"];\n',
        [NodeID, JustificationID]
      )
    )
  ),
  forall(
    rdf(Justification, doyle:has_consequence, Node, doyle),
    (
      rdf_datatype(Node, doyle:has_id, int, NodeID, doyle),
      rdf_datatype(
        Justification,
        doyle:has_id,
        int,
        JustificationID,
        doyle
      ),
      format(
        Stream,
        '  j~w -> n~w [color="black"];\n',
        [JustificationID, NodeID]
      )
    )
  ),
  format(Stream, '\n', []),
  format(Stream, '  charset="UTF-8"\n', []),
  format(Stream, '  fontsize="11"\n', []),
  format(Stream, '  label="Doyle"\n', []),
  format(Stream, '  overlap=false\n', []),
  format(Stream, '}\n', []).

/*
truth_maintenance(Justification, Node):-
  in_node(Node),
  !.
truth_maintenance(Justification, Node):-
  out_node(Node),
  \+(is_valid(Justification)),
  !.
truth_maintenance(Justification, Node):-
  affected_consequences(Node, AffectedConsequences),
  truth_maintenance2(Justification, Node, AffectedConsequences).

truth_maintenance2(_Justification, _Node, []):-
  !.
truth_maintenance2(_Justification, Node, AffectedConsequences):-
  repercussions(Node, Repercussions),
  ord_add_element(Repercussions, Node, Repercussions_),

write_doyle(_Request):-
  findall(
    tr([
      td(Node),
      td(SupportStatus),
      td(SupportingJustifications),
      td(SupportingNodes),
      td(Antecedents),
      td(Foundations),
      %td(Ancestors),
      td(Consequences),
      td(AffectedConsequences),
      td(BelievedConsequences),
      td(Repercussions),
      td(BelievedRepercussions)
    ]),
    (
      node(Node),
      support_status(Node, SupportStatus),
      supporting_justifications(Node, SupportingJustifications),
      supporting_nodes(Node, SupportingNodes),
      antecedents(Node, Antecedents),
      foundations(Node, Foundations),
      %ancestors(Node, Ancestors),
      consequences(Node, Consequences),
      affected_consequences(Node, AffectedConsequences),
      believed_consequences(Node, BelievedConsequences),
      repercussions(Node, Repercussions),
      believed_repercussions(Node, BelievedRepercussions)
    ),
    Rows
  ),
  reply_html_page(
    title('Doyle'),
    [
      a(href('index.html'), 'Index'),
      table(
        [align(center), border(1), width('80%')],
        [tr([
          th('Node'),
          th('SupportStatus'),
          th('SupportingJustifications'),
          th('SupportingNodes'),
          th('Antecedents'),
          th('Foundations'),
          %th('Ancestors'),
          th('Consequences'),
          th('AffectedConsequences'),
          th('BelievedConsequences'),
          th('Repercussions'),
          th('BelievedRepercussions')
          | Rows
        ])]
      )
    ]
  ).
*/

