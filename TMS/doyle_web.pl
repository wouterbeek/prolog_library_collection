:- module(
  doyle_web,
  [
    doyle_web/2 % +TMS:atom
                % -Markup:dom
  ]
).

/** <module> DOYLE WEB

Web predicates for Doyle's TMS.

@author Wouter Beek
@version 2013/05
*/

:- use_module(html(html)).
:- use_module(tms(doyle)).
:- use_module(tms(tms)).



doyle_web(TMS, Markup):-
  findall(
    tr([
      td(Node),
      td(SupportStatus),
      %td(SupportingJustifications),
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
      node(TMS, Node),
      doyle:support_status(Node, SupportStatus),
      %doyle:supporting_justifications(Node, SupportingJustifications),
      doyle:supporting_nodes(Node, SupportingNodes),
      doyle:antecedents(Node, Antecedents),
      doyle:foundations(Node, Foundations),
      %doyle:ancestors(Node, Ancestors),
      doyle:consequences(Node, Consequences),
      doyle:affected_consequences(Node, AffectedConsequences),
      doyle:believed_consequences(Node, BelievedConsequences),
      doyle:repercussions(Node, Repercussions),
      doyle:believed_repercussions(Node, BelievedRepercussions)
    ),
    Rows
  ),
  list_to_table(
    [header(true)],
    [
      [
        'Node',
        'Support status',
        %'Supporting justification',
        'Supporting nodes',
        'Antecedents',
        'Foundations',
        %'Ancestors',
        'Consequences',
        'Affected consequences',
        'Believed consequences',
        'Repercussions',
        'Believed repercussions'
      ]
    |
      Rows
    ],
    Markup
  ).

