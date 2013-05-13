:- module(
  ugraphs_ext_web,
  [
    bipartite_web/2 % +Graph:graph
                    % -Markup:list
  ]
).

/** <module> UGRAPHS_EXT_WEB

Methods for generating SVG markup for undirected graphs.

There is a subtle difference between _|_web|_ and _|_markup|_ methods.
The former returns an SVG root element, for direct inclusion in an SVG
or (X)HTML5 document. The latter returns markup that cannot be directly
displayed but that can be used, together with other SVG and (X)HTML5
elements, within a composite. The latter is for instance used for
displaying multiple graphs on a single web page.

In general, the markup method does the real job of generating the graphic,
while the web method uses this markup by setting some of its parameters
and then insering this markup into a broader context of markup that is
ready for web browser display.

# Datatypes

## =graph=

The parent of the datatypes =ugraph= (i.e., undirected graph) and
=dgraph= (i.e., directed graph).

## =dgraph=

A directed graph, see [dgraph.pl].

## =ugraph=

An undirected graph, see [udgraph_ext.pl].

@author Wouter Beek
@version 2012/10, 2012/12-2013/01, 2013/05
@tbd Integrate this with GRAPH_EXPORT.
*/

:- use_module(graph_theory(graph_export)).
:- use_module(graph_theory(graph_generic)).
:- use_module(server(error_web)).
:- use_module(standards(standards)).
:- use_module(svg(svg)).



bipartite_web(UGraph, [SVG_Root, element(p, [], [AtomicG])]):-
  default_surface(Size),
  svg_head(Size, SVG_Head),
  bipartite(UGraph, S1, S2),

  % Global parameters.
  Size = size(2, [Width, Height]),
  default_border(size(2, [X_Border, Y_Border])),
  VerticeRadius = 0.5,

  % The line for S1.
  Line1X is X_Border,
  Line1Y1 is Y_Border,
  Line1Y2 is Height - Y_Border,
  line([], Line1X, Line1Y1, Line1X, Line1Y2, '', Line1),

  % The line for S2.
  Line2X is Width - X_Border,
  Line2Y1 is Y_Border,
  Line2Y2 is Height - Y_Border,
  line([], Line2X, Line2Y1, Line2X, Line2Y2, '', Line2),

  length(S1, LengthS1),
  Distance1 is (Height - 2 * Y_Border) / (LengthS1 - 1),
  findall(
    S1Circle,
    (
      nth0(I1, S1, _V1),
      Y1 is Y_Border + Distance1 * I1,
      circle([], Line1X, Y1, VerticeRadius, '', S1Circle)
    ),
    S1Circles
  ),

  length(S2, LengthS2),
  Distance2 is (Height - 2 * Y_Border) / (LengthS2 - 1),
  findall(
    S2Circle,
    (
      nth0(I2, S2, _V2),
      Y2 is Y_Border + Distance2 * I2,
      circle([], Line2X, Y2, VerticeRadius, '', S2Circle)
    ),
    S2Circles
  ),

  edges1(UGraph, EG),
  findall(
    EdgeLine,
    (
      nth0(I1, S1, Vertice1),
      member(Vertice1-Vertice2, EG),
      nth0(I2, S2, Vertice2),
      Y1 is Y_Border + Distance1 * I1,
      Y2 is Y_Border + Distance2 * I2,
      line([], Line1X, Y1, Line2X, Y2, '', EdgeLine)
    ),
    EdgeLines
  ),

  append([EdgeLines, S1Circles, S2Circles], Shapes),
  root_element(svg, SVG_Head, [Line1, Line2 | Shapes], SVG_Root),
  term_to_atom(UGraph, AtomicG),
  !.
bipartite_web(UGraph, Markup):-
  error_web('Graph ~w is not bipartite.', [UGraph], Markup).

