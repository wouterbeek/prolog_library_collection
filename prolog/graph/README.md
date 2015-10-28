plGraph
=======

A graph support library for Prolog that uses the S-representation.



S-representation
----------------

[1] shows an example of a directed graph in S-representation.

```prolog
[1]   [1-[2],2-[1,3],3-[]]
```

Directed graphs can be notated in S-representation as well.
It depends on the context whether the S-representation in [2] denotes
a directed or an undirected graph.

```prolog
[2]   [1-[2],2-[1,3],3-[2]]
```



Graph representation primitives
-------------------------------

Each graph representation has to implement the following predicates:
  - `AddEs_3`
  - `AddVs_3`
  - `DelEs_3`
  - `DelVs_3`
  - `Es2Vs`
  - `G2E_2`
  - `G2E_3`
  - `G2E_4`
  - `G2Es_2`
  - `G2V_2`
  - `G2Vs_2`
  - `GComps_3`
  - `V2N_3`
  - `V2Ns_3`



Abbreviations
-------------

Especially when it comes to argument names, this library uses the following
abbreviations for often-occurring concepts from graph theory:
   - `AddEs_3`, Function adding edges to a graph
   - `AddVs_3`, Function adding vertices to a graph
  - `DelEs_3`, Function deleting edges from a graph
  - `DelVs_3`, Function deleting vertices from a graph
  - `E`, Edge
  - `Es2Vs`, Function mapping edges to vertices appearing in those edges
  - `G`, Graph
  - `G2Es_2`, Function mapping graphs to all their edges
  - `G2V_2`, Function mapping graphs to their vertices
  - `G2Vs_2`, Function mapping graphs to all their vertices
  - `GComps`, To/from mapping relating graphs to their edges and vertices
  - `Lbl`, Label of a labeled edge
  - `SubG`, Subgraph
  - `V`, Vertex
  - `V2N_3`, Function mapping vertices to their neighbors
  - `V2Ns_3`, Function mapping vertices to all their neighbors
  - `W`, Vertex



---

Developed in 2014 by [Wouter Beek](http://www.wouterbeek.com),
