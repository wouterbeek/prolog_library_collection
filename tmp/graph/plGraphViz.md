plGraphViz
==========

This library allows you to easily export graphs represented as Prolog terms
using [GraphViz](http://www.graphviz.org/), an advanced graph drawing library.
The Prolog terms have the following form:

~~~prolog
graph(Vertices, Edges, GraphAttrs)
~~~

`Vertices` and `Edges` are lists of compound terms of the following form:

~~~prolog
vertex(Id, VertexAttrs)
edge(FromId, ToId, EdgeAttrs)
~~~

`Id` identifies a vertex and may or may not occur in any of the edges
(i.e., unconnected vertices are allowed).
`FromId` and `ToId` may occur in the list of vertices,
in order to draw an edge between vertices with set attributes.

Attributes have the form `Name=Value`.
`GraphAttrs` are attributes of the graph.
`VertexAttrs` are attributes of the vertex.
`EdgeAttrs` are attributes of the edge.

Attribute values are given as Prolog terms as well,
and are type-checked before exporting.
Many of the GraphViz attributes are supported.
New ones are added on an as-needed bases
(open an issue on Github if you want to see a specific feature added!).
HTML-like labels are supported, allowing complex tables to be shown
inside vertices.

---

### Installation

~~~shell
$ git clone https://github.com/wouterbeek/plGraphViz.git
$ cd plGraphViz
$ git submodule update --init
~~~

### Example

~~~prolog
$ swipl run.pl
?- export_graph_to_gv_file(
     graph([vertex(1,[]),vertex(2,[])],[edge(1,2,[])],[]),
     File,
     [method(sfdp),output(png)]
   ).
   File = 'PATH/plGraphViz/data/tmp.png'
~~~

![](https://raw.githubusercontent.com/wouterbeek/plGraphViz/master/example1.png "Example graph.")

The graphic can be saved to a different file by instantiating
the `File` argument.

---

### 'Method' option

Option `method(+atom)` sets the drawing method that is used by GraphViz
 to place the vertices and edges.
The following values are supported.

| **Value**       | **Description**         |
| `circo`         | Circular layout.        |
| `dot` (default) | Directed graph.         |
| `fdp`           | Undirected graph.       |
| `neato`         | Undirected graph.       |
| `osage`         | Tree map.               |
| `sfdp`          | Large undirected graph. |
| `twopi`         | Radical layouts.        |

---

### 'Output' option

Option `output(+atom)` sets the type of file the graph is written to.
The following values are supported.

| **Value**             | **Description**                       |
|:---------------------:|:--------------------------------------|
| `bmp`                 | Windows Bitmap Format                 |
| `canon`               |                                       |
| `dot`                 |                                       |
| `gv`,  `xdot`, `xdot1.2`, `xdot1.4` | DOT                     |
| `cgimage`             | CGImage bitmap format                 |
| `cmap`                | Client-side imagemap (deprecated)     |
| `eps`                 | Encapsulated PostScript               |
| `exr`                 | OpenEXR                               |
| `fig`                 |                                       |
| `gd`, `gd2`           | GD/GD2 formats                        |
| `gif`                 |                                       |
| `gtk`                 | GTK canvas                            |
| `ico`                 | Icon Image File Format                |
| `imap`                |                                       |
| `cmapx`               | Server-side and client-side imagemaps |
| `imap_np`, `cmapx_np` | Server-side and client-side imagemaps |
| `ismap`               | Server-side imagemap (deprecated)     |
| `jp2`                 | JPEG 2000                             |
| `jpg`, `jpeg`, `jpe`  | JPEG                                  |
| `pct`, `pict`         | PICT                                  |
| `pdf` (default)       | Portable Document Format (PDF)        |
| `pic`                 | Kernighan's PIC graphics language     |
| `plain`, `plain-ext`  | Simple text format                    |
| `png`                 | Portable Network Graphics format      |
| `pov`                 | POV-Ray markup language (prototype)   |
| `ps`                  | PostScript                            |
| `ps2`                 | PostScript for PDF                    |
| `psd`                 | PSD                                   |
| `sgi`                 | SGI                                   |
| `svg`, `svgz`         | Scalable Vector Graphics              |
| `tga`                 | Truevision TGA                        |
| `tif`, `tiff`         | TIFF (Tag Image File Format)          |
| `tk`                  | TK graphics                           |
| `vml`, `vmlz`         | Vector Markup Language (VML)          |
| `vrml`                | VRML                                  |
| `wbmp`                | Wireless BitMap format                |
| `webp`                | Image format for the Web              |
| `xlib`, `x11`         | Xlib canvas                           |

---

### HTML-like labels

Example of using HTML-like labels:

~~~prolog
export_graph_to_gv_file(
  graph(
    [vertex(1,[]),vertex(2,[label=html(table([tr([td(a),td(b)]),tr([td(c),td(d)])]))])],
    [edge(1,2,[label='From 1 to 2.'])],
    []
  ),
  File,
  []
).
~~~

![](https://raw.githubusercontent.com/wouterbeek/plGraphViz/master/example2.png "Example graph with HTML-like labels.")

---

Developed during 2013-2014 by [Wouter Beek](http://www.wouterbeek.com).
