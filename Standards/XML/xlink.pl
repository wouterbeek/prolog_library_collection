:- module(
  xlink,
  [
  ]
).

/** <module> XLINK

Support for the XML Linking Language.

Create and describe links between resources from within XML documents.

#

  * Link
    An explicit relationship between (portions of) resources.
  * Linking element
    An XLink-conforming XML element that asserts the existence of a link.
  * Resource
    Any addressable unit of information or service.
  * Hyperlink
    A link that is intended primarily for human presentation.
  * Traversal
    Using or following a link for any purpose.
  * Starting resource
    The source from which traversal is begun.
  * Ending resource
    The destination where traversal ends.
  * Arc
    Information about how to traverse a pair or resources
    (e.g. direction of traversal, application behavior information).
  * Local resource
  * Remote resource
  * Linkbases
  * Extended link
    A link that associates an arbitrary number of resources.

# Simple link

~~~{.dtd}
<ELEMENT xlink:type="simple" xlink:href="URI">CONTENT</ELEMENT>
~~~

@author Wouter Beek
@version 2013/05
*/

:- use_module(xml(xml_namespace)).

:- xml_register_namespace(xlink, 'http://www.w3.org/1999/xlink').




