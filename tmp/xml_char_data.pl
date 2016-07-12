:- module(
  xml_char_data,
  [
    'CharData'//1 % ?Data:atom
  ]
).

/** <module> XML character data

**Text** consists of intermingled character data and markup.

**Markup** takes the form of:
  - start-tags
  - end-tags
  - empty-element tags
  - entity references
  - character references
  - comments
  - CDATA section delimiters
  - document type declarations
  - processing instructions
  - XML declarations
  - text declarations
  - white space that is at the top level of the document entity
    (that is, outside the document element and not inside any other markup)

All text that is not markup constitutes the **character data** of the document.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2015/07, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).





%! 'CharData'(?Data:atom)// .
% ```bnf
% CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
% ```
%
% @compat XML 1.0.5 [14].
% @compat XML 1.1.2 [14].

'CharData'(Data) -->
  dcg_atom('*'('CharData0', []), Data).

'CharData0'(_) --> "<", !, {false}.
'CharData0'(_) --> "&", !, {false}.
'CharData0'(_) --> "]]>", !, {false}.
'CharData0'(Code) --> [Code].
