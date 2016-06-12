:- module(
  xml_conditional,
  [
    conditionalSect//2 % ?Version:oneof(['1.0','1.1'])
                       % ?Conditional:compound
  ]
).

/** <module> XML: Conditional Section

**Conditional sections** are portions of the document type declaration
 external subset or of external parameter entities which are included in,
 or excluded from, the logical structure of the DTD based on the keyword
 which governs them.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11
*/

:- use_module(library(lists)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml_external_subset)).
:- use_module(library(xml/xml10_code)).





%! conditionalSect(?Version:oneof(['1.0','1.1']), ?Conditional:compound)// .
% ```bnf
% conditionalSect ::= includeSect | ignoreSect
% ```
%
% @compat XML 1.1.2 [61]

conditionalSect(Version, Conditional) -->
  includeSect(Version, Conditional).
conditionalSect(Version, Conditional) -->
  ignoreSect(Version, Conditional).



%! includeSect(?Version:oneof(['1.0','1.1']), ?IncludeSection:compound)// .
% ```bnf
% includeSect ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
% ```
%
% @compat XML 1.1.2 [62]
% @tbd [VC: Proper Conditional Section/PE Nesting]

includeSect(Version, include(Declarations)) -->
  "<![",
  ?('S'),
  "INCLUDE",
  ?('S'),
  "[",
  extSubsetDecl(Version, Declarations),
  "]]>".



%! ignoreSect(?Version:oneof(['1.0','1.1']), -IgnoreConditional:compound)// .
% ```bnf
% ignoreSect ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
% ```
%
% @compat XML 1.1.2 [63]
% @tbd [VC: Proper Conditional Section/PE Nesting]

ignoreSect(Version, ignore(Names)) -->
  "<![",
  ?('S'),
  "IGNORE",
  ?('S'),
  "[",
  '*'(ignoreSectContents(Version, Names), []),
  "]]>".



%! ignoreSectContents(?Version:oneof(['1.0','1.1']), -Names:list(atom))// .
% ```bnf
% ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
% ```
%
% @compat XML 1.1.2 [64]

ignoreSectContents(Version, [H|T]) -->
  'Ignore'(Version, H),
  ignoreSectContents0(Version, Ls),
  {append(Ls, T)}.

ignoreSectContents0(Version, [H2|T]) -->
  "<![",
  ignoreSectContents(Version, H1),
  "]]>",
  'Ignore'(Version, X),
  {append(H1, [X], H2)},
  ignoreSectContents0(Version, T).
ignoreSectContents0(_, []) --> "".



% 'Ignore'(?Version:oneof(['1.0','1.1']), ?Name:atom)// .
% ```bnf
% Ignore ::= Char* - (Char* ('<![' | ']]>') Char*) 
% ```
%
% @compat XML 1.1.2 [65]

'Ignore'(Version, Name) -->
  '*'('Ignore_char'(Version), Name, [codes_atom]).

'Ignore_char'(_, _) --> "<![", !, {false}.
'Ignore_char'(_, _) --> "]]>", !, {false}.
'Ignore_char'(Version, Code) --> 'Char'(Version, Code).
