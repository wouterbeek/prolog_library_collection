:- module(
  xml_element_type_decl,
  [
    elementdecl//1 % ?ElementDeclaration:compound
  ]
).

/** <module> XML: Element type declaration

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml_name_token)).
:- use_module(library(xml/xml10_code)).





%! children(?ContentParticleOccurrence:compound)// .
% ```bnf
% children ::= (choice | seq) ('?' | '*' | '+')?
% ```
%
% @compat XML 1.1.2 [47]

children(cp(Cp,Occurrence)) -->
  (   choice(Cp)
  ;   seq(Cp)
  ),
  xml_occurrence(Occurrence).



%! choice(?ChoiceOfContentParticles:compound)// .
% Choice list of content particles.
%
% ```bnf
% choice ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
% ```
%
% @compat XML 1.1.2 [49]
% @tbd [VC: Proper Group/PE Nesting]

choice(choice(Cps)) -->
  "(",
    ?('S'),
    'm*'(2, cp(Cps), [separator(xml_pipe)]),
    ?('S'),
  ")".



%! cp(?ContentParticleOccurrence:compound)// .
% **Content particle**, consisting of:
%   - names
%   - choice lists of content particles
%   - sequence lists of content particles
%
% ```bnf
% cp ::= (Name | choice | seq) ('?' | '*' | '+')?
% ```
%
% @compat XML 1.1.2 [48]

cp(cp(Cp,Occurrence)) -->
  (   'Name'(Cp)
  ;   choice(Cp)
  ;   seq(Cp)
  ),
  xml_occurrence(Occurrence).



%! contentspec(?ContentSpec:compound)// .
% ```bnf
% contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
% ```
%
% @compat XML 1.1.2 [46]

contentspec(empty) --> "EMPTY".
contentspec(any) --> "ANY".
contentspec(Names) --> 'Mixed'(Names).
contentspec(CpOccurrence) --> children(CpOccurrence).



%! elementdecl(?ElementDeclaration:compound)// .
% ```bnf
% elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
% ```
%
% @compat XML 1.1.2 [45]
% @tbd [VC: Unique Element Type Declaration]

elementdecl(element_decl(Name,ContentSpec)) -->
  "<!ELEMENT",
  'S',
  'Name'(Name),
  'S',
  contentspec(ContentSpec),
  ?('S'),
  ">".



%! 'Mixed'(?Names:list(atom))// .
% An element type has **mixed content** when elements of that type may
%  contain character data, optionally interspersed with child elements.
%
% In this case, the types of the child elements may be constrained,
%  but not their order or their number of occurrences
%
% ```bnf
% Mixed ::=   '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
%           | '(' S? '#PCDATA' S? ')'
% ```
%
% @compat XML 1.1.2 [51]
% @tbd [VC: Proper Group/PE Nesting]
% @tbd [VC: No Duplicate Types]

'Mixed'(Names) -->
  "(",
  ?('S'),
  "#PCDATA",
  xml_pipe,
  '*'('Name', Names, [separator(xml_pipe)]),
  ?('S'),
  ")*".
'Mixed'([]) -->
  "(",
  ?('S'),
  "#PCDATA",
  ?('S'),
  ")".



%! seq(?SequenceOfContentParticles:compound)// .
% ```bnf
% seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'
% ```
%
% @compat XML 1.1.2 [50]
% @tbd [VC: Proper Group/PE Nesting]

seq(seq(Cps)) -->
  "(",
  ?('S'),
  '*'(cp, Cps, [separator(xml_comma)]),
  ?('S'),
  ")".

xml_comma -->
  ?('S'),
  ",",
  ?('S').


xml_occurrence(0-1) --> "?".
xml_occurrence(0-inf) --> "*".
xml_occurrence(1-inf) --> "+".
xml_occurrence(1-1) --> "".
