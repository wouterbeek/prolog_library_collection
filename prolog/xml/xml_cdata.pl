:- module(
  xml_cdata,
  [
    'CDSect'//2 % ?Version:oneof(['1.0','1.1'])
                % ?CharacterData:atom
  ]
).

/** <module> CDATA

Grammar for CDATA.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml10_code)).





%! 'CDSect'(?Version:oneof(['1.0','1.1']), ?CharacterData:atom)// .
% **Character Data**.
%
% # Syntax
%
% ```bnf
% CDSect ::= CDStart CData CDEnd
% ```
%
% CDATA sections may occur anywhere character data may occur;
%  they are used to escape blocks of text containing characters which would
%  otherwise be recognized as markup.
%
% ### Example
%
% ```xml
% <![CDATA[<greeting>Hello, world!</greeting>]]>
% ```
%
% @compat XML 1.0.5 [18].
% @compat XML 1.1.2 [18].

'CDSect'(Version, Data) -->
  'CDStart',
  dcg_atom('CData'(Version), Data),
  'CDEnd'.



%! 'CDStart'// .
% ```bnf
% CDStart ::= '<![CDATA['
% ```
%
% @compat XML 1.0.5 [19].
% @compat XML 1.1.2 [19].

'CDStart' --> "<![CDATA[".



%! 'CData'(?Codes:list(code))// .
% ```bnf
% CData ::= (Char* - (Char* ']]>' Char*))
% ```
%
% @compat XML 1.0.5 [20].
% @compat XML 1.1.2 [20].

'CData'(_, _) -->
  'CDEnd', !,
  {false}.
'CData'(Version, [H|T]) -->
  'Char'(Version, H),
  'CData'(Version, T).
'CData'(_, []) --> "".



%! 'CDEnd'// .
% ```bnf
% CDEnd ::= ']]>'
% ```
%
% @compat XML 1.0.5 [21].
% @compat XML 1.1.2 [21].

'CDEnd' --> "]]>".
