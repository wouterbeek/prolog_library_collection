:- module(
  xml_pi,
  [
    'PI'//2 % ?Version:oneof(['1.0','1.1'])
            % ?ProcessingInstruction:compound
  ]
).

/** <module> Processing Instructions

Grammar for XML PIs.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml_name_token)).
:- use_module(library(xml/xml10_code)).





%! 'PI'(
%!  ?Version:oneof(['1.0','1.1']),
%!  ?ProcessingInstruction:compound
%! )// .
% # Syntax
%
% ```bnf
% PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
% ```
%
% # Semantics
%
% Processing instructions (PIs) allow documents to contain instructions
%  for applications.
%
% @arg Target Identifies the application to which the instruction is directed.
% @arg
%
% @compat XML 1.0.5 [16].
% @compat XML 1.1.2 [16].

'PI'(Version, pi(Target,Codes)) -->
  "<?",
  'PITarget'(Target),
  (   'S',
      '*'('PI0'(Version), Codes, [])
  ;   {Codes = []}
  ),
  "?>".

'PI0'(_, _) --> "?>", !, {false}.
'PI0'(Version, Code) --> 'Char'(Version, Code).



%! 'PITarget'(?Name:atom)// .
% # Syntax
%
% ```bnf
% PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
% ```
%
% Target names `XML`, `xml`, and so on are reserved for standardization
%  in this or future versions of this specification.
%
% @compat XML 1.0.5 [17].
% @compat XML 1.1.2 [17].

'PITarget'(_) -->
  atom_ci(xml), !,
  {false}.
'PITarget'(Name) -->
  'Name'(Name).
