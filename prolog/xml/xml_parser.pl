:- module(
  xml_parser,
  [
    'AttlistDecl'//2,      % ?Version, ?AttributeDeclaration
    'AttValue'//2,         % ?Version, ?Value
    'CDSect'//2,           % ?Version, ?CharacterData
    'Char'//2,             % +Version, ?Code
    'CharData'//1,         % ?Data
    'CharRef'//2,          % ?Version, ?Code
    'Comment'//1,          % +Version
    conditionalSect//2,    % ?Version, ?Conditional
    content//2,            % ?Version, ?Content
    'DeclSep'//1,          % ?Name:atom
    document//5,           % ?Version, ?XmlDeclaration, ?DoctypeDeclaration
                           % -ProcessingInstructions, ?RootElement
    element//2,            % ?Version, ?Element
    elementdecl//1,        % ?ElementDeclaration
    'EmptyElemTag'//3,     % ?Version, ?Name, ?Attributes
    'EncodingDecl'//1,     % -Encoding
    'EntityDecl'//2,       % ?Version, ?EntityDeclaration
    'EntityDef'//2,        % ?Version, ?EntityDefinition
    'EntityValue'//2,      % ?Version, ?Names
    'Eq'//0,
    'ETag@xml'//1,         % ?Name
    'ExternalID'//1,       % ?ExternalId
    extParsedEnt//3,       % ?Version, ?Encoding, ?Content
    extSubsetDecl//2,      % ?Version, ?Declarations
    markupdecl//2,         % ?Version, ?Declaration
    'Misc'//2,             % -Version, -PIs
    'Name'//1,             % -Name
    'Names'//1,            % ?Names
    'Nmtoken'//1,          % ?Token
    'Nmtokens'//1,         % ?Tokens
    'PEDef'//2,            % ?Version, ?ParameterEntityDefinition
    'PEReference'//1,      % -Ref
    'PI'//2,               % +Version, -PI
    'PubidLiteral'//1,     % ?Literal
    'NotationDecl'//1,     % ?NotationDeclaration
    'Reference'//2,        % ?Version, ?Reference
    'RestrictedChar11'//1, % ?Code
    'S'//0,
    'SDDecl'//1,           % -Standalone
    'STag'//3,             % ?Version, ?Name, ?Attributes
    'SystemLiteral'//1,    % ?Literal
    'TextDecl'//2,         % ?Version, ?Encoding
    'VersionInfo'//1,      % -Version
    'XMLDecl'//3           % -Version, -Encoding, -Standalone
  ]
).

/** <module> XML parser

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2017/06, 2017/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(math_ext)).

:- meta_predicate
    quoted(//, ?, ?),
    quoted(?, //, ?, ?).





%! 'AttDef'(?Version:compound, ?AttributeDefinition:compound)// .
%
% ```bnf
% AttDef ::= S Name S AttType S DefaultDecl
% ```
%
% @compat XML 1.1.2 [53]

'AttDef'(Version, attr_def(AttributeName,Type,Default)) -->
  'S',
  'Name'(AttributeName),
  'S',
  'AttType'(Type),
  'S',
  'DefaultDecl'(Version, Default).



%! 'AttlistDecl'(?Version:compound, ?AttributeDeclaration:compound)// .
%
% Defines the attributes of an element type.
%
% ```bnf
% AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
% ```
%
% @compat XML 1.1.2 [52]

'AttlistDecl'(Version, attr_decl(ElementType,AttrDefs)) -->
  "<!ATTLIST",
  'S',
  'Name'(ElementType),
  *('AttDef'(Version), AttrDefs),
  ?('S'),
  ">".



%! 'Attribute'(?Version:compound, ?Attribute:pair(atom,list(atom)))// .
%
% ```bnf
% Attribute ::= Name Eq AttValue
% ```
%
% @compat XML 1.0.5 [41].
% @compat XML 1.1.2 [41].
% @tbd [VC: Attribute Value Type]
% @tbd [WFC: No External Entity References]
% @tbd [WFC: No < in Attribute Values]

'Attribute'(Version, Name-Values) -->
  'Name'(Name),
  'Eq',
  'AttValue'(Version, Values).



%! 'AttType'(?Type)// .
%
% ```bnf
% AttType ::= StringType | TokenizedType | EnumeratedType
% ```
%
% @compat XML 1.1.2 [54]

'AttType'(Type) -->
  'StringType'(Type).
'AttType'(Type) -->
  'TokenizedType'(Type).
'AttType'(Type) -->
  'EnumeratedType'(Type).



%! 'AttValue'(?Version:compound, ?Value:list(atom))// .
%
% ```bnf
% AttValue ::= '"' ([^<&"] | Reference)* '"'
%            | "'" ([^<&'] | Reference)* "'"
% ```
%
% @compat XML 1.0.5 [10].
% @compat XML 1.1.2 [10].

'AttValue'(Version, Names) -->
  quoted(Quote, *('AttValue_'(Version, Quote), Names)).

'AttValue_'(Version, Quote, Name) -->
  'Reference'(Version, Name),
  {
    (   Quote == "\""
    ->  \+ sub_atom(Name, _, 1, _, '"')
    ;   Quote == "'"
    ->  \+ sub_atom(Name, _, 1, _, '\'')
    ),
    \+ sub_atom(Name,_, 1, _, '<'),
    \+ sub_atom(Name,_, 1, _, '&')
  }.



%! 'CData'(?Codes:list(code))// .
%
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
%
% ```bnf
% CDEnd ::= ']]>'
% ```
%
% @compat XML 1.0.5 [21].
% @compat XML 1.1.2 [21].

'CDEnd' --> "]]>".



%! 'CDSect'(?Version:compound, ?CharacterData:atom)// .
%
% *Character Data*
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
%
% ```bnf
% CDStart ::= '<![CDATA['
% ```
%
% @compat XML 1.0.5 [19].
% @compat XML 1.1.2 [19].

'CDStart' --> "<![CDATA[".



%! 'Char'(+Version:compound, ?Code:code)// .
%
% An **XML Character** is an atomic unit of text specified by ISO/IEC
% 10646.
%
% # XML 1.0
%
% ```ebnf
% Char ::= #x9                // Horizontal tab
%        | #xA                // Line feed
%        | #xD                // Carriage return
%        | [#x20-#xD7FF]      // Space, punctuation, numbers, letters
%        | [#xE000-#xFFFD]
%        | [#x10000-#x10FFFF]
% ```
%
% Avoid comapatibility characters [Unicode, section 2.3].  Avoid the
% following characters (control characters, permanently undefined
% Unicode characters):
%
% ```
% [#x7F-#x84] // Delete, ...
% [#x86-#x9F]
% [#xFDD0-#xFDEF],
% [#x1FFFE-#x1FFFF]
% [#x2FFFE-#x2FFFF]
% [#x3FFFE-#x3FFFF]
% [#x4FFFE-#x4FFFF]
% [#x5FFFE-#x5FFFF]
% [#x6FFFE-#x6FFFF]
% [#x7FFFE-#x7FFFF]
% [#x8FFFE-#x8FFFF]
% [#x9FFFE-#x9FFFF]
% [#xAFFFE-#xAFFFF]
% [#xBFFFE-#xBFFFF]
% [#xCFFFE-#xCFFFF]
% [#xDFFFE-#xDFFFF]
% [#xEFFFE-#xEFFFF]
% [#xFFFFE-#xFFFFF]
% [#x10FFFE-#x10FFFF]
% ```
%
% # XML 1.1
%
% ```ebnf
% Char ::= [#x1-#xD7FF]
%        | [#xE000-#xFFFD]
%        | [#x10000-#x10FFFF]
% /* any Unicode character, excluding the surrogate blocks,
%    FFFE, and FFFF. */
% ```
%
% @arg Version is either `version(1,0)' for XML 1.0 or `version(1,1)'
%      for XML 1.1.

'Char'(version(1,0), 0x9) --> [0x9].
'Char'(version(1,0), 0xA) --> [0xA].
'Char'(version(1,0), 0xD) --> [0xD].
'Char'(version(1,0), Code) -->
  [Code],
  {(  between(0x20, 0xD7FF, Code)
  ;   between(0xE000, 0xFFFD, Code)
  ;   between(0x10000, 0x10FFFF, Code)
  )}.
'Char'(version(1,1), Code) -->
  [Code],
  {(  between(0x1, 0xD7FF, Code)
  ;   between(0xE000, 0xFFFD, Code)
  ;   between(0x10000, 0x10FFFF, Code)
  )}.



%! 'CharData'(?Data:atom)// .
%
% ```bnf
% CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
% ```
%
% @compat XML 1.0.5 [14].
% @compat XML 1.1.2 [14].

'CharData'(Data) -->
  dcg_atom(*('CharData_'), Data).

'CharData_'(_) --> "<",   !, {false}.
'CharData_'(_) --> "&",   !, {false}.
'CharData_'(_) --> "]]>", !, {false}.
'CharData_'(C) --> [C].



%! 'CharRef'(?Version, ?Code:code)// .
%
% *Character Reference*
%
% # Syntax
%
% ```abnf
%   CharRef ::= '&#' [0-9]+ ';'
%             | '&#x' [0-9a-fA-F]+ ';'
% ```
%
% ## Well-formedness constraint: Legal Character
%
% Characters referred to using character references MUST match
% the production for Char//2.
%
% If the character reference begins with `&#x', the digits and letters
% up to the terminating `;' provide a hexadecimal representation of
% the character's code point in ISO/IEC 10646.  If it begins just with
% `&#', the digits up to the terminating `;' provide a decimal
% representation of the character's code point.
%
% @compat XML 1.0.5 [66]
% @compat XML 1.1.2 [66]

'CharRef'(Version, Code) -->
  "&#",
  ("x" -> +(xdigit, Weights) ; +(digit, Weights)),
  {integer_weights(Code, Weights)},
  ";",
  % Only characters that match 'Char'//2 are allowed.
  {'Char'(Version, Code, _, _)}.



%! children(?ContentParticleOccurrence:compound)// .
%
% ```bnf
% children ::= (choice | seq) ('?' | '*' | '+')?
% ```
%
% @compat XML 1.1.2 [47]

children(cp(Cp,Occurrence)) -->
  (choice(Cp) ; seq(Cp)),
  kleene(Occurrence).



%! choice(?ChoiceOfContentParticles:compound)// .
%
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
  +&(cp(Cps), pipe_sep),
  ?('S'),
  ")".



%! 'Comment'(+Version:compound)// .
%
% # Syntax
%
% ```abnf
% Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
% ```
%
% Comments may appear anywhere in a document outside other markup; in
% addition, they may appear within the document type declaration at
% places allowed by the grammar.
%
% For compatibility, the string `--` (double-hyphen) MUST NOT occur
% within comments.
%
% Parameter entity references MUST NOT be recognized within comments.
%
% Note that the grammar does not allow a comment ending in `--->`.
%
% # Example
%
% ```xml
% <!-- declarations for <head> & <body> -->
% ```
%
% @arg Version is either `version(1,0)' for XML 1.0 or `version(1,1)'
%      for XML 1.1.
%
% @compat XML 1.0.5 [15].
% @compat XML 1.1.2 [15].

'Comment'(Version) -->
  "<!--",
  *(comment_char(Version)),
  "-->".

comment_char(_) -->
  "--", !,
  {false}.
comment_char(Version) -->
  'Char'(Version, _).



%! conditionalSect(?Version:compound, ?Conditional:compound)// .
%
% *Conditional sections* are portions of the document type declaration
% external subset or of external parameter entities which are included
% in, or excluded from, the logical structure of the DTD based on the
% keyword which governs them.
%
% ```bnf
% conditionalSect ::= includeSect | ignoreSect
% ```
%
% @compat XML 1.1.2 [61]

conditionalSect(Version, Conditional) -->
  includeSect(Version, Conditional).
conditionalSect(Version, Conditional) -->
  ignoreSect(Version, Conditional).



%! content(?Version:compound, ?Content:list(compound))// .
%
% ```bnf
% content ::= CharData?
%             (
%               (element | Reference | CDSect | PI | Comment)
%               CharData?
%!            )*
% ```
%
% @compat XML 1.0.5 [43].
% @compat XML 1.1.2 [43].

content(Version, L) -->
  ('CharData'(H) -> {L = [H|T]} ; {L = T}),
  *(content_(Version), T).

content_(Version, H) -->
  content__(Version, H),
  ?('CharData'(_)).

content__(Version, H) -->
  element(Version, H).
content__(Version, H) -->
  'Reference'(Version, H).
content__(Version, H) -->
  'CDSect'(Version, H).
content__(Version, H) -->
  'PI'(Version, H).
content__(Version, _) -->
  'Comment'(Version).



%! contentspec(?ContentSpec:compound)// .
%
% ```bnf
% contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
% ```
%
% @compat XML 1.1.2 [46]

contentspec(empty) -->
  "EMPTY".
contentspec(any) -->
  "ANY".
contentspec(Names) -->
  'Mixed'(Names).
contentspec(CpOccurrence) -->
  children(CpOccurrence).



%! cp(?ContentParticleOccurrence:compound)// .
%
% *Content particle*, consisting of:
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
  ('Name'(Cp) ; choice(Cp) ; seq(Cp)),
  kleene(Occurrence).



%! 'DeclSep'(-Name)// .
%
% ```bnf
% DeclSep ::= PEReference | S
% ```
%
% @compat XML 1.0.5 [28a].
% @compat XML 1.1.2 [28a].
% @tbd [WFC: PE Between Declarations]

'DeclSep'(Ref) -->
  'PEReference'(Ref).
'DeclSep'(_) -->
  'S'.



%! 'DefaultDecl'(?Version:compound, ?DefaultValue:compound)// .
%
% ```bnf
% DefaultDecl ::= '#REQUIRED'
%               | '#IMPLIED'
%               | (('#FIXED' S)? AttValue)
% ```
%
% @compat XML 1.1.2 [60]
% @tbd [VC: Required Attribute]
% @tbd [VC: Attribute Default Value Syntactically Correct]
% @tbd [WFC: No < in Attribute Values]
% @tbd [VC: Fixed Attribute Default]
% @tbd [WFC: No External Entity References]

'DefaultDecl'(_, required) -->
  "#REQUIRED".
'DefaultDecl'(_, implied) -->
  "#IMPLIED".
'DefaultDecl'(Version, fixed(Value)) -->
  ("#FIXED" -> 'S' ; ""),
  'AttValue'(Version, Value).



%! doctypedecl(+Version:compound, -Decl)// .
%
% ```bnf
% doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S?
%                 ('[' intSubset ']' S?)? '>'
% ```
%
% @arg Version is either `version(1,0)' for XML 1.0 or `version(1,1)'
%      for XML 1.1.
%
% @compat XML 1.0.5 [28].
% @compat XML 1.1.2 [28].
% @tbd [VC: Root Element Type]
% @tbd [WFC: External Subset]

doctypedecl(Version, doctype_decl(Name,ExtId,Decls)) -->
  "<!DOCTYPE",
  'S',
  'Name'(Name),
  ('S' -> 'ExternalID'(ExtId) ; ""),
  ?('S'),
  (   "["
  ->  intSubset(Version, Decls),
      "]",
      ?('S')
  ;   {Decls = []}
  ).



%! document(?Version:compound, ?XmlDeclaration:compound,
%!          ?DoctypeDeclaration:compound,
%!          -ProcessingInstructions:list(compound),
%!          ?RootElement:compound)// .
%
% # Syntax
%
% XML documents SHOULD begin with an XML declaration which specifies
%  the version of XML being used.
%
% ## XML 1.0.5
%
% ```bnf
% document ::= prolog element Misc*
% ```
%
% ## XML 1.1.2
%
% ```bnf
% document ::= ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
% ```
%
% ### Example
%
% ```xml
% <?xml version="1.0"?>
% <greeting>Hello, world!</greeting>
% ```
%
% @compat XML 1.0.5 [1].
% @compat XML 1.1.2 [1].

document(version(1,0), XmlDecl, DoctypeDecl, Pis, RootElement) -->
  prolog(version(1,0), prolog(XmlDecl,DoctypeDecl,Pis1)),
  element(version(1,0), RootElement),
  *('Misc'(version(1,0)), Pis2),
  {
    exclude(var, Pis2, Pis3),
    append(Pis1, Pis3, Pis)
  }.
document(version(1,1), XmlDecl, DoctypeDecl, Pis, X, Y):-
  document(version(1,0), XmlDecl, DoctypeDecl, Pis, X, Y),
  append(Codes, Y, X),
  \+ ((
    member(Code, Codes),
    'RestrictedChar'(version(1,1), Code, _, _)
  )).



%! element(?Version:compound, ?Element:compound)// .
%
% ```bnf
% element ::= EmptyElemTag
%           | STag content ETag
% ```
%
% @compat XML 1.0.5 [39].
% @compat XML 1.1.2 [39].
% @tbd [WFC: Element Type Match]
% @tbd [VC: Element Valid]

element(Version, element(Name, Attributes)) -->
  'EmptyElemTag'(Version, Name, Attributes).
element(Version, element(Name,Attributes,Content)) -->
  'STag'(Version, Name, Attributes),
  content(Version, Content),
  'ETag@xml'(Name).



%! elementdecl(?ElementDeclaration:compound)// .
%
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



%! 'EmptyElemTag'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?Name,
%!   ?Attributes
%! )// .
%
% An empty XML element.
% ```ebnf
% EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
% ```
%
% @compat XML 1.0.5 [44].
% @compat XML 1.1.2 [44].
% @tbd [WFC: Unique Att Spec]

'EmptyElemTag'(Version, Name, Attributes) -->
  "<",
  'Name'(Name),
  (   'S',
      +&('Attribute'(Version), 'S', Attributes)
  ->  ""
  ;   {Attributes = []}
  ),
  ?('S'),
  "/>".



%! 'EncodingDecl'(-Encoding:atom)// .
%
% ```ebnf
% EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
% ```
%
% @compat XML 1.0.5 [80]
% @compat XML 1.1.2 [80]

'EncodingDecl'(Encoding) -->
  'S',
  "encoding",
  'Eq',
  quoted('EncName'(Encoding)).



%! 'EncName'(-Encoding:atom)// is det.
%
% ```bnf
% EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
% ```
%
% compat XML 1.0.5 [81]
% compat XML 1.1.2 [81]

'EncName'(Encoding) -->
  dcg_atom('EncName_', Encoding).

'EncName_'([H|T]) -->
  alpha(H),
  *(enc_name_char, T).

enc_name_char(C) --> alphanum(C).
enc_name_char(0'.) --> ".".
enc_name_char(0'_) --> "_".
enc_name_char(0'-) --> "-".



%! 'EntityDecl'(?Version:compound, ?EntityDeclaration:compound)// .
%
% ```bnf
%    EntityDecl ::= GEDecl | PEDecl
% ```
%
% @compat XML 1.0.5 [70]
% @compat XML 1.1.2 [70]

'EntityDecl'(Version, GeneralEntityDeclaration) -->
  'GEDecl'(Version, GeneralEntityDeclaration).
'EntityDecl'(Version, ParameterEntityDeclaration) -->
  'PEDecl'(Version, ParameterEntityDeclaration).



%! 'EntityDef'(?Version:compound, ?EntityDefinition:compound)// .
%
% ```bnf
% EntityDef ::= EntityValue | (ExternalID NDataDecl?)
% ```
%
% @compat XML 1.0.5 [73]

'EntityDef'(Version, entity_def(Literal)) -->
  'EntityValue'(Version, Literal).
'EntityDef'(_, entity_def(ExternalId,NotationName)) -->
  'ExternalID'(ExternalId),
  ?('NDataDecl', NotationName).



%! 'EntityRef'(?EntityReference:atom)// .
%
% **Entity Reference**.
%
% ```bnf
% EntityRef ::= '&' Name ';'
% ```
%
% @compat XML 1.0.5 [68].
% @compat XML 1.1.2 [68].
% @tbd [WFC: Entity Declared]
% @tbd [VC: Entity Declared]
% @tbd [WFC: Parsed Entity]
% @tbd [WFC: No Recursion]

'EntityRef'(Name) -->
  "&",
  'Name'(Name),
  ";".



%! 'EntityValue'(?Version:compound, ?References:list(atom))// .
%
% A sequence of references to:
%   - Parameter Entities
%   - Entities
%   - Characters
%
% ```bnf
% EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
%               | "'" ([^%&'] | PEReference | Reference)* "'"
% ```
%
% @compat XML 1.0.5 [9].
% @compat XML 1.1.2 [9].

'EntityValue'(Version, References) -->
  quoted(Quote, *('EntityValue_'(Version, Quote), References)).

'EntityValue_'(Version, Quote, Reference) -->
  (   'PEReference'(Reference)
  ;   'Reference'(Version, Reference)
  ),
  {
    (   Quote == 0'"
    ->  \+ sub_atom(Reference, _, 1, _, '"')
    ;   Quote == 0''
    ->  \+ sub_atom(Reference, _, 1, _, '\'')
    ),
    \+ sub_atom(Reference, _, 1, _, '%'),
    \+ sub_atom(Reference, _, 1, _, '&')
  }.



%! 'EnumeratedType'(?Type:compound)// .
%
% ```bnf
% EnumeratedType ::= NotationType | Enumeration
% ```
%
% @compat XML 1.1.2 [57]

'EnumeratedType'(Type) --> 'NotationType'(Type).
'EnumeratedType'(Type) --> 'Enumeration'(Type).



%! 'Enumeration'(?Type:compound)// .
%
% ```bnf
% Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
% ```
%
% @compat XML 1.1.2 [59]
% @tbd [VC: Enumeration]
% @tbd [VC: No Duplicate Tokens]

'Enumeration'(oneof([Tokens])) -->
  "(",
  ?('S'),
  +&('Nmtoken', Tokens, pipe_sep),
  ?('S'),
  ")".



%! 'Eq'// is det.
%
% ```bnf
% Eq ::= S? '=' S?
% ```
%
% @compat XML 1.0.5 [25].
% @compat XML 1.1.2 [25].

'Eq' -->
  ?('S'),
  "=",
  ?('S').



%! 'ETag@xml'(?Name:atom)// .
%
% The end tag of an XML element.
%
% ```ebnf
% ETag ::= '</' Name S? '>'
% ```
%
% Unfortunate naming conflict with the `ETag` HTTP header.
%
% @compat XML 1.0.5 [42].
% @compat XML 1.1.2 [42].

'ETag@xml'(Name) -->
  "</",
  'Name'(Name),
  ?('S'),
  ">".



%! 'ExternalID'(?ExternalId:compound)// .
%
% ```bnf
% ExternalID ::= 'SYSTEM' S SystemLiteral
%              | 'PUBLIC' S PubidLiteral S SystemLiteral
% ```
%
% @compat XML 1.0.5 [75]
% @compat XML 1.1.2 [75]

'ExternalID'(external_id(system,Literal)) -->
  "SYSTEM",
  'S',
  'SystemLiteral'(Literal).
'ExternalID'(external_id(public,Literal1,Literal2)) -->
  "PUBLIC",
  'S',
  'PubidLiteral'(Literal1),
  'S',
  'SystemLiteral'(Literal2).



%! extParsedEnt(?Version:compound, ?Encoding:atom, ?Content)// .
%
% ```bnf
% extParsedEnt ::= ( TextDecl? content ) - ( Char* RestrictedChar Char* ) 
% ```
%
% @compat XML 1.1.2 [78]

extParsedEnt(version(1,0), Encoding, Content) -->
  ?('TextDecl'(version(1,0)), Encoding),
  content(version(1,0), Content).
extParsedEnt(version(1,1), Encoding, Content, X, Y):-
  extParsedEnt(version(1,0), Encoding, Content, X, Y),
  append(Codes, Y, X),
  \+ ((
    member(Code, Codes),
    'RestrictedChar'(version(1,1), Code, _, _)
  )).



%! extSubset(?Version:compound, ?Encoding:atom,
%!           ?Declarations:list(compound))// .
%
% ```bnf
% extSubset ::= TextDecl? extSubsetDecl
% ```
%
% @compat XML 1.0.5 [30].
% @compat XML 1.1.2 [30].

extSubset(Version, Encoding, Declarations) -->
  ?('TextDecl'(Version), Encoding),
  extSubsetDecl(Version, Declarations).



%! extSubsetDecl(?Version:compound, ?Declarations:list(compound))// .
%
% ```bnf
% extSubsetDecl ::= ( markupdecl | conditionalSect | DeclSep)*
% ```
%
% @compat XML 1.0.5 [31].
% @compat XML 1.1.2 [31].

extSubsetDecl(Version, [H|T]) -->
  markupdecl(Version, H),
  extSubsetDecl(Version, T).
extSubsetDecl(Version, [H|T]) -->
  conditionalSect(Version, H),
  extSubsetDecl(Version, T).
extSubsetDecl(Version, [H|T]) -->
  'DeclSep'(H),
  extSubsetDecl(Version, T).



%! 'GEDecl'(?Version:compound, ?GeneralEntityDeclaration:compound)// .
%
% *General Entity Declaration*
%
% ```bnf
% GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
% ```
%
% @compat XML 1.0.5 [71]

'GEDecl'(Version, general_entity_decl(EntityName,EntityDefinition)) -->
  "<!ENTITY",
  'S',
  'Name'(EntityName),
  'S',
  'EntityDef'(Version, EntityDefinition),
  ?('S'),
  ">".



% 'Ignore'(?Version:compound, ?Name:atom)// .
%
% ```bnf
% Ignore ::= Char* - (Char* ('<![' | ']]>') Char*) 
% ```
%
% @compat XML 1.1.2 [65]

'Ignore'(Version, Name) -->
  dcg_atom(*('Ignore_'(Version)), Name).

'Ignore_'(_, _) -->
  "<![", !,
  {false}.
'Ignore_'(_, _) -->
  "]]>", !,
  {false}.
'Ignore_'(Version, Code) -->
  'Char'(Version, Code).



%! ignoreSect(?Version:compound, -IgnoreConditional:compound)// .
%
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
  *(ignoreSectContents(Version), Names),
  "]]>".



%! ignoreSectContents(?Version:compound, -Names:list(atom))// .
%
% ```bnf
% ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
% ```
%
% @compat XML 1.1.2 [64]

ignoreSectContents(Version, [H|T]) -->
  'Ignore'(Version, H),
  ignoreSectContents_(Version, Ls),
  {append(Ls, T)}.

ignoreSectContents_(Version, [H2|T]) -->
  "<![",
  ignoreSectContents(Version, H1),
  "]]>",
  'Ignore'(Version, X),
  {append(H1, [X], H2)},
  ignoreSectContents_(Version, T).
ignoreSectContents_(_, []) --> "".



%! includeSect(?Version:compound, ?IncludeSection:compound)// .
%
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



%! intSubset(+Version, -Decls)// .
%
% ```bnf
% intSubset ::= (markupdecl | DeclSep)*
% ```
%
% @compat XML 1.0.5 [28b].
% @compat XML 1.1.2 [28b].

intSubset(Version, [H|T]) -->
  (markupdecl(Version, H) -> "" ; 'DeclSep'(H)),
  intSubset(Version, T).
intSubset(_, []) --> "".



%! markupdecl(?Version:compound, ?Declaration:compound)// .
%
% ```bnf
% markupdecl ::=   elementdecl
%                | AttlistDecl
%                | EntityDecl
%                | NotationDecl
%                | PI
%                | Comment
% ```
%
% @compat XML 1.0.5 [29].
% @compat XML 1.1.2 [29].
% @tbd [WFC: PEs in Internal Subset]
% @tbd [VC: Proper Declaration/PE Nesting]

markupdecl(_, ElementDeclaration) -->
  elementdecl(ElementDeclaration).
markupdecl(Version, AttributeDeclaration) -->
  'AttlistDecl'(Version, AttributeDeclaration).
markupdecl(Version, EntityDeclaration) -->
  'EntityDecl'(Version, EntityDeclaration).
markupdecl(_, NotationDeclaration) -->
  'NotationDecl'(NotationDeclaration).
markupdecl(Version, Pi) -->
  'PI'(Version, Pi).
markupdecl(Version, _) -->
  'Comment'(Version).



%! 'Misc'(+Version:compound, -PI)// .
%
% ```ebnf
% Misc ::= Comment | PI | S
% ```
%
% @arg Version is either `version(1,0)' for XML 1.0 or `version(1,1)'
%      for XML 1.1.
%
% @compat XML 1.0.5 [27].
% @compat XML 1.1.2 [27].

'Misc'(Version, _) -->
  'Comment'(Version).
'Misc'(Version, PI) -->
  'PI'(Version, PI).
'Misc'(_, _) -->
  'S'.



%! 'Mixed'(?Names:list(atom))// .
% An element type has **mixed content** when elements of that type may
%  contain character data, optionally interspersed with child elements.
%
% In this case, the types of the child elements may be constrained,
%  but not their order or their number of occurrences
%
% ```bnf
% Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
%         | '(' S? '#PCDATA' S? ')'
% ```
%
% @compat XML 1.1.2 [51]
% @tbd [VC: Proper Group/PE Nesting]
% @tbd [VC: No Duplicate Types]

'Mixed'(Names) -->
  "(",
  ?('S'),
  "#PCDATA",
  (pipe_sep -> +&('Name', pipe_sep, Names) ; {Names = []}),
  ?('S'),
  ")*".
'Mixed'([]) -->
  "(",
  ?('S'),
  "#PCDATA",
  ?('S'),
  ")".



%! 'Name'(?Name:atom)// is det.
%
% An **XML Name** is an *Nmtoken* with a restricted set of initial
% characters.
%
% Disallowed initial characters for names include digits, diacritics,
% the full stop and the hyphen.
%
% ```ebnf
% Name ::= NameStartChar (NameChar)*
% ```
%
% # Reserved names
%
% Names beginning with `(x,m,l)` are reserved for standardization in
% this or future versions of this specification.
%
% # XML Namespaces
%
% The Namespaces in XML Recommendation assigns a meaning to names
% containing colon characters. Therefore, authors should not use the
% colon in XML names except for namespace purposes, but XML processors
% must accept the colon as a name character.
%
% @compat XML 1.0.5 [5].
% @compat XML 1.1.2 [5].

'Name'(Name) -->
  dcg_atom('Name_', Name).

'Name_'([H|T]) -->
  'NameStartChar'(H),
  *('NameChar', T).



%! 'NameChar'(?Code:code)// .
%
% ```ebnf
% NameChar ::= NameStartChar
%            | "-"
%            | "."
%            | [0-9]
%            | #xB7
%            | [#x0300-#x036F]
%            | [#x203F-#x2040]
% ```
%
% @compat XML 1.0.5 [4a].
% @compat XML 1.1.2 [4a].

'NameChar'(Code) -->
  'NameStartChar'(Code).
'NameChar'(0'-) -->
  "-".
'NameChar'(0'.) -->
  ".".
'NameChar'(Code) -->
  digit(Code).
% #x00B7
'NameChar'(0'·) -->
  "·".
% #x0300-#x036F
'NameChar'(Code) -->
  [Code],
  {between(0x300, 0x36F, Code)}.
% #x203F
'NameChar'(0'‿) -->
  "‿".
% #x2040
'NameChar'(0'⁀) -->
  "⁀".



%! 'Names'(?Names:list(atom))// .
%
% ```ebnf
% Names ::= Name (#x20 Name)*
% ```
%
% @compat XML 1.0.5 [6].
% @compat XML 1.1.2 [6].

'Names'(Names) -->
  +&('Name', " ", Names).



%! 'NameStartChar'(?Code:code)// .
%
% ```ebnf
% NameStartChar ::= ":"
%                 | [A-Z]
%                 | "_"
%                 | [a-z]
%                 | [#xC0-#xD6]
%                 | [#xD8-#xF6]
%                 | [#xF8-#x2FF]
%                 | [#x370-#x37D]
%                 | [#x37F-#x1FFF]
%                 | [#x200C-#x200D]
%                 | [#x2070-#x218F]
%                 | [#x2C00-#x2FEF]
%                 | [#x3001-#xD7FF]
%                 | [#xF900-#xFDCF]
%                 | [#xFDF0-#xFFFD]
%                 | [#x10000-#xEFFFF]
% ```
%
% @compat XML 1.0.5 [4].
% @compat XML 1.1.2 [4].

'NameStartChar'(Code) -->
  alpha(Code).
'NameStartChar'(0':) -->
  ":".
'NameStartChar'(0'_) -->
  "_".
% #xC0-#xD6
% #xD8-#xF6
% #xF8-#x2FF
% #x370-#x37D
% #x37F-#x1FFF
% #x200C-#x200D
% #x2070-#x218F
% #x2C00-#x2FEF
% #x3001-#xD7FF
% #xF900-#xFDCF
% #xFDF0-#xFFFD
% #x10000-#xEFFFF
'NameStartChar'(Code) -->
  [Code],
  {(  between(0xC0, 0xD6, Code)
  ;   between(0xD8, 0xF6, Code)
  ;   between(0xF8, 0x2FF, Code)
  ;   between(0x370, 0x37D, Code)
  ;   between(0x37F, 0x1FFF, Code)
  ;   between(0x200C, 0x200D, Code)
  ;   between(0x2070, 0x218F, Code)
  ;   between(0x2C00, 0x2FEF, Code)
  ;   between(0x3001, 0xD7FF, Code)
  ;   between(0xF900, 0xFDCF, Code)
  ;   between(0xFDF0, 0xFFFD, Code)
  ;   between(0x10000, 0xEFFFF, Code)
  )}.



%! 'NDataDecl'(?NotationName:atom)// .
%
% *Notation Data Declaration*
%
% ```bnf
%    NDataDecl ::= S 'NDATA' S Name
% ```
%
% @compat XML 1.0.5 [76]
% @compat XML 1.1.2 [76]
% @tbd [VC: Notation Declared]

'NDataDecl'(NotationName) -->
  'S',
  "NDATA",
  'S',
  'Name'(NotationName).



%! 'Nmtoken'(?Token:atom)// .
%
% ```ebnf
% Nmtoken ::= (NameChar)+
% ```
%
% @compat XML 1.0.5 [7].
% @compat XML 1.1.2 [7].

'Nmtoken'(Token) -->
  dcg_atom('Nmtoken_codes', Token).

'Nmtoken_codes'(Cs) -->
  +('NameChar', Cs).



%! 'Nmtokens'(?Tokens:list(atom))// .
%
% ```ebnf
% Nmtokens ::= Nmtoken (#x20 Nmtoken)*
% ```
%
% @compat XML 1.0.5 [8].
% @compat XML 1.1.2 [8].

'Nmtokens'(Tokens) -->
  +&('Nmtoken', " ", Tokens).



%! 'NotationDecl'(?NotationDeclaration:compound)// .
%
% *Notations* identify by name:
%  - the format of unparsed entities,
%  - the format of elements which bear a notation attribute, or
%  - the application to which a processing instruction is addressed.
%
% *Notation declarations* provide a name for the notation, for use in
% entity and attribute-list declarations and in attribute
% specifications, and an external identifier for the notation which
% may allow an XML processor or its client application to locate a
% helper application capable of processing data in the given notation.
%
% ```bnf
% NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
% ```
%
% @compat XML 1.1.2 [82]
% @tbd [VC: Unique Notation Name]

'NotationDecl'(notation_decl(Name,Literal)) -->
  "<!NOTATION",
  'S',
  'Name'(Name),
  'S',
  ('ExternalID'(Literal) ; 'PublicID'(Literal)),
  ?('S'),
  ">".



%! 'NotationType'(?Names:list(atom))// .
%
% ```bnf
% NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
% ```
%
% @compat XML 1.1.2 [58]
% @tbd [VC: Notation Attributes]
% @tbd [VC: One Notation Per Element Type]
% @tbd [VC: No Notation on Empty Element]
% @tbd [VC: No Duplicate Tokens]

'NotationType'(Names) -->
  "NOTATION",
  'S',
  "(",
  ?('S'),
  +&('Name', pipe_sep, Names),
  ?('S'),
  ")".



%! 'PEDecl'(?Version:compound, ?ParameterEntityDeclaration:compound)//
%
% Parameter Entity declaration.
%
% ```bnf
% PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
% ```
%
% @compat XML 1.0.5 [72]

'PEDecl'(Version, pe_decl(Name,ParameterEntityDefinition)) -->
  "<!ENTITY",
  'S',
  "%",
  'S',
  'Name'(Name),
  'S',
  'PEDef'(Version, ParameterEntityDefinition),
  ?('S'),
  ">".



%! 'PEDef'(?Version:compound, ?ParameterEntityDefinition:compound)// .
%
% ```bnf
% PEDef ::= EntityValue | ExternalID
% ```
%
% @compat XML 1.0.5 [74]

'PEDef'(Version, Names) -->
  'EntityValue'(Version, Names).
'PEDef'(_, ExternalId) -->
  'ExternalID'(ExternalId).



%! 'PEReference'(-Name:atom)// is det.
%
% **Parameter Entity Refenrece**.
%
% ```bnf
% PEReference ::= '%' Name ';'
% ```
%
% @compat XML 1.0.5 [69].
% @compat XML 1.1.2 [69].
% @tbd [VC: Entity Declared]
% @tbd [WFC: No Recursion]
% @tbd [WFC: In DTD]

'PEReference'(Name) -->
  "%",
  'Name'(Name),
  ";".



%! 'PI'(+Version:compound, -PI)// .
%
% # Syntax
%
% ```ebnf
% PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
% ```
%
% # Semantics
%
% Processing instructions (PIs) allow documents to contain
% instructions for applications.
%
% @arg Version is either `version(1,0)' for XML 1.0 or `version(1,1)'
%      for XML 1.1.
%
% @compat XML 1.0.5 [16].
% @compat XML 1.1.2 [16].

'PI'(Version, X) -->
  "<?",
  'PITarget'(Target),
  (   'S'
  ->  *(pi_code(Version), Cs),
      {
        atom_codes(A, Cs),
        X =.. [Target,A]
      }
  ;   {X = Target}
  ),
  "?>".

pi_code(_, _) -->
  "?>", !,
  {false}.
pi_code(Version, C) -->
  'Char'(Version, C).



%! 'PITarget'(-Target)// .
%
% ```ebnf
% PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
% ```
%
% Target names `XML`, `xml`, and so on are reserved for standardization
% in this or future versions of this specification.
%
% @compat XML 1.0.5 [17].
% @compat XML 1.1.2 [17].

'PITarget'(_) -->
  atom_ci(xml), !,
  {false}.
'PITarget'(Target) -->
  'Name'(Target).



%! prolog(-Version:compound, -Encoding:atom, -Standalone:boolena,
%!        -PIsList)// is det.
%
% ```ebnf
% prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
% ```
%
% @compat XML 1.0.5 [22].
% @compat XML 1.1.2 [22].

prolog(Version, prolog(Encoding,Standalone,PIs4,Decl)) -->
  ?('XMLDecl'(Version, Encoding, Standalone)),
  *('Misc'(Version), PIs1),
  (doctypedecl(Version, Decl) -> *('Misc'(Version), PIs2) ; ""),
  {
    append(PIs1, PIs2, PIs3),
    exclude(var, PIs3, PIs4)
  }.



%! 'PublicID'(?Literal:atom)// is det.
%
% ```bnf
% PublicID ::= 'PUBLIC' S PubidLiteral
% ```
%
% @compat XML 1.1.2 [83]

'PublicID'(Literal) -->
  "PUBLIC",
  'S',
  'PubidLiteral'(Literal).



%! 'PubidChar'(?C:nonneg)// .
% ```bnf
% PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
% ```
%
% @compat XML 1.0.5 [13].
% @compat XML 1.1.2 [13].

% #x20
'PubidChar'(0' ) --> " ".
% #xA
'PubidChar'(0'\n) --> "\n".
% #xD
'PubidChar'(0'\r) --> "\r".
% [a-zA-Z0-9]
'PubidChar'(C) --> alphanum(C).
% [-'()+,./:=?;!*#@$_%]
'PubidChar'(0'-) --> "-".
'PubidChar'(0'') --> "'".
'PubidChar'(0'() --> "(".
'PubidChar'(0')) --> ")".
'PubidChar'(0'+) --> "+".
'PubidChar'(0',) --> ",".
'PubidChar'(0'.) --> ".".
'PubidChar'(0'/) --> "/".
'PubidChar'(0':) --> ":".
'PubidChar'(0'?) --> "?".
'PubidChar'(0'!) --> "!".
'PubidChar'(0'*) --> "*".
'PubidChar'(0'#) --> "#".
'PubidChar'(0'@) --> "@".
'PubidChar'(0'$) --> "$".
'PubidChar'(0'_) --> "_".
'PubidChar'(0'%) --> "%".



%! 'PubidLiteral'(?Literal:atom)// .
%
% ```bnf
% PubidLiteral ::= '"' PubidChar* '"'
%                | "'" (PubidChar - "'")* "'"
% ```
%
% @compat XML 1.0.5 [12].
% @compat XML 1.1.2 [12].

'PubidLiteral'(Literal) -->
  quoted(Quote, dcg_atom(*('PubidChar0'(Quote)), Literal)).

'PubidChar0'(Quote, C) -->
  'PubidChar'(C),
  {(  Quote == "'"
  ->  C =\= 39
  )}.



%! 'Reference'(?Version:compound, ?Reference:atom)// .
%
% ```bnf
% Reference ::= EntityRef | CharRef
% ```
%
% @compat XML 1.0.5 [67].
% @compat XML 1.1.2 [67].

'Reference'(_, Reference) -->
  'EntityRef'(Reference).
'Reference'(Version, Reference) -->
  'CharRef'(Version, Reference).



%! 'RestrictedChar'(?Code:code)// .
%
% ```ebnf
% RestrictedChar11 ::=
%     \\ Start of heading, start of text, end of text, end of transmission,
%     \\ enquiry, positive acknowledgement, bell, backspace.
%       [#x1-#x8]
%
%     \\ Vertical tab, form feed.
%     | [#xB-#xC]
%
%     \\ Shift out, shift in, data link escape, device control (1, 2, 3, 4),
%     \\ negative acknowledgement, synchronous idle,
%     \\ end of transmission block, cancel, end of medium, substitute,
%     \\ escape, file separator, group separator, record separator,
%     \\ unit separator.
%     | [#xE-#x1F]
%
%     | [#x7F-#x84]
%     | [#x86-#x9F]
% ```

'RestrictedChar11'(C) -->
  [C],
  {(  between(0x1,  0x8,  C)
  ;   between(0xB,  0xC,  C)
  ;   between(0xE,  0x1F, C)
  ;   between(0x7F, 0x84, C)
  ;   between(0x86, 0x9F, C)
  )}.



%! 'S'// is det.
%
% Greedy white space.
%
% ```ebnf
% S ::= ( #x20 | #x9 | #xD | #xA )+ // Any consecutive number of spaces,
%                                   // carriage returns, line feeds, and
%                                   // horizontal tabs.
% ```
%
% The presence of carriage_return// in the above production is
% maintained purely for backward compatibility with the First Edition.
% All `#xD` characters literally present in an XML document are either
% removed or replaced by line_feed// (i.e., `#xA`) characters before
% any other processing is done.

'S' -->
  +(s_code), !.

s_code --> [0x20].
s_code --> [0x9].
s_code --> [0xD].
s_code --> [0xA].



%! 'SDDecl'(-Standalone:boolean)// is det.
%
% Standalone Declaration
%
% ```ebnf
% SDDecl ::= S 'standalone' Eq
%            (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
% ```
%
% @compat XML 1.0.5 [32].
% @compat XML 1.1.2 [32].
% @tbd [VC: Standalone Document Declaration]

'SDDecl'(Standalone) -->
  'S',
  "standalone",
  'Eq',
  ("'" -> yesno(Standalone), "'" ; "\"" -> yesno(Standalone), "\"").

yesno(true) -->"yes".
yesno(false) --> "no".



%! seq(?SequenceOfContentParticles:compound)// .
%
% ```bnf
% seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'
% ```
%
% @compat XML 1.1.2 [50]
% @tbd [VC: Proper Group/PE Nesting]

seq(seq(Cps)) -->
  "(",
  ?('S'),
  *&(cp, xml_comma, Cps),
  ?('S'),
  ")".

xml_comma -->
  ?('S'),
  ",",
  ?('S').


%! 'STag'(?Version:compound, ?Name:atom, ?Attributes:list)// .
%
% The start tag of an XML element.
%
% ```ebnf
% STag ::= '<' Name (S Attribute)* S? '>'
% ```
%
% @compat XML 1.0.5 [40].
% @compat XML 1.1.2 [40].
% @tbd [WFC: Unique Att Spec]

'STag'(Version, Name, Attributes) -->
  "<",
  'Name'(Name),
  (   'S',
      +&('Attribute'(Version), 'S', Attributes)
  ->  ""
  ;   {Attributes = []}
  ),
  ?('S'),
  ">".



%! 'StringType'// .
%
% ```bnf
% StringType ::= 'CDATA'
% ```
%
% @compat XML 1.1.2 [55]

'StringType'(cdata) -->
  "CDATA".



%! 'SystemLiteral'(?Literal:atom)// .
%
% ```bnf
% SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
% ```
%
% @compat XML 1.0.5 [11].
% @compat XML 1.1.2 [11].

'SystemLiteral'(Literal) -->
  quoted(Quote, dcg_atom(*('SystemLiteral_'(Quote)), Literal)).

'SystemLiteral_'(Quote, _) -->
  [Quote], !,
  {fail}.
'SystemLiteral_'(_, Code) -->
  [Code].



%! 'TextDecl'(?Version:compound, ?Encoding:atom)// .
%
% ```bnf
% TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
% ```
%
% @compat XML 1.1.2 [77]

'TextDecl'(Version, Encoding) -->
  "<?xml",
  ?('VersionInfo'(Version)),
  'EncodingDecl'(Encoding),
  ?('S'),
  "?>".



%! 'TokenizedType'// .
%
% ```bnf
% TokenizedType ::= 'ID'
%                 | 'IDREF'
%                 | 'IDREFS'
%                 | 'ENTITY'
%                 | 'ENTITIES'
%                 | 'NMTOKEN'
%                 | 'NMTOKENS'
% ```
%
% @compat XML 1.1.2 [56]
% @tbd [VC: ID]
% @tbd [VC: One ID per Element Type]
% @tbd [VC: ID Attribute Default]
% @tbd [VC: IDREF] for 'IDREF' and 'IDREFS'
% @tbd [VC: Entity Name] for 'ENTITY' and 'ENTITIES'
% @tbd [VC: Name Token] for 'NMTOKEN' and 'NMTOKENS'

'TokenizedType'(id) --> "ID".
'TokenizedType'(idref) --> "IDREF".
'TokenizedType'(idrefs) --> "IDREFS".
'TokenizedType'(entity) --> "ENTITY".
'TokenizedType'(entities) --> "ENTITIES".
'TokenizedType'(nmtoken) --> "NMTOKEN".
'TokenizedType'(nmtokens) --> "NMTOKENS".



%! 'VersionInfo'(-Version:compound)// is det.
%
% ```ebnf
% VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
% ```
%
% @compat XML 1.0.5 [24].
% @compat XML 1.1.2 [24].

'VersionInfo'(Version) -->
  'S',
  "version",
  'Eq',
  (   "'"
  ->  'VersionNum'(Version),
      "'"
  ;   "\""
  ->  'VersionNum'(Version),
      "\""
  ).



%! 'VersionNum'(-Version:compound)// is det.
%
% # XML 1.0
%
% ```bnf
% VersionNum ::= '1.' [0-9]+
% ```
%
% # XML 1.1
%
% ```bnf
% VersionNum ::= '1.1'
% ```
%
% @arg Version is a compound term of the form
%      `version(Major:nonneg,Minor:nonneg)'.
%
% @compat XML 1.0.5 [26].
% @compat XML 1.1.2 [26].

'VersionNum'(version(1,Minor)) -->
  "1.",
  +(digit_weight, Weights), !,
  {integer_weights(Minor, Weights)}.
'VersionNum'(version(1,1)) -->
  "1.1".



%! 'XMLDecl'(-Version:compound, -Encoding:atom, -Standalone:boolean)// is det.
%
% ```ebnf
% XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
% ```
%
% @compat XML 1.0.5 [23].
% @compat XML 1.1.2 [23].

'XMLDecl'(Version, Encoding, Standalone) -->
  "<?xml",
  'VersionInfo'(Version),
  ('EncodingDecl'(Encoding) -> "" ; ""),
  ('SDDecl'(Standalone) -> "" ; ""),
  ('S' -> "" ; ""),
  "?>".





% HELPERS %

%! kleene(?Range:pair(or([nonneg,oneof([∞])])))// .
%
% ```bnf
% ("?" | "*" | "+")?
% ```

kleene(0-1) --> "?".
kleene(0-inf) --> "*".
kleene(1-inf) --> "+".
kleene(1-1) --> "".



%! pipe_sep// .
%
% A common separator in XML.
%
% ```bnf
% S? "|" S?
% ```

pipe_sep -->
  ?('S'),
  "|",
  ?('S').



%! quoted(:Dcg_0)// is det.
%! quoted(?Quote:oneof([0'",0'']), :Dcg_0)// is det.
%
% Parse or generate `Dcg_0' within double or single quotes.

quoted(Dcg_0) -->
  quoted(_, Dcg_0).


quoted(0'", Dcg_0) -->
  "\"", !,
  Dcg_0,
  "\"".
quoted(0'', Dcg_0) -->
  "'",
  Dcg_0,
  "'".
