:- module(
  xml_parse,
  [
    'Char'//2,             % +Version, ?Code
    'Comment'//1,          % +Version
    'DeclSep'//1,          % ?Name:atom
    %doctypedecl//2,        % +Version, -Decl
    'EncodingDecl'//1,     % -Enc
    'Eq'//0,
    'Misc'//2,             % -Version, -PIs
    'Name'//1,             % -Name
    'PEReference'//1,      % -Ref
    'PI'//2,               % +Version, -PI
    %prolog//4,             % -Version, -Enc, -Standalone, -PIs
    'RestrictedChar11'//1, % ?Code
    'S'//0,
    'SDDecl'//1,           % -Standalone
    'VersionInfo'//1,      % -Version
    'XMLDecl'//3           % -Version, -Enc, -Standalone
  ]
).

/** <module> XML parser

@author Wouter Beek
@compat XML 1.0.5
@compat XML 1.1.2
@see http://www.w3.org/TR/2008/REC-xml-20081126/
@see http://www.w3.org/TR/2006/REC-xml11-20060816/
@version 2015/07, 2015/11, 2016/06, 2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).





%! 'Char'(?Code)// .
%
% An **XML Character** is an atomic unit of text specified by ISO/IEC
% 10646.
%
% # XML 1.0
%
% ```ebnf
% Char ::=   #x9              // Horizontal tab
%          | #xA              // Line feed
%          | #xD              // Carriage return
%          | [#x20-#xD7FF]    // Space, punctuation, numbers, letters
%          | [#xE000-#xFFFD]
%          | [#x10000-#x10FFFF]
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
%          /* any Unicode character, excluding the surrogate blocks,
%             FFFE, and FFFF. */
% ```

'Char'(version(1,0), 0x9) --> [0x9].
'Char'(version(1,0), 0xA) --> [0xA].
'Char'(version(1,0), 0xD) --> [0xD].
'Char'(version(1,0), C) -->
  [C],
  {(  between(0x20, 0xD7FF, C)
  ;   between(0xE000, 0xFFFD, C)
  ;   between(0x10000, 0x10FFFF, C)
  )}.
'Char'(version(1,1), C) -->
  [C],
  {(  between(0x1, 0xD7FF, C)
  ;   between(0xE000, 0xFFFD, C)
  ;   between(0x10000, 0x10FFFF, C)
  )}.



%! 'Comment'(+Version)// .
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
% @compat XML 1.0.5 [15].
% @compat XML 1.1.2 [15].

'Comment'(Version) -->
  "<!--",
  '*'(comment_char(Version)),
  "-->".

comment_char(_) --> "--", !, {false}.
comment_char(Version) --> 'Char'(Version, _).



%! 'DeclSep'(-Name)// .
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



/*
%! doctypedecl(+Version, -Decl)// .
%
% ```bnf
% doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S?
%                 ('[' intSubset ']' S?)? '>'
% ```
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
*/



%! 'EncodingDecl'(-Enc)// .
%
% ```ebnf
% EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
% ```
%
% @compat XML 1.0.5 [80]
% @compat XML 1.1.2 [80]

'EncodingDecl'(Enc) -->
  'S',
  "encoding",
  'Eq',
  ("\"" -> 'EncName'(Enc), "\"" ; "'" -> 'EncName'(Enc), "'").



%! 'EncName'(-Enc)// .
%
% ```bnf
% EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
% ```
%
% compat XML 1.0.5 [81]
% compat XML 1.1.2 [81]

'EncName'(Enc) -->
  ascii_letter(H),
  '*'(enc_name_char, T),
  {atom_codes(Enc, [H|T])}.

enc_name_char(C) --> ascii_alpha_numeric(C).
enc_name_char(0'.) --> ".".
enc_name_char(0'_) --> "_".
enc_name_char(0'-) --> "-".



%! 'Eq'// .
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



/*
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
*/



%! 'Misc'(+Version, -PI)// .
%
% ```ebnf
% Misc ::= Comment | PI | S
% ```
%
% @compat XML 1.0.5 [27].
% @compat XML 1.1.2 [27].

'Misc'(Version, _) -->
  'Comment'(Version).
'Misc'(Version, PI) -->
  'PI'(Version, PI).
'Misc'(_, _) -->
  'S'.



%! 'Name'(?Name:atom)//
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
  'NameStartChar'(H),
  *('NameChar', T),
  {atom_codes(Name, [H|T])}.



%! 'NameChar'(?C)// .
%
% ```ebnf
% NameChar ::=   NameStartChar
%              | "-"
%              | "."
%              | [0-9]
%              | #xB7
%              | [#x0300-#x036F]
%              | [#x203F-#x2040]
% ```
%
% @compat XML 1.0.5 [4a].
% @compat XML 1.1.2 [4a].

'NameChar'(C) --> 'NameStartChar'(C).
'NameChar'(0'-) --> "-".
'NameChar'(0'.) --> ".".
'NameChar'(C) --> digit(_, C).
% #x00B7
'NameChar'(0'·) --> "·".
% #x0300-#x036F
'NameChar'(C) --> [C], {between(0x300, 0x36F, C)}.
% #x203F
'NameChar'(0'‿) --> "‿".
% #x2040
'NameChar'(0'⁀) --> "⁀".



%! 'NameStartChar'(?C)// .
%
% ```ebnf
% NameStartChar ::=   ":"
%                   | [A-Z]
%                   | "_"
%                   | [a-z]
%                   | [#xC0-#xD6]
%                   | [#xD8-#xF6]
%                   | [#xF8-#x2FF]
%                   | [#x370-#x37D]
%                   | [#x37F-#x1FFF]
%                   | [#x200C-#x200D]
%                   | [#x2070-#x218F]
%                   | [#x2C00-#x2FEF]
%                   | [#x3001-#xD7FF]
%                   | [#xF900-#xFDCF]
%                   | [#xFDF0-#xFFFD]
%                   | [#x10000-#xEFFFF]
% ```
%
% @compat XML 1.0.5 [4].
% @compat XML 1.1.2 [4].

'NameStartChar'(C) --> ascii_letter(C).
'NameStartChar'(0':) --> ":".
'NameStartChar'(0'_) --> "_".
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
'NameStartChar'(C) -->
  [C],
  {(  between(0xC0, 0xD6, C)
  ;   between(0xD8, 0xF6, C)
  ;   between(0xF8, 0x2FF, C)
  ;   between(0x370, 0x37D, C)
  ;   between(0x37F, 0x1FFF, C)
  ;   between(0x200C, 0x200D, C)
  ;   between(0x2070, 0x218F, C)
  ;   between(0x2C00, 0x2FEF, C)
  ;   between(0x3001, 0xD7FF, C)
  ;   between(0xF900, 0xFDCF, C)
  ;   between(0xFDF0, 0xFFFD, C)
  ;   between(0x10000, 0xEFFFF, C)
  )}.



%! 'PEReference'(-Ref)// .
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



%! 'PI'(+Version, -PI)// .
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
% @compat XML 1.0.5 [16].
% @compat XML 1.1.2 [16].

'PI'(Version, X) -->
  "<?",
  'PITarget'(Target),
  (   'S'
  ->  '*'(pi_code(Version), Cs),
      {
        atom_codes(A, Cs),
        X =.. [Target,A]
      }
  ;   {X = Target}
  ),
  "?>".

pi_code(_, _) --> "?>", !, {false}.
pi_code(Version, C) --> 'Char'(Version, C).



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



/*
%! prolog(-Version, -Enc, -Standalone, -PIs)// .
%
% ```ebnf
% prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
% ```
%
% @compat XML 1.0.5 [22].
% @compat XML 1.1.2 [22].

prolog(Version, Enc, Standalone, PIs) -->
  ?('XMLDecl'(Version, Enc, Standalone)),
  '*'('Misc'(Version), PIs1),
  (doctypedecl(Version) -> '*'('Misc'(Version), PIs2) ; ""),
  {
    append(PIs1, PIs2, PIs3),
    exclude(var, PIs3, PIs)
  }.
*/



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



%! 'S'// .
%
% White space.
%
% ```ebnf
% S ::= ( #x20 | #x9 | #xD | #xA )+   // Any consecutive number of spaces,
%                                     // carriage returns, line feeds, and
%                                     // horizontal tabs.
% ```
%
% The presence of carriage_return// in the above production is
% maintained purely for backward compatibility with the First Edition.
% All `#xD` characters literally present in an XML document are either
% removed or replaced by line_feed// (i.e., `#xA`) characters before
% any other processing is done.

'S' --> +(s_code).

s_code --> [0x20].
s_code --> [0x9].
s_code --> [0xD].
s_code --> [0xA].



%! 'SDDecl'(-Standalone)// .
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



%! 'VersionInfo'(-Version)// .
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
  ("'" -> 'VersionNum'(Version), "'" ; "\"" -> 'VersionNum'(Version), "\"").



%! 'VersionNum'(-Version)// .
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
% @compat XML 1.0.5 [26].
% @compat XML 1.1.2 [26].

'VersionNum'(version(1,Minor)) -->
  "1.",
  +(digit, Ds),
  {pos_sum(Ds, Minor)}.
'VersionNum'(version(1,1)) -->
  "1.1".



%! 'XMLDecl'(-Version, -Enc, -Standalone)// .
%
% ```ebnf
% XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
% ```
%
% @compat XML 1.0.5 [23].
% @compat XML 1.1.2 [23].

'XMLDecl'(Version, Enc, Standalone) -->
  "<?xml",
  'VersionInfo'(Version),
  ?('EncodingDecl', Enc),
  ?('SDDecl'(Standalone)),
  ?('S'),
  "?>".
