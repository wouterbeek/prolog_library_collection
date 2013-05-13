:- module(
  xml,
  [
    dom_to_xml/4, % +DTD_File:atom
                  % +StyleName:atom
                  % +DOM
                  % -XML:atom
    file_to_xml/2, % +File:atom
                   % -XML:dom
    stream_to_xml/2, % +Stream:stream
                     % -XML:dom
    stylesheet_pi/2, % +CSS_FileSpecification
                     % -PI:atom
    stylesheet_pi/3, % +Type:oneof(['text/css'])
                     % +CSS_FileSpecification
                     % -PI:atom
    uri_to_xml/2, % +URI:uri
                  % -XML:dom
    xml_declaration//2, % +Version:versionm
                        % +Standalone:atom
    xml_doctype/2 % +Stream:stream
                  % -DocType
  ]
).

/** <module> XML

The XML (Extensible Markup Language) is a subset of SGML (Standard Generalized
Markup Language).



---+ Design goals

* Straightforwardly usable over the Internet.
* Supporting a wide variety of applications.
* Compatible with SGML.
* Easy to write programs that process XML documents.
* Minimum number of optional features.
* XML documents should be human-legible and reasonably clear.
* "The XML design should be prepared quickly." [???]
* The design of XML shall be formal and concise.
* XML documents shall be easy to create.
* Terseness is of minimal importance.



---+ Concepts

  * *|Document element|*
    The single element in an XML document that has no parent element.
  * *|Logical structure|*
    The declarations, elements, comments, characters references, and
    processing instructions of an XML document.
  * *|Physical structure|*
    The entities / units that compose and XML document.
  * *Root*
    Synonym of _|document element|_.
  * *Validity*
    The property that an XML document complies with the constraints
    expressed in the document type declaration is references.
  * *Well-formedness*
    The property that an XML document matches the productions in the XML
    specification, meets all the well-formedness constraints, and contains
    only parsed entities that are well-formed.
  * *|XML document|*
    Can be split up in logical and physical structure.
  * *|XML processor|*
    A software module that can access the content and structure of
    XML documents.

XML document grammar rule:
==
document ::= prolog element Misc*
==



---+ Logical structure

The template that entitles the elements (and their order)
to be included in an XML document.


---++ Character references

Refer to specific characters in the ISO/IEC 10646 character set.

==
CharRef ::= '&#' [0-9]+ ';'	| '&#x' [0-9a-fA-F]+ ';'
==

Must match the production for =Char=.


---++ Comments

*Comments* may appear outside other markup and in some locations of the DTD.


---++ Declarations


---++ Document type declarations

---+++ External subset

A pointer to a special kind of _|external entity|_ containing
_|markup declarations|_.

---+++ Internal subset

Direct inclusion of _|markup declarations|_ in an XML document.


---++ Markup declaration

*|External markup declaration|*: A _|markup declaration|_ that occurs in the
_|external subject|_ or in an (internal or external) _|parameter entity|_.

---+++ Attribute-list declaration

Attribute declaration:
==
AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
AttDef      ::= S Name S AttType S DefaultDecl
==

---++++ Attribute type

==
AttType       ::= StringType | TokenizedType | EnumeratedType
StringType    ::= 'CDATA'
TokenizedType ::= 'ID'       |   // An XML name that is unique in the
                                 // XML document.
                  'IDREF'    |   // XML name, refering to the ID type
                                 // attribute of some element in the
                                 // XML document.
                  'IDREFS'   |   // Separated by whitespace.
                  'ENTITY'   |   // The name of an unparsed ENTITY defined
                                 // elsewhere in the DTD.
                  'ENTITIES' |   // Separated by whitespace.
                  'NMTOKEN'  |   // Like a name, but with no extra
                                 // restrictions on the first letter.
                  'NMTOKENS'     // Separated by whitespace.
==

Example of ID and IDREF attribute types:
==
<!ATTLIST employee social_security_number ID    #REQUIRED>
<!ATTLIST project  project_id             ID    #REQUIRED>
<!ATTLIST team_member person              IDREF #REQUIRED>
<!ATTLIST assignment  project_id          IDREF #REQUIRED>
==


---++++ Enumerated attribute types

Enumerated attribute types:
==
EnumeratedType ::= NotationType | Enumeration
NotationType   ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
==

Example of attribute type NOTATION:
==
<!NOTATION gif  SYSTEM "image/gif">
<!NOTATION tiff SYSTEM "image/tiff">
<!NOTATION jpeg SYSTEM "image/jpeg">
<!NOTATION png  SYSTEM "image/png">
<!ATTLIST  image type NOTATION (gif | tiff | jpeg | png) #REQUIRED>
==


---++++ Attribute default values

Attribute defaults:
  * =#FIXED=
    Constant and immutable.
  * =#IMPLIED=
    Optional without default.
  * Literal
    Default given as quoted string.
  * =#REQUIRED=
    Requried without default.

Grammar:
==
DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
==

Examples:
==
<!ATTLIST termdef
          id      ID      #REQUIRED
          name    CDATA   #IMPLIED>
<!ATTLIST list
          type    (bullets|ordered|glossary)  "ordered">
<!ATTLIST form
          method  CDATA   #FIXED "POST">
==


---+++ Element type declaration

Element type declarations constrain the element's content and attribute
values.

An element must not be declared more than once.

==
elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
contentspec ::= 'EMPTY' | 'ANY' | Mixed | children 
==

Examples:
==
<!ELEMENT br EMPTY>
<!ELEMENT p (#PCDATA|emph)* >
<!ELEMENT %name.para; %content.para; >
<!ELEMENT container ANY>
==

---++++ Element content

An element has *|element content|* if its content must only contain child
elements and no character data. The constrain on the element's content is
then a *|content model|*.

Examples:
==
<!ELEMENT spec (front, body, back?)>
<!ELEMENT div1 (head, (p | list | note)*, div2*)>
<!ELEMENT dictionary-body (%div.mix; | %dict.mix;)*>
==


---+++ Entity declaration

---+++ Notation declaration


---++ XML declaration

Specifying version and optionally encoding.

---+++ Standalone declaration

Optionally specified as part of the _|XML declaration|_.

Signals whether there are external declarations.

Nota that _|external entities|_ are not considered in the
standalone declaration.


---+++ Mixed content

An element has *|mixed content|* if it may contain character data
and child elements. _|In this case the order and the number of occurrences
of child elements cannot be constrained.|_

==
Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' |
          '(' S? '#PCDATA' S? ')'
==

Examples:
==
<!ELEMENT p (#PCDATA|a|ul|b|i|em)*>
<!ELEMENT p (#PCDATA | %font; | %phrase; | %special; | %form;)* >
<!ELEMENT b (#PCDATA)>
==


---++ Processing instructions

*|Processing instructions|* must be passed through to applications.



---


---+ Physical structure

The actual data used in an XML document.


---++ Entities

---+++ Parsed entities / text entities

*|Parsed entities|* contain text data that becomes part of the XML document
after processing.

---++++ Text

*Text* is any sequence of characters.

---+++++ End-of-line handling

Normalize all line endings
(i.e. occurrences and/or combinations of #xD (carriage return) and #xA)
to #xA / line feeds.

---+++++ Character

A *character* is an atomic unit of text specified by ISO/IEC 10646.

==
Char ::= #x9 |   // Horizontal tab
         #xA |   // Line feed
         #xD |   // Carriage return
         [#x20-#xD7FF] |      // Unicode characters, excluding the
         [#xE000-#xFFFD] |    // surrogate blocks #xFFFE and
         [#x10000-#x10FFFF]   // #xFFFF.
==

Avoid comapatibility characters [Unicode, section 2.3].
Avoid the following characters (control characters,
permanently undefined Unicode characters):
==
[#x7F-#x84], [#x86-#x9F], [#xFDD0-#xFDEF],
[#x1FFFE-#x1FFFF], [#x2FFFE-#x2FFFF], [#x3FFFE-#x3FFFF],
[#x4FFFE-#x4FFFF], [#x5FFFE-#x5FFFF], [#x6FFFE-#x6FFFF],
[#x7FFFE-#x7FFFF], [#x8FFFE-#x8FFFF], [#x9FFFE-#x9FFFF],
[#xAFFFE-#xAFFFF], [#xBFFFE-#xBFFFF], [#xCFFFE-#xCFFFF],
[#xDFFFE-#xDFFFF], [#xEFFFE-#xEFFFF], [#xFFFFE-#xFFFFF],
[#x10FFFE-#x10FFFF].
==

---+++++ CDATA

*CDATA* may occur anywhere _|character data|_ may occur.
In CDATA the less than sign and ampersand need not be escaped.

Anywhere character data can occur, CDATA can be used to escape blocks of
text containing characters which would otherwise be recognized as markup.

Delimiters:
  * Start tag: <![CDATA[
  * End tag: ]]>

==
CDSect  ::= CDStart CData CDEnd
CDStart ::= '<![CDATA['
CData   ::= (Char* - (Char* ']]>' Char*))
CDEnd   ::= ']]>'
==

---+++++ Character data

_Text_ that is not _markup_ is *|character data|*.

==
CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
==

Ampersand and left angle bracket must not occur except when used in markup
delimiters. Otherwise, use numeric character references or strings =&amp;=,
=&lt;=.

Right bracket must not occur in string "]]>" when not marking the end of a
CDATA section. Use numberic character reference or string =&gt;=.

Attribute values can contain single and double quotes, using =&apos;= and
=&quot;=.

---+++++ Comments

==
Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
==

Compatibility: Double hyphen must not occur in comments.

---+++++ Literal data

*|Literal data|* is any quoted string not containing the quotation mark that
is used as the delimiter for that string.

*|Literal data|*: Any quoted string not containing the quotation mark that
is used as the delimiter for that string.

Used for:
  * Content of internal entities. [???]
  * Values of attributes.
  * External identifiers. [???]

==
EntityValue   ::= '"' ([^%&"] | PEReference | Reference)* '"' |
                  "'" ([^%&'] | PEReference | Reference)* "'"
AttValue      ::= '"' ([^<&"] | Reference)* '"' |
                  "'" ([^<&'] | Reference)* "'"
SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
PubidLiteral  ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
PubidChar     ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
==

---+++++ Markup

The following _text_ is *markup*:
  * Start-tags
  * End-tags
  * Empty-element tags
  * Entity references
  * Character references
  * Comments
  * CDATA section delimiters
  * Document type declarations
  * Processing instructions
  * XML declarations
  * Text declarations
  * White space that is at the top level of the document entity.

---+++++ Name

---++ Names

*Nmtoken*: Any mixture of name characters.

*Name*: An _Nmtoken_ with a restricted set of initial characters.
Disallowed: digits, diacritics [???], full stop [???], hyphen.

Reserved names:
  * Names beginning with =xml=.
  * Strings that would match =|(('X'|'x')('M'|'m')('L'|'l'))|=.

Avoid the use of colon in names except for namespace purposes.

==
NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] |
                  [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] |
                  [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
                  [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
                  [#x10000-#xEFFFF]
NameChar      ::= NameStartChar | "-" | "." | [0-9] | #xB7 |
                  [#x0300-#x036F] | [#x203F-#x2040]
Name          ::= NameStartChar (NameChar)*
Names         ::= Name (#x20 Name)*
Nmtoken       ::= (NameChar)+
Nmtokens      ::= Nmtoken (#x20 Nmtoken)*
==

---+++++ PI, Processing Instruction

Instructions for applications (specified by =PITarget=).

==
PI        ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
PITarget  ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
==

Target names =xml= and =XML= are reserved.

Since Processing Instruction targets must be an XML name, they cannot contain
certain characters, e.g. slashes. A workaround is to define a NOTATION which
can identify a short XML name with more complicated content.

Example:
==
<!NOTATION tex SYSTEM "/usr/local/bin/tex">
==





---++++ Attribute

Attribute value normalization algorithm:
  1. All line breaks are normalized to #xA.
  1. Begin with a normalized value consisting of the empty string.
  1. For each character, entity reference, or character reference in the
     unnormalized attribute value, do:
     1. For a character reference, append the referenced character to the
        normalized value.
     1. For an entity reference, recursively apply step 3 to the
        replacement text of the entity.
     1. For a white space character (#x20, #xD, #xA, #x9), append a
        space character (#x20) to the normalized value.
     1. For another character, append the character to the normalized value.
  1. If the attribute type is not CDATA, then discard leading and trailing
     spaces, and replace sequences of spaces by a single space character.

---++++ Element

The boundaries of non-empty elements are delimited by start- and end-tags.

*|GI, Generic Indentifier|*: the name of an element's type.

An element type may have associated attribute specifications.

==
element      ::= EmptyElemTag | STag content ETag

// Empty element tag.
EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'

// Non-empty element tag.
STag         ::= '<' Name (S Attribute)* S? '>'
content      ::= CharData?
                 ((element | Reference | CDSect | PI | Comment) CharData?)*
ETag         ::= '</' Name S? '>'

// Attributes
Attribute    ::= Name Eq AttValue 
==

---+++++ Element content

==
children ::= (choice | seq) ('?' | '*' | '+')?
cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')'
==

---+++ Unparsed entities

A container whose content may be anything but XML text.

---+++ Predefined entities

---+++ Internal entities

An entity in which no separate physical storage exists.
The content is provided in its declaration.

Example:
==
<! ENTITY Publisher1 "McGrawHill Publishing Company.">
==

---+++ External entities

Refers to a storage unit.

Example:
==
<ENTITY FirstImg SYSTEM "www.books.com/images/book1.gif" NDATA GIF>
==





---++ Conditional sections

Portions of the Document Type Declaration external subset or of external
parameter entities that are included/excluded from the logical
structure of the DTD.

==
conditionalSect    ::= includeSect | ignoreSect
includeSect        ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
ignoreSect         ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
Ignore             ::= Char* - (Char* ('<![' | ']]>') Char*)
==

Parameter entities can be redefined in the itnernal DTD subset of a document.

Example parameter entity:
==
<!ENTITY % notes_allowed "INCLUDE">
==

Example parameter entity reference:
==
<![%notes_allowed;[
  <!ELEMENT production_note (#PCDATA)>
]]>
==

This allows declarations to be turned on/off from outside of a DTD document.

Another example:
==
<!ENTITY % draft 'INCLUDE' >
<!ENTITY % final 'IGNORE' >

<![%draft;[
<!ELEMENT book (comments*, title, body, supplements?)>
]]>
<![%final;[
<!ELEMENT book (title, body, supplements?)>
]]>
==

---++ Entity

  * *|Parsed entity|*
    The contents of a parsed entity (called 'replacement text') are an
    integral part of an XML document.
    Invoked by name using *|entity references|*.
  * *|Unparsed entity|*
    The contents of an unparsed entity need not be XML and need not even be
    text. The notation of an unparsed entity is identityfied by name.
    Invoked by name, given in the value of ENTITY or ENTITIES attributes. [???]
  * *|General entities|*
    Entities for use within document content.
  * *|Parameter entities|*
    Parsed entities for use in the DTD.

General and parameter entities:
  * use different forms of reference
  * are recognized in different contexts
  * occur in different namespaces

---+++ Entity declaration

==
EntityDecl ::= GEDecl | PEDecl
GEDecl     ::= '<!ENTITY' S Name S EntityDef S? '>'
PEDecl     ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
EntityDef  ::= EntityValue | (ExternalID NDataDecl?)
PEDef      ::= EntityValue | ExternalID
==

In case the same name is used multiple times, all but the first encountered
entity is dicarded.

---+++ Entity reference

Refers to the contents of a named entity.

Parsed general entity references use ampersand and semicolon delimiters.

Parameter-entity references use percent and semicolon delimiters.

General entity declarations must precede any reference to it.

Example:
==
<!ENTITY super "supercalifragilisticexpialidocious">
==

Entity references can only refer to parsed entities.

Grammar:
==
Reference   ::= EntityRef | CharRef
EntityRef   ::= '&' Name ';'
PEReference ::= '%' Name ';'
==

---+++ External entity reference

Grammar:
==
ExternalID ::= 'SYSTEM' S SystemLiteral |
               'PUBLIC' S PubidLiteral S SystemLiteral
NDataDecl  ::= S 'NDATA' S Name
==

Example:
==
<!ENTITY footer SYSTEM "http://www.oreilly.com/boilerplate/footer.xml">
==

The external file starts with a *|text declaration|*.
This is like an XML declaration, but with required encoding declaration,
optional version information, and absent standalone declaration.

---+++ External unparsed entity [NOT OFTEN USED?]

Non-XML formatted content stored in external files.

Example:
==
<!ENTITY turing_getting_off_bus
         SYSTEM "http://www.turing.org.uk/turing/pi1/bus.jpg"
         NDATA jpeg>
==

The =NDATA= declaration specifies the data type. These are defined in the
DTD using a NOTATION declaration.

Example:
==
<!NOTATION jpeg SYSTEM "image/jpeg">
==

Since entity references can only refer to parsed entities, external unparsed
entities cannot be included in an XML document by using an entitiy reference.
Instead, an element with an ENTITY type attribute whose value is the name
of the unparsed external entity must be defined.

Example:
==
<!ELEMENT image EMPTY>
<!ATTLIST image source ENTITY #REQUIRED>

<image source="turing_getting_off_bus"/>
==

---+++ Internal entity

An entity with an =EntityValue=.

An internal entity is a parsed entity.

Example:
==
<!ENTITY Pub-Status "This is a pre-release of the specification.">
==

---++ Entity reference

A way of escaping characters.

Predefined entity references:
  * &lt;
  * &amp;
  * &gt;
  * &quot;
  * &apos;

---++ Language identification

Attribute =|xml:lang|= specifies the natural or formal language used in
the contents and attribute values of the element for which the attribute
is declared.

The values that may be declared for this attribute must come from BCP 47
and the empty string.

Example of a collection of French poems for English students, with glosses
and notes in English:
==
<!ATTLIST poem   xml:lang CDATA 'fr'>
<!ATTLIST gloss  xml:lang CDATA 'en'>
<!ATTLIST note   xml:lang CDATA 'en'>
==

---++ Parameter entity

Multiple elements may (partially) share the same attributes.

*|Parameter entities|* can be using in a DTD to introduce the same text
at multiple locations.

Example of a parameter entity specification:
==
<!ENTITY % residential_content "address, footage, rooms, baths">
==

Example of parameter entity references, using a parameter entity:
==
<!ELEMENT apartment (%residential_content;, %rental_content;)>
<!ELEMENT sublet    (%residential_content;, %rental_content;)>
==

Parameter entities can be redefined in the internal DTD subset of a document.

---+++ external parameter entity 

Parameter entity references to external parameter entities cause the contents
of external DTDs to be inserted.

Example:
==
<!ENTITY % names SYSTEM "names.dtd">
%names;
==

---++ PCDATA

Parsed character data.

---++ Prolog

An XML document must begin with an XML declaration specifying the XML version.

*Validity*: An XML document is valid if it has an associated DTD and
the document complies with the constraints in the DTD.

The Document Type Declaration that identifies a DTD, Document Type Definition,
must appear before the first element in the XML document.

==
prolog      ::= XMLDecl? Misc* (doctypedecl Misc*)?
XMLDecl     ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
Eq          ::= S? '=' S?
VersionNum  ::= '1.' [0-9]+
Misc        ::= Comment | PI | S 
==

A DTD consists of *|markup declarations|*:
  * Element type declarations
  * Attribute-list declarations
  * Entity declarations
  * Notation declarations
  * The above may be enclosed within parameter entities.

MIME media type: =|application/xml-dtd|=

==
doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
DeclSep     ::= PEReference | S
intSubset   ::= (markupdecl | DeclSep)*
markupdecl  ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
==

Example:
==
<!DOCTYPE person SYSTEM "http://www.cafeconleche.org/dtds/person.dtd">
<!DOCTYPE rss PUBLIC "-//Netscape Communications//DTD RSS 0.91//EN"
              "http://my.netscape.com/publish/formats/rss-0.91.dtd">
==

* =Name= must match the element type of the root element.

*|Entity references|* provide replacement text for parts of the DTD
that will be included in the XML document.

*|Parameter entities|* provide replacement text for excluse use inside a DTD.

---++ External subset

==
extSubset     ::= TextDecl? extSubsetDecl
extSubsetDecl ::= ( markupdecl | conditionalSect | DeclSep)*
==

*|External markup declaration|*:
A markup declaration occurring in an external subset or
in a parameter entity. [???]

==
SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
==

---+ Text

*Text*: A sequence of characters that may represent markup or character data.

---+ White space

==
S ::= (#x20 | #x9 | #xD | #xA)+   // Any consecutive number of spaces,
                                  // carriage returns, line feeds, and
                                  // horizontal tabs.
==

---++ White space preservation

Attribute =|xml:space|= indicates that applications should preserve white
space occurring in the element for which the attribute is defined.

The attribute must be declared by being given an enumerated type with at
least one of the values =default= and =preserve=.

Examples:
==
<!ATTLIST poem  xml:space (default|preserve) 'preserve'>
<!ATTLIST pre xml:space (preserve) #FIXED 'preserve'>
==

@author Wouter Beek
@compat XML 1.0 (Fifth Edition)
@see http://www.w3.org/TR/2008/REC-xml-20081126/
@tbd
@version 2012/10, 2013/02-2013/05
*/

:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(sgml_parse)).
:- use_module(standards(standards), [charset/1]).
:- use_module(xml(xml_namespace)).

:- db_add_novel(user:prolog_file_type(css, css)).
:- db_add_novel(user:prolog_file_type(dtd, dtd)).
:- db_add_novel(user:prolog_file_type(xml, xml)).

:- multifile(http:location/3).
:- dynamic(http:location/3).

% Serve CSS files.
http:location(css, root(css),  []).
:- assert(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix, priority(10)]).

:- xml_register_namespace(iso, 'http://www.iso.org/').
:- xml_register_namespace(stdc, 'http://www.example.org/standards/').
:- xml_register_namespace(w3c, 'http://www.w3.org/').

init:-
  Graph = w3c,
  
  % XML Working Group
  rdf_global_id(w3c:'XML/Core/', XMLWG),
  rdfs_assert_label(XMLWG, 'XML Core Working Group', Graph),
  
  % XML Recommendation
  rdf_global_id(w3c:'TR/2008/REC-xml-20081126/', This),
  rdfs_assert_individual(This, w3c:'Recommendation', Graph),
  rdf_assert_datatype(This, w3c:year, gYear, 2008, Graph),
  rdf_assert_literal(
    This,
    w3c:title,
    'Extensible Markup Language (XML) 1.0 (Fifth Edition)',
    Graph
  ),
  rdf_assert(This, w3c:developed_by, XMLWG, Graph),
  rdf_assert_literal(This, w3c:author, 'Tim Bray', Graph),
  rdf_assert_literal(This, w3c:author, 'Jean Paoli', Graph),
  rdf_assert_literal(This, w3c:author, 'C. M. Sperberg-McQueen', Graph),
  rdf_assert_literal(This, w3c:author, 'Eve Maler', Graph),
  rdf_assert_literal(This, w3c:author, 'François Yergeau', Graph),
  rdf_assert(This, w3c:supercedes, w3c:'TR/2006/REC-xml-20060816/', Graph),
  % SGML
  rdf_assert(This, w3c:mentions, iso:'8879', Graph),
   % Characters
  rdf_assert(This, w3c:requires, iso:'10646', Graph),
  rdf_assert(This, w3c:requires, std:'BCP47', Graph),
  % Language identification tags
  rdf_assert(This, w3c:requires, std:'IANA-LANGCODES', Graph),
  % Unicode
  rdf_assert(This, w3c:requires, std:'Unicode', Graph),
  true.
:- init.



%% dom_to_xml(+DTD_Name, +Style_Name, +DOM, -XML) is det.
% Translates DOM to XML, applying DTD checks and Style decoration.
%
% @param DTD_Name The atomic name of a DTD file. File locations that
%   contain DTD files must be asserted using
%   =|file_search_path/3|=.

dom_to_xml(DTD_Name, Style_Name, DOM, XML):-
  new_dtd(DTD_Name, DTD),
  % Retrieve the first DTD file with the given name.
  dtd_file(DTD_Name, DTD_File),
  load_dtd(DTD, DTD_File),
  tmp_file_stream(text, TemporaryFile, Out),
  file_name_type(Style_Name, css, Style),
  stylesheet_pi(css(Style), PI),
  % Set the header to false, since this XML content will be inserted inside
  % a Web page.
  % We do add the stylesheet parsing instruction, since this is allowed by
  % Firefox.
  xml_write(Out, [PI | DOM], [dtd(DTD), header(false)]),
  close(Out),
  open(TemporaryFile, read, In, [type(text)]),
  stream_to_atom(In, XML),
  close(In),
  delete_file(TemporaryFile),
  free_dtd(DTD).

%% dtd_file(+Name:atom, -File:atom) is det.
% Returns the first DTD file with the given name or throws an
% existence error.
%
% @param Name The atomic name of a DTD file.
% @param File The atomic name of the path of a DTD file.

dtd_file(Name, File):-
  % By setting option =solutions= to value =all= we backtrack over
  % the various file search paths that are defined for =dtd=.
  once(
    absolute_file_name(
      dtd(Name),
      File,
      [access(read), file_type(dtd), solutions(all)]
    )
  ).

%% file_to_xml(+File:atom, -XML:dom) is det.
% Reads the XML from the given file and return the DOM.

file_to_xml(File, XML):-
  setup_call_cleanup(
    open(File, read, Stream),
    stream_to_xml(Stream, XML),
    close(Stream)
  ).

%% stream_to_xml(+Stream:stream, -XML:dom) is det.
% Reads the XML DOM from the given stream.

stream_to_xml(Stream, XML):-
  load_structure(
    stream(Stream),
    XML,
    [
      dialect(xml),
      max_errors(-1),
      shorttag(false),
      space(remove),
      syntax_errors(quiet)
    ]
  ).

stylesheet_pi(CSS_FileSpecification, PI):-
  stylesheet_pi('text/css', CSS_FileSpecification, PI).

stylesheet_pi(Type, CSS_FileSpecification, pi(PI)):-
  http_absolute_location(CSS_FileSpecification, CSS_File, []),
  format(atom(PI), 'xml-stylesheet type="~w" href="~w"', [Type, CSS_File]).

%% uri_to_xml(+URI:uri, -XML:dom) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given URI.

uri_to_xml(URI, XML):-
  setup_call_cleanup(
    % First perform this setup once/1.
    http_open(URI, Stream, [timeout(60)]),
    % The try to make this goal succeed.
    stream_to_xml(Stream, XML),
    % If goal succeeds, then perform this cleanup.
    close(Stream)
  ).

%% xml_declaration(+Version:version, +Standalone:yes_no)//
% DCG for XML declarations.
% Based on the XML version and whether the XML is a standalone file.
%
% @param Version A version of XML.
% @param Standalone Whether the XML file is standalone or not.
%        Possible values:
%        1. =yes=
%        2. =no=

xml_declaration(version(Major/Minor/_Variant, _Year), Standalone) -->
  ['<?xml'],
  {format(atom(VersionInfo), 'version="~w.~w"', [Major, Minor])},
  [VersionInfo],
  {
    charset(Encoding),
    format(atom(EncodingDeclaration), 'encoding="~w"', [Encoding])
  },
  [EncodingDeclaration],
  {format(atom(StandaloneDeclaration), 'standalone="~w"', [Standalone])},
  [StandaloneDeclaration],
  ['?>'].

%% xml_doctype(+Stream, -DocType) is semidet.
% Parse a _repositional_ stream and get the  name of the first XML
% element *and* demand that this   element defines XML namespaces.
% Fails if the document is illegal XML before the first element.
%
% Note that it is not  possible   to  define valid RDF/XML without
% namespaces, while it is not possible  to define a valid absolute
% Turtle URI (using <URI>) with a valid xmlns declaration.
%
% @author Jan Wielemaker
% @version 2011

xml_doctype(Stream, DocType):-
  catch(
    setup_call_cleanup(
      make_parser(Stream, Parser, State),
      sgml_parse(
        Parser,
        [
          call(begin, on_begin),
          call(cdata, on_cdata),
          max_errors(1),
          source(Stream),
          syntax_errors(quiet)
        ]
      ),
      cleanup_parser(Stream, Parser, State)
    ),
    Exception,
    true
  ),
  nonvar(Exception),
  Exception = tag(DocType).

on_begin(Tag, Attributes, _Parser):-
  memberchk(xmlns:_=_, Attributes),
  throw(tag(Tag)).

on_cdata(_CDATA, _Parser):-
  throw(error(cdata)).

