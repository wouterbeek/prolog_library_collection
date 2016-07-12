:- module(
  xml_element,
  [
    content//2, % ?Version:oneof(['1.0','1.1'])
                % ?Content:list(compound)
    element//2, % ?Version:oneof(['1.0','1.1'])
                % ?Element:compound
    'EmptyElemTag'//3, % ?Version:oneof(['1.0','1.1'])
                       % ?Name
                       % ?Attrs
    'ETag@xml'//1, % ?Name
    'STag'//3 % ?Version:oneof(['1.0','1.1'])
              % ?Name
              % ?Attrs
  ]
).

/** <module> XML element

DCG rules for XML elements.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml_cdata)).
:- use_module(library(xml/xml_char_data)).
:- use_module(library(xml/xml_comment)).
:- use_module(library(xml/xml_entity_ref)).
:- use_module(library(xml/xml_literal)).
:- use_module(library(xml/xml_name_token)).
:- use_module(library(xml/xml_pi)).
:- use_module(library(xml/xml_prolog)).
:- use_module(library(xml/xml10_code)).





%! 'Attribute'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?Attribute:pair(atom,list(atom))
%! )// .
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



%! content(?Version:oneof(['1.0','1.1']), ?Content:list(compound))// .
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
  *(content0(Version), T).

content0(Version, Content) -->
  (   element(Version, Content)
  ;   'Reference'(Version, Content)
  ;   'CDSect'(Version, Content)
  ;   'PI'(Version, Content)
  ;   'Comment'(Version)
  ),
  ?('CharData'(_)).



%! element(?Version:oneof(['1.0','1.1']), ?Element:compound)// .
%
% ```bnf
% element ::=   EmptyElemTag
%             | STag content ETag
% ```
%
% @compat XML 1.0.5 [39].
% @compat XML 1.1.2 [39].
% @tbd [WFC: Element Type Match]
% @tbd [VC: Element Valid]

element(Version, element(Name, Attrs)) -->
  'EmptyElemTag'(Version, Name, Attrs).
element(Version, element(Name,Attrs,Content)) -->
  'STag'(Version, Name, Attrs),
  content(Version, Content),
  'ETag@xml'(Name).



%! 'EmptyElemTag'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?Name,
%!   ?Attrs
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

'EmptyElemTag'(Version, Name, Attrs) -->
  "<",
  'Name'(Name),
  ('S', seplist('Attribute'(Version), 'S', Attrs) ; {Attrs = []}),
  ?('S'),
  "/>".



%! 'ETag@xml'(?Name)// .
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



%! 'STag'(?Version:oneof(['1.0','1.1']), ?Name, ?Attrs)// .
% The start tag of an XML element.
%
% ```ebnf
% STag ::= '<' Name (S Attribute)* S? '>'
% ```
%
% @compat XML 1.0.5 [40].
% @compat XML 1.1.2 [40].
% @tbd [WFC: Unique Att Spec]

'STag'(Version, Name, Attrs) -->
  "<",
  'Name'(Name),
  ('S', seplist('Attribute'(Version), 'S', Attrs) ; {Attrs = []}),
  ?('S'),
  ">".
