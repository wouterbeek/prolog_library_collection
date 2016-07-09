:- module(
  xml_literal,
  [
    'AttValue'//2, % ?Version:oneof(['1.0','1.1'])
                   % ?Value:list(atom)
    'EntityValue'//2, % ?Version:oneof(['1.0','1.1'])
                      % ?Names:list(atom)
    'PubidLiteral'//1, % ?Literal:atom
    'SystemLiteral'//1 % ?Literal:atom
  ]
).

/** <mdoule> XML syntax: Literal

Grammar for XML literals.

**Literal data** is any quoted string not containing the quotation mark
 used as a delimiter for that string.

Literals are used for:
  - specifying the content of internal entities (EntityValue//1)
  - the values of attributes (AttValue//1)
  - external identifiers (SystemLiteral//1).

Note that a SystemLiteral can be parsed without scanning for markup.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2015/09, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(xml/xml_entity_ref)).





%! 'AttValue'(?Version:oneof(['1.0','1.1']), ?Value:list(atom))// .
%
% ```bnf
% AttValue ::=   '"' ([^<&"] | Reference)* '"'
%              | "'" ([^<&'] | Reference)* "'"
% ```
%
% @compat XML 1.0.5 [10].
% @compat XML 1.1.2 [10].

'AttValue'(Version, Names) -->
  quoted(Quote, '*'('Reference0'(Version, Quote), Names, [])).

'Reference0'(Version, Quote, Name) -->
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



%! 'EntityValue'(?Version:oneof(['1.0','1.1']), ?References:list(atom))// .
% A sequence of references to:
%   - Parameter Entities
%   - Entities
%   - Characters
%
% ```bnf
% EntityValue ::=   '"' ([^%&"] | PEReference | Reference)* '"'
%                 | "'" ([^%&'] | PEReference | Reference)* "'"
% ```
%
% @compat XML 1.0.5 [9].
% @compat XML 1.1.2 [9].

'EntityValue'(Version, References) -->
  quoted(Quote, *('EntityValue0'(Version, Quote), References)).

'EntityValue0'(Version, Quote, Reference) -->
  (   'PEReference'(Reference)
  ;   'Reference'(Version, Reference)
  ),
  {
    (   Quote == "\""
    ->  \+ sub_atom(Reference, _, 1, _, '"')
    ;   Quote == "'"
    ->  \+ sub_atom(Reference, _, 1, _, '\'')
    ),
    \+ sub_atom(Reference, _, 1, _, '%'),
    \+ sub_atom(Reference, _, 1, _, '&')
  }.



%! 'PubidChar'(?C:nonneg)// .
% ```bnf
% PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
% ```
%
% @compat XML 1.0.5 [13].
% @compat XML 1.1.2 [13].

% #x20
'PubidChar'(C) --> space(C).
% #xA
'PubidChar'(C) --> line_feed(C).
% #xD
'PubidChar'(C) --> carriage_return(C).
% [a-zA-Z0-9]
'PubidChar'(C) --> ascii_alpha_numeric(C).
% [-'()+,./:=?;!*#@$_%]
'PubidChar'(C) --> hyphen_minus(C).
'PubidChar'(C) --> apostrophe(C).
'PubidChar'(C) --> round_bracket(C).
'PubidChar'(C) --> plus_sign(C).
'PubidChar'(C) --> comma(C).
'PubidChar'(C) --> dot(C).
'PubidChar'(C) --> forward_slash(C).
'PubidChar'(C) --> colon(C).
'PubidChar'(C) --> question_mark(C).
'PubidChar'(C) --> exclamation_mark(C).
'PubidChar'(C) --> asterisk(C).
'PubidChar'(C) --> number_sign(C).
'PubidChar'(C) --> at_sign(C).
'PubidChar'(C) --> dollar_sign(C).
'PubidChar'(C) --> underscore(C).
'PubidChar'(C) --> percent_sign(C).



%! 'PubidLiteral'(?Literal:atom)// .
% ```bnf
% PubidLiteral ::=   '"' PubidChar* '"'
%                  | "'" (PubidChar - "'")* "'"
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



%! 'SystemLiteral'(?Literal:atom)// .
% ```bnf
% SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
% ```
%
% @compat XML 1.0.5 [11].
% @compat XML 1.1.2 [11].

'SystemLiteral'(Literal) -->
  quoted(Quote, dcg_atom('*'('SystemLiteral0'(Quote), []), Literal)).

'SystemLiteral0'(Quote, C) -->
  [C],
  {(Quote == "\"" -> C =\= 34 ; Quote == "'" -> C =\= 39)}.
