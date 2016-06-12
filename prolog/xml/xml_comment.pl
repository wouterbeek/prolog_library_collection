:- module(
  xml_comment,
  [
    'Comment'//1 % ?Version:oneof(['1.0','1.1'])
  ]
).

/** <module> XML: Comment

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml10_code)).





%! 'Comment'(?Version:oneof(['1.0','1.1']))// .
% # Syntax
%
% ```abnf
% Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
% ```
%
% Comments may appear anywhere in a document outside other markup;
%  in addition, they may appear within the document type declaration
%  at places allowed by the grammar.
%
% For compatibility, the string `--` (double-hyphen) MUST NOT occur within
%  comments.
%
% Parameter entity references MUST NOT be recognized within comments.
%
% Note that the grammar does not allow a comment ending in `--->`.
%
% ### Example
%
% ```xml
% <!-- declarations for <head> & <body> -->
% ```
%
% @compat XML 1.0.5 [15].
% @compat XML 1.1.2 [15].

'Comment'(Version) -->
  "<!--",
  '*'('CommentChar0'(Version), []),
  "-->".

'CommentChar0'(_) --> "--", !, {false}.
'CommentChar0'(Version) --> 'Char'(Version, _).
