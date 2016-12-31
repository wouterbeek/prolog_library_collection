:- module(
  xml_name_token,
  [
    'Names'//1, % ?Names:list(atom)
    'Nmtoken'//1, % ?Token:atom
    'Nmtokens'//1 % ?Tokens:list(atom)
  ]
).

/** <module> XML name token

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/03-2014/05, 2014/10-2014/12, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_unicode)).





%! 'Names'(?Names:list(atom))// .
% ```ebnf
% Names ::= Name (#x20 Name)*
% ```
%
% @compat XML 1.0.5 [6].
% @compat XML 1.1.2 [6].

'Names'(Names) -->
  seplist('Name', " ", Names).



%! 'Nmtoken'(?Token:atom)// .
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
% ```ebnf
% Nmtokens ::= Nmtoken (#x20 Nmtoken)*
% ```
%
% @compat XML 1.0.5 [8].
% @compat XML 1.1.2 [8].

'Nmtokens'(Tokens) -->
  seplist('Nmtoken', " ", Tokens).
