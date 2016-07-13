:- module(
  rfc5988,
  [
    link//1 % -Links:list(dict)
  ]
).

/** <module> RFC 5988: Web Linking

@author Wouter Beek
@compat RFC 5988
@see https://tools.ietf.org/html/rfc5988
@version 2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(html/html_dcg), [
     'MediaDesc'//1 % -MediaDescriptions:list(string)
   ]).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/http11), [
     'OWS'//0
   ]).
:- use_module(library(http/rfc2616), [
     'ALPHA'//1,         % ?Code
     'DIGIT'//1,         % ?Weight
     'DIGIT'//2,         % ?Weight:between(0,9), ?Code
     'LOALPHA'//1,       % ?Code
     'quoted-string'//1, % -String:string
     'SP'//0
   ]).
:- use_module(library(http/rfc4288), [
     'subtype-name'//1, % -SubtypeName:string
     'type-name'//1     % -TypeName:string
   ]).
:- use_module(library(http/rfc5987), [
     'ext-value'//1, % -Value:dict
     parmname//1     % -Name:string
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 % -LTag:list(string)
   ]).
:- use_module(library(uri/rfc3986), [
     'URI'//1,                             % -Uri:dict
     'URI-reference'//1 as 'URI-Reference' % -UriReference:dict
   ]).





%! 'ext-name-star'(-Name:string)// is det.
% ```abnf
% ext-name-star = parmname "*"   ; reserved for RFC2231-profiled
%                                ; extensions.  Whitespace NOT
%                                ; allowed in between.
% ```

'ext-name-star'(S) --> parmname(S), "*".



%! 'ext-rel-type'(-Uri:dict)// is det.
% ```abnf
% ext-rel-type = URI
% ```

'ext-rel-type'(D) --> 'URI'(D).



%! link(-Links:list(dict))// is det.
% ```abnf
% Link = "Link" ":" #link-value
% ```

link(L) --> *#('link-value', L).



%! 'link-extension'(-Key:string, -Value)// is det.
% ```abnf
% link-extension = ( parmname [ "=" ( ptoken | quoted-string ) ] )
%                 | ( ext-name-star "=" ext-value )
% ```

'link-extension'(Key, Value) -->
  'ext-name-star'(Key), !, "=", 'ext-value'(Value).
'link-extension'(Key, Value) -->
  parmname(Key), ("=" -> (ptoken(Value), ! ; 'quoted-string'(Value)) ; {Value = true}).



%! 'link-param'(-Parameter:dict)// is det.
% ```abnf
% link-param = ( ( "rel" "=" relation-types )
%              | ( "anchor" "=" <"> URI-Reference <"> )
%              | ( "rev" "=" relation-types )
%              | ( "hreflang" "=" Language-Tag )
%              | ( "media" "=" ( MediaDesc | ( <"> MediaDesc <"> ) ) )
%              | ( "title" "=" quoted-string )
%              | ( "title*" "=" ext-value )
%              | ( "type" "=" ( media-type | quoted-mt ) )
%              | ( link-extension ) )
% ```

'link-param'(_{'@type': 'llo:parameter', 'llo:key': Key, 'llo:value': Value}) -->
  'link-param'(Key, Value).

'link-param'("rel", L)         --> atom_ci(rel), "=", !, 'relation-types'(L).
'link-param'("anchor", Uri)    --> atom_ci(anchor), "=", !, "\"", 'URI-Reference'(Uri), "\"".
'link-param'("rev", L)         --> atom_ci(rev), "=", !, 'relation-types'(L).
'link-param'("hreflang", LTag) --> atom_ci(hreflang), "=", !, 'Language-Tag'(LTag).
'link-param'("media", L)       --> atom_ci(media), "=", !, ('MediaDesc'(L) ; "\"", 'MediaDesc'(L), "\""), !.
'link-param'("title", S)       --> atom_ci(title), "=", !, 'quoted-string'(S).
'link-param'("title*", S)      --> atom_ci('title*'), "=", !, 'ext-value'(S).
'link-param'("type", D)        --> atom_ci(type), "=", !, ('media-type'(D) ; 'quoted-mt'(D)), !.
'link-param'(S, X)             --> 'link-extension'(S, X).



%! 'link-value'(-Value:dict)// is det.
% ```abnf
% link-value = "<" URI-Reference ">" *( ";" link-param )
% ```

'link-value'(_{'@type': 'llo:link-value', 'llo:parameters': L, 'llo:uri': D}) -->
  "<", 'URI-Reference'(D), ">", *(sep_link_param, L).

sep_link_param(D) --> 'OWS', ";", 'OWS', 'link-param'(D).



%! 'media-type'(-MediaType:dict)// is det.
% ```abnf
% media-type = type-name "/" subtype-name
% ```

'media-type'(_{'@type': 'llo:MediaType', 'llo:type': Type, 'llo:subtype': Subtype}) -->
  'type-name'(Type), "/", 'subtype-name'(Subtype).



%! ptoken(-Token:string)// is det.
% ```abnf
% ptoken = 1*ptokenchar
% ```

ptoken(S) --> +(ptokenchar, Cs), {string_codes(S, Cs)}.



%! ptokenchar(?Code)// .
% abnf
% ptokenchar = "!" | "#" | "$" | "%" | "&" | "'" | "("
%            | ")" | "*" | "+" | "-" | "." | "/" | DIGIT
%            | ":" | "<" | "=" | ">" | "?" | "@" | ALPHA
%            | "[" | "]" | "^" | "_" | "`" | "{" | "|"
%            | "}" | "~"
% ```

ptokenchar(0'!) --> "!".
ptokenchar(0'#) --> "#".
ptokenchar(0'$) --> "$".
ptokenchar(0'%) --> "%".
ptokenchar(0'&) --> "&".
ptokenchar(0'') --> "'".
ptokenchar(0'() --> "(".
ptokenchar(0')) --> ")".
ptokenchar(0'*) --> "*".
ptokenchar(0'+) --> "+".
ptokenchar(0'-) --> "-".
ptokenchar(0'.) --> ".".
ptokenchar(0'/) --> "/".
ptokenchar(C)   --> 'DIGIT'(_, C).
ptokenchar(0':) --> ":".
ptokenchar(0'<) --> "<".
ptokenchar(0'=) --> "=".
ptokenchar(0'>) --> ">".
ptokenchar(0'?) --> "?".
ptokenchar(0'@) --> "@".
ptokenchar(C)   --> 'ALPHA'(C).
ptokenchar(0'[) --> "[".
ptokenchar(0']) --> "]".
ptokenchar(0'^) --> "^".
ptokenchar(0'_) --> "_".
ptokenchar(0'`) --> "`".
ptokenchar(0'{) --> "{".
ptokenchar(0'|) --> "|".
ptokenchar(0'}) --> "}".
ptokenchar(0'~) --> "~".



%! 'quoted-mt'(-MediaType:dict)// is det.
% ```abnf
% quoted-mt = <"> media-type <">
% ```

'quoted-mt'(D) --> "\"", 'media-type'(D), "\"".



%! 'reg-rel-type'(-Value:string)// .
% ```abnf
% reg-rel-type   = LOALPHA *( LOALPHA | DIGIT | "." | "-" )
% ```

'reg-rel-type'(S) --> 'LOALPHA'(H), *(reg_rel_type_code, T), {string_codes(S, [H|T])}.

reg_rel_type_code(C)   --> 'LOALPHA'(C).
reg_rel_type_code(C)   --> 'DIGIT'(_, C).
reg_rel_type_code(0'.) --> ".".
reg_rel_type_code(0'-) --> "-".



%! 'relation-type'(-Value)// is det.
% ```abnf
% relation-type  = reg-rel-type | ext-rel-type
% ```

'relation-type'(S) --> 'reg-rel-type'(S).
'relation-type'(D) --> 'ext-rel-type'(D).



%! 'relation-types'(-RelationTypes:list)// is det.
% ```abnf
% relation-types = relation-type
%                | <"> relation-type *( 1*SP relation-type ) <">
% ```

'relation-types'([H|T]) --> "\"", !, 'relation-type'(H), *(sep_relation_type, T), "\"".
'relation-types'([H])   --> 'relation-type'(H).

sep_relation_type(X) --> +('SP'), 'relation-type'(X).
