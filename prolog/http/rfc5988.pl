:- module(
  rfc5988,
  [
    link//1 % -Links:list(pair)
  ]
).

/** <module> RFC 5988: Web Linking

Typed links are comprised of:

  - context IRI

  - link relation type

    - identifies relation semantics, e.g., ‘copyright’

    - characterizes the target resource, e.g., ‘service’

    - _not_ media types

    - only the semantics of ‘alternate’ and ‘stylesheet’ interact
   
    - ‘rel’: at most once

  - target IRI

  - optional target attributes, e.g., media type hint.

    - ‘hreflang’: target content language hint; weaker than
      `Content-Language` header.  Multiple values for multiple
      languages.

    - ‘media’: intended destination medium/media [?]

    - ‘title’: human-readable destination label in the
      `Content-Language` (if present); at most once

    - ‘title*’: [?]

    - ‘type’: hint of target media type; weaker than `Content-Type`

Examples:

```http
Link: <http://example.com/TheBook/chapter2>; rel="previous"; title="previous chapter"

Link: </>; rel="http://example.net/foo"

Link: </TheBook/chapter2>; rel="previous"; title*=UTF-8'de'letztes%20Kapitel, </TheBook/chapter4>; rel="next"; title*=UTF-8'de'n%c3%a4chstes%20Kapitel

Link: <http://example.org/>; rel="start http://example.net/relation/other"
```

---

@author Wouter Beek
@compat RFC 5988
@see https://tools.ietf.org/html/rfc5988
@version 2015/12, 2016/08-2016/09, 2016/11
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
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
     'quoted-string'//1, % -String
     'SP'//0
   ]).
:- use_module(library(http/rfc4288), [
     'subtype-name'//1, % -SubtypeName
     'type-name'//1     % -TypeName
   ]).
:- use_module(library(http/rfc5987), [
     'ext-value'//1, % -Val:compound
     parmname//1     % -Name:atom
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 % -LTag:list(atom)
   ]).
:- use_module(library(uri/rfc3986), [
     'URI'//1,                             % -Uri:compound
     'URI-reference'//1 as 'URI-Reference' % -UriReference:compound
   ]).





%! 'ext-name-star'(-Name:atom)// is det.
%
% ```abnf
% ext-name-star = parmname "*"   ; reserved for RFC2231-profiled
%                                ; extensions.  Whitespace NOT
%                                ; allowed in between.
% ```

'ext-name-star'(Name) -->
  parmname(Name),
  "*".



%! 'ext-rel-type'(-Uri:compound)// is det.
%
% ```abnf
% ext-rel-type = URI
% ```

'ext-rel-type'(Uri) -->
  'URI'(Uri).



%! link(-Links:list(pair))// is det.
%
% ```abnf
% Link = "Link" ":" #link-value
% ```

link(Links) -->
  *#('link-value', Links).



%! 'link-extension'(-Pair:pair(atom))// is det.
%
% ```abnf
% link-extension = ( parmname [ "=" ( ptoken | quoted-string ) ] )
%                 | ( ext-name-star "=" ext-value )
% ```

'link-extension'(Key-Val) -->
  'ext-name-star'(Key), !,
  "=",
  'ext-value'(Val).
'link-extension'(Key-Val) -->
  parmname(Key),
  (   "="
  ->  (   ptoken(Val), !
      ;   'quoted-string'(Val)
      )
  ;   {Val = true}
  ).



%! 'link-param'(-Pair)// is det.
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

'link-param'(rel-Types) -->
  atom_ci(rel),
  "=", !,
  'relation-types'(Types).
'link-param'(anchor-Uri) -->
  atom_ci(anchor),
  "=", !,
  "\"",
  'URI-Reference'(Uri),
  "\"".
'link-param'(rev-Types) -->
  atom_ci(rev),
  "=", !,
  'relation-types'(Types).
'link-param'(hreflang-LTag) -->
  atom_ci(hreflang),
  "=", !,
  'Language-Tag'(LTag).
'link-param'(media-MTs) -->
  atom_ci(media),
  "=", !,
  (   'MediaDesc'(MTs)
  ;   "\"",
      'MediaDesc'(MTs),
      "\""
  ), !.
'link-param'(title-Str) -->
  atom_ci(title),
  "=", !,
  'quoted-string'(Str).
'link-param'('title*'-Title) -->
  atom_ci('title*'),
  "=", !,
  'ext-value'(Title).
'link-param'(type-MT) -->
  atom_ci(type),
  "=", !,
  (   'media-type'(MT)
  ;   'quoted-mt'(MT)
  ), !.
'link-param'(Pair) -->
  'link-extension'(Pair).



%! 'link-value'(-Val)// is det.
%
% ```abnf
% link-value = "<" URI-Reference ">" *( ";" link-param )
% ```

'link-value'(Ref-Params) -->
  "<",
  'URI-Reference'(Ref),
  ">",
  *(sep_link_param, Pairs),
  {
    group_pairs_by_key(Pairs, GroupedPairs),
    maplist(enforce_max_cardinality, GroupedPairs, Params)
  }.

enforce_max_cardinality(Key-[Val], Key-Val) :- !.
enforce_max_cardinality(Key-[Val|_], Key-Val) :-
  at_most_once(Key), !.
enforce_max_cardinality(Pair, Pair).

at_most_once(rel).
at_most_once(title).
at_most_once('title*').
at_most_once(type).

sep_link_param(Pair) -->
  'OWS',
  ";",
  'OWS',
  'link-param'(Pair).



%! 'media-type'(-MT)// is det.
%
% ```abnf
% media-type = type-name "/" subtype-name
% ```

'media-type'(media_type(Type,Subtype,[])) -->
  'type-name'(Type),
  "/",
  'subtype-name'(Subtype).



%! ptoken(-Token:atom)// is det.
%
% ```abnf
% ptoken = 1*ptokenchar
% ```

ptoken(Token) -->
  +(ptokenchar, Cs), !,
  {atom_codes(Token, Cs)}.



%! ptokenchar(-Code:code)// .
%
% ```abnf
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



%! 'quoted-mt'(-MT:compound)// is det.
%
% ```abnf
% quoted-mt = <"> media-type <">
% ```

'quoted-mt'(MT) -->
  "\"",
  'media-type'(MT),
  "\"".



%! 'reg-rel-type'(-Type:atom)// .
%
% ```abnf
% reg-rel-type   = LOALPHA *( LOALPHA | DIGIT | "." | "-" )
% ```

'reg-rel-type'(Type) -->
  'LOALPHA'(H),
  *(reg_rel_type_code, T), !,
  {atom_codes(Type, [H|T])}.

reg_rel_type_code(C)   --> 'LOALPHA'(C).
reg_rel_type_code(C)   --> 'DIGIT'(_, C).
reg_rel_type_code(0'.) --> ".".
reg_rel_type_code(0'-) --> "-".



%! 'relation-type'(-Type:atom)// is det.
%
% ```abnf
% relation-type  = reg-rel-type | ext-rel-type
% ```

'relation-type'(Type) -->
  'reg-rel-type'(Type), !.
'relation-type'(Type) -->
  'ext-rel-type'(UriComps),
  {uri_comps(Type, UriComps)}.



%! 'relation-types'(-Types:list(atom))// is det.
%
% ```abnf
% relation-types = relation-type
%                | <"> relation-type *( 1*SP relation-type ) <">
% ```

'relation-types'([H|T]) -->
  "\"", !,
  must_see('relation-type'(H)),
  *(sep_relation_type, T), !,
  "\"".
'relation-types'([H]) -->
  'relation-type'(H).

sep_relation_type(X) -->
  +('SP'),
  'relation-type'(X).
