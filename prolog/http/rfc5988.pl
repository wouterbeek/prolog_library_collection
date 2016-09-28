:- module(
  rfc5988,
  [
    link//1 % -Links:list(dict)
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
@version 2015/12, 2016/08-2016/09
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
     'ext-value'//1, % -Val:dict
     parmname//1     % -Name
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 % -LTag:list(string)
   ]).
:- use_module(library(uri/rfc3986), [
     'URI'//1,                             % -Uri:dict
     'URI-reference'//1 as 'URI-Reference' % -UriReference:dict
   ]).




%! 'ext-name-star'(-Name)// is det.
%
% ```abnf
% ext-name-star = parmname "*"   ; reserved for RFC2231-profiled
%                                ; extensions.  Whitespace NOT
%                                ; allowed in between.
% ```

'ext-name-star'(Str) --> parmname(Str), "*".



%! 'ext-rel-type'(-Uri:dict)// is det.
%
% ```abnf
% ext-rel-type = URI
% ```

'ext-rel-type'(D) --> 'URI'(D).



%! link(-Links:list(dict))// is det.
%
% ```abnf
% Link = "Link" ":" #link-value
% ```

link(L) --> *#('link-value', L).



%! 'link-extension'(-Pair)// is det.
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

'link-param'(rel-L) -->
  atom_ci(rel),
  "=", !,
  'relation-types'(L).
'link-param'(anchor-Uri) -->
  atom_ci(anchor),
  "=", !,
  "\"",
  'URI-Reference'(Uri),
  "\"".
'link-param'(rev-L) -->
  atom_ci(rev),
  "=", !,
  'relation-types'(L).
'link-param'(hreflang-LTag) -->
  atom_ci(hreflang),
  "=", !,
  'Language-Tag'(LTag).
'link-param'(media-L) -->
  atom_ci(media),
  "=", !,
  (   'MediaDesc'(L)
  ;   "\"",
      'MediaDesc'(L),
      "\""
  ), !.
'link-param'(title-S) -->
  atom_ci(title),
  "=", !,
  'quoted-string'(S).
'link-param'('title*'-S) -->
  atom_ci('title*'),
  "=", !,
  'ext-value'(S).
'link-param'(type-D) -->
  atom_ci(type),
  "=", !,
  (   'media-type'(D)
  ;   'quoted-mt'(D)
  ), !.
'link-param'(Pair) -->
  'link-extension'(Pair).



%! 'link-value'(-Val:dict)// is det.
%
% ```abnf
% link-value = "<" URI-Reference ">" *( ";" link-param )
% ```

'link-value'(link_value{params: Params, uri: Ref}) -->
  "<",
  'URI-Reference'(Ref),
  ">",
  *(sep_link_param, Pairs),
  {
    group_pairs_by_key(Pairs, GroupedPairs1),
    maplist(enforce_max_card, GroupedPairs1, GroupedPairs2),
    dict_pairs(Params, params, GroupedPairs2)
  }.


enforce_max_card(Key-[Val], Key-Val) :- !.
enforce_max_card(Key-[Val|_], Key-Val) :-
  at_most_once(Key), !.
enforce_max_card(Pair, Pair).


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



%! ptoken(-Token)// is det.
%
% ```abnf
% ptoken = 1*ptokenchar
% ```

ptoken(Str) -->
  +(ptokenchar, Cs),
  {string_codes(Str, Cs)}.



%! ptokenchar(?Code)// .
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



%! 'quoted-mt'(-MT)// is det.
%
% ```abnf
% quoted-mt = <"> media-type <">
% ```

'quoted-mt'(MT) --> "\"", 'media-type'(MT), "\"".



%! 'reg-rel-type'(-Val)// .
%
% ```abnf
% reg-rel-type   = LOALPHA *( LOALPHA | DIGIT | "." | "-" )
% ```

'reg-rel-type'(Str) -->
  'LOALPHA'(H),
  *(reg_rel_type_code, T),
  {string_codes(Str, [H|T])}.


reg_rel_type_code(C)   --> 'LOALPHA'(C).
reg_rel_type_code(C)   --> 'DIGIT'(_, C).
reg_rel_type_code(0'.) --> ".".
reg_rel_type_code(0'-) --> "-".



%! 'relation-type'(-Val)// is det.
%
% ```abnf
% relation-type  = reg-rel-type | ext-rel-type
% ```

'relation-type'(S) -->
  'reg-rel-type'(S).
'relation-type'(D) -->
  'ext-rel-type'(D).



%! 'relation-types'(-RelationTypes)// is det.
%
% ```abnf
% relation-types = relation-type
%                | <"> relation-type *( 1*SP relation-type ) <">
% ```

'relation-types'([H|T]) -->
  "\"", !,
  'relation-type'(H),
  *(sep_relation_type, T),
  "\"".
'relation-types'([H]) -->
  'relation-type'(H).


sep_relation_type(X) -->
  +('SP'),
  'relation-type'(X).
