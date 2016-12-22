:- module(
  rfc5988,
  [
    link//2 % +BaseUri:atom, -LinkValues:list(compound)
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
@version 2015/12, 2016/08-2016/09, 2016/11-2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_dcg), [
     'MediaDesc'//1 % -MediaDescriptions:list(atom)
   ]).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/http11), [
     'OWS'//0
   ]).
:- use_module(library(http/rfc2616), [
     'ALPHA'//1,         % ?Code:code
     'DIGIT'//1,         % ?Weight:between(0,9)
     'DIGIT'//2,         % ?Weight:between(0,9), ?Code:code
     'LOALPHA'//1,       % ?Code:code
     'quoted-string'//1, % -String:atom
     'SP'//0
   ]).
:- use_module(library(http/rfc4288), [
     'subtype-name'//1, % -SubtypeName:atom
     'type-name'//1     % -TypeName:atom
   ]).
:- use_module(library(http/rfc5987), [
     'ext-value'//1, % -Val:compound
     parmname//1     % -Name:atom
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 % -LTag:list(atom)
   ]).
:- use_module(library(pair_ext)).
:- use_module(library(uri/rfc3986), [
     'URI'//1,          % -Uri:compound
     'URI-reference'//1 % -Uri:compound
   ]).
:- use_module(library(uri/uri_ext)).





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



%! 'ext-rel-type'(+BaseUri:atom, -RelType:atom)// is det.
%
% ```abnf
% ext-rel-type = URI
% ```

'ext-rel-type'(BaseUri, RelType) -->
  'URI'(UriComps),
  {uri_comps(RelType, BaseUri, UriComps)}.



%! link(+BaseUri:atom, -LinkValues:list(compound))// is det.
%
% ```abnf
% Link = "Link" ":" #link-value
% ```

link(BaseUri, LinkValues) -->
  *#('link-value'(BaseUri), LinkValues), !.



%! 'link-extension'(-LinkParam:pair(atom))// is det.
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



%! 'link-param'(+BaseUri:atom, -LinkParam:pair(atom,term))// is det.
%
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

'link-param'(BaseUri, rel-RelTypes) -->
  atom_ci(rel),
  "=", !,
  'relation-types'(BaseUri, RelTypes).
'link-param'(BaseUri, anchor-Uri) -->
  atom_ci(anchor),
  "=", !,
  "\"",
  'URI-Reference'(BaseUri, Uri),
  "\"".
'link-param'(BaseUri, rev-RelTypes) -->
  atom_ci(rev),
  "=", !,
  'relation-types'(BaseUri, RelTypes).
'link-param'(_, hreflang-LTag) -->
  atom_ci(hreflang),
  "=", !,
  'Language-Tag'(LTag).
'link-param'(_, media-MTs) -->
  atom_ci(media),
  "=", !,
  ("\"" -> 'MediaDesc'(MTs), "\"" ; 'MediaDesc'(MTs)).
'link-param'(_, title-Str) -->
  atom_ci(title),
  "=", !,
  'quoted-string'(Str).
'link-param'(_, 'title*'-Title) -->
  atom_ci('title*'),
  "=", !,
  'ext-value'(Title).
'link-param'(_, type-MT) -->
  atom_ci(type),
  "=", !,
  ('media-type'(MT) -> "" ; 'quoted-mt'(MT)).
'link-param'(_, LinkParam) -->
  'link-extension'(LinkParam).



%! 'link-value'(+BaseUri:atom, -LinkValue:compound)// is det.
%
% LinkValue has the form ‘link(Uri,Params)’, where Params is a list of
% keys value pairs in which the key is the link type.
%
% A *link value* is a pair whose key is an atomic URI and whose value
% is (i) an atom, in case there is only one value, and (ii) a list of
% atoms in case there are multiple values.
%
% The standard sets forth the following restriction on the maximum
% number of values allows for certain keys: ‘rel’, ‘title’, ‘title*’,
% and ‘type’ cannot have more than one value.  If a link value with
% either of those keys happens to have multiple values, the _first_
% one is given.
%
% ```abnf
% link-value = "<" URI-Reference ">" *( ";" link-param )
% ```

'link-value'(BaseUri, link(Uri,Params)) -->
  "<",
  'URI-Reference'(BaseUri, Uri),
  ">",
  *(sep_link_param(BaseUri), Pairs),
  {
    group_pairs_by_key(Pairs, GroupedPairs),
    maplist(pair_merge_value, GroupedPairs, MergedPairs),
    maplist(enforce_max_cardinality, MergedPairs, Params)
  }.

enforce_max_cardinality(Key-[Val], Key-Val) :- !.
% The ‘rel’, ‘title’, ‘title*’ and ‘type’ parameters _must not_ appear
% more than once in a given link-value; occurrences after the first
% _must_ be ignored by parsers.
enforce_max_cardinality(Key-[Val|_], Key-Val) :-
  at_most_once(Key), !.
enforce_max_cardinality(Pair, Pair).

at_most_once(rel).
at_most_once(title).
at_most_once('title*').
at_most_once(type).

sep_link_param(BaseUri, LinkParam) -->
  'OWS',
  ";",
  'OWS',
  'link-param'(BaseUri, LinkParam).



%! 'media-type'(-MT)// is det.
%
% ```abnf
% media-type = type-name "/" subtype-name
% ```

'media-type'(media(Type/Subtype,[])) -->
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



%! ptokenchar(-Code:code)// is det.
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



%! 'reg-rel-type'(-RelType:atom)// is det.
%
% ```abnf
% reg-rel-type = LOALPHA *( LOALPHA | DIGIT | "." | "-" )
% ```

'reg-rel-type'(RelType) -->
  'LOALPHA'(H),
  *(reg_rel_type_code, T), !,
  {atom_codes(RelType, [H|T])}.

reg_rel_type_code(C)   --> 'LOALPHA'(C).
reg_rel_type_code(C)   --> 'DIGIT'(_, C).
reg_rel_type_code(0'.) --> ".".
reg_rel_type_code(0'-) --> "-".



%! 'relation-type'(+BaseUri:atom, -RelType:atom)// is det.
%
% ```abnf
% relation-type  = reg-rel-type | ext-rel-type
% ```

'relation-type'(_, RelType) -->
  'reg-rel-type'(RelType), !.
'relation-type'(BaseUri, RelType) -->
  'ext-rel-type'(BaseUri, RelType).



%! 'relation-types'(+BaseUri:atom, -RelTypes:list(atom))// is det.
%
% ```abnf
% relation-types = relation-type
%                | <"> relation-type *( 1*SP relation-type ) <">
% ```

'relation-types'(BaseUri, [H|T]) -->
  "\"", !,
  must_see('relation-type'(BaseUri, H)),
  *(sep_relation_type(BaseUri), T), !,
  "\"".
'relation-types'(BaseUri, [H]) -->
  'relation-type'(BaseUri, H).

sep_relation_type(BaseUri, RelType) -->
  +('SP'), !,
  'relation-type'(BaseUri, RelType).



%! 'URI-Reference'(+BaseUri:atom, -Uri:atom)// is det.

'URI-Reference'(BaseUri, Uri) -->
  'URI-reference'(UriComps),
  {uri_comps(Uri, BaseUri, UriComps)}.
