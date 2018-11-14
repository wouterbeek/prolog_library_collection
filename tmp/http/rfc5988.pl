:- encoding(utf8).
:- module(
  rfc5988,
  [
    link//1, % -Values
    link//2  % ?BaseUri, -Values
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
@version 2017/05-2017/07
*/

:- use_module(library(apply)).
:- use_module(library(dcg)).
:- use_module(library(dcg/html401), [
     'MediaDesc'//1 % -MediaDescriptions
   ]).
:- use_module(library(dcg/rfc5646), [
     'Language-Tag'//1 % -LanguageTag
   ]).
:- use_module(library(http/rfc2616), [
     'ALPHA'//1,         % ?Code
     'DIGIT'//1,         % ?Weight
     'DIGIT'//2,         % ?Weight, ?Code
     'LOALPHA'//1,       % ?Code
     'quoted-string'//1, % -String
     'SP'//0,
     token//1            % -Token
   ]).
:- use_module(library(http/rfc4288), [
     'subtype-name'//1, % -SubtypeName
     'type-name'//1     % -TypeName
   ]).
:- use_module(library(http/rfc5987), [
     'ext-value'//1, % -Value
     parmname//1     % -Name
   ]).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(uri/rfc3986), [
     'URI'//1,                             % -Uri
     'URI-reference'//2 as 'URI-Reference' % ?BaseUri, -Uri
   ]).
:- use_module(library(uri_ext)).

:- dynamic
    http:http_header/1,
    http:http_separable/1.

:- multifile
    http:http_header/1,
    http:http_separable/1.





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



%! 'ext-rel-type'(-RelationType:atom)// is det.
%
% ```abnf
% ext-rel-type = URI
% ```

'ext-rel-type'(RelationType) -->
  'URI'(UriComps),
  {uri_comps(RelationType, UriComps)}.



%! link(-Values:list(compound))// is det.
%! link(?BaseUri:atom, -Values:list(compound))// is det.
%
% ```abnf
% Link = #link-value
% ```

http:http_header(link).
http:http_separable(link).
link(Values) -->
  link(_, Values).


link(BaseUri, Values) -->
  *&('link-value'(BaseUri), (blanks,",",blanks), Values), !.



%! 'link-extension'(-LinkParameter:pair(atom))// is det.
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



%! 'link-param'(?BaseUri:atom, -LinkParameter:pair(atom,term))// is det.
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

'link-param'(_, rel-RelationTypes) -->
  atom_ci(rel),
  "=", !,
  'relation-types'(RelationTypes).
'link-param'(BaseUri, anchor-Uri) -->
  atom_ci(anchor),
  "=", !,
  "\"",
  'URI-Reference'(BaseUri, Uri),
  "\"".
'link-param'(_, rev-RelationTypes) -->
  atom_ci(rev),
  "=", !,
  'relation-types'(RelationTypes).
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



%! 'link-value'(?BaseUri:atom, -Value:compound)// is det.
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
    maplist(ord_union0, GroupedPairs, MergedPairs),
    maplist(enforce_max_cardinality, MergedPairs, Params)
  }.

ord_union0(Key-Values, Key-Value) :-
  ord_union(Values, Value).

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
  ";",
  whites,
  'link-param'(BaseUri, LinkParam).



%! 'media-type'(?MediaType:compound)// is det.
%
% ```abnf
% media-type = type-name "/" subtype-name
% ```

'media-type'(media(Type/Subtype,[])) -->
  'type-name'(Type),
  "/",
  'subtype-name'(Subtype).



%! ptoken(?Token:atom)// is det.
%
% ```abnf
% ptoken = 1*ptokenchar
% ```

ptoken(Token) -->
  dcg_atom(+(ptokenchar), Token).



%! ptokenchar(?Code:code)// is det.
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



%! 'quoted-mt'(-MediaType:compound)// is det.
%
% ```abnf
% quoted-mt = <"> media-type <">
% ```

'quoted-mt'(MT) -->
  "\"",
  'media-type'(MT),
  "\"".



%! 'reg-rel-type'(-RelationType:atom)// is det.
%
% ```abnf
% reg-rel-type = LOALPHA *( LOALPHA | DIGIT | "." | "-" )
% ```

'reg-rel-type'(RelationType) -->
  'LOALPHA'(H),
  *('reg-rel-type_', T),
  {atom_codes(RelationType, [H|T])}.

'reg-rel-type_'(Code) --> 'LOALPHA'(Code).
'reg-rel-type_'(Code) --> 'DIGIT'(_, Code).
'reg-rel-type_'(0'.) --> ".".
'reg-rel-type_'(0'-) --> "-".



%! 'relation-type'(-RelationType:atom)// is det.
%
% ```abnf
% relation-type  = reg-rel-type | ext-rel-type
% ```

'relation-type'(RelationType) -->
  'reg-rel-type'(RelationType), !.
'relation-type'(RelationType) -->
  'ext-rel-type'(RelationType).



%! 'relation-types'(-RelationTypes:list(atom))// is det.
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

sep_relation_type(RelationType) -->
  +('SP'), !,
  'relation-type'(RelationType).
