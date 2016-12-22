:- module(
  rfc2295,
  [
    alternates//1 % -Alternates:list(compound)
  ]
).

/** <module> RFC 2295: Transparent Content Negotiation in HTTP

@author Wouter beek
@compat https://tools.ietf.org/html/rfc2295
@version 2016/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http11), [
     charset//1,     % -Charset:atom
     'media-type'//1 % -MT:compound
   ]).
:- use_module(library(http/rfc1945)).
:- use_module(library(http/rfc2616), [
     'quoted-string'//1, % -String:atom
     'LWS'//0,
     token//1            % -Token:atom
   ]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri/rfc3986)).





%! alternates(-Alternates:list(compound))// is det.
%
% “The Alternates response header is used to convey the list of
% variants bound to a negotiable resource.  This list can also include
% directives for any content negotiation process.  If a response from
% a transparently negotiable resource includes an Alternates header,
% this header MUST contain the complete variant list bound to the
% negotiable resource.  Responses from resources which do not support
% transparent content negotiation MAY also use Alternates headers.”
%
% ```abnf
% Alternates = "Alternates" ":" variant-list
% ```

alternates(Alternates) -->
  'variant-list'(Alternates).



%! 'extension-attribute'(-Attr:compound)// is det.
%
% ```abnf
% extension-attribute = "{" extension-name extension-value "}"
% ```

'extension-attribute'(Attr) -->
  "{",
  'extension-name'(Key),
  'extension-value'(Val),
  "}",
  {Attr =.. [Key,Val]}.



%! 'extension-list-directive'(-Dir:compound)// is det.
%
% ```abnf
% extension-list-directive = token [ "=" ( token | quoted-string ) ]
% ```

'extension-list-directive'(Dir) -->
  token(Key),
  (   "="
  ->  (token(Val) -> "" ; 'quoted-string'(Val)),
      {Dir =.. [Key,Val]}
  ;   {Dir = Key}
  ).



%! 'extension-name'(-Name:atom)// is det.
%
% ```abnf
% extension-name = token
% ```

'extension-name'(Name) -->
  token(Name).



%! 'extension-specials'(-A:atom)// is det.
%
% ```abnf
% extension-specials = <any element of tspecials except <"> and "}">
% ```

'extension-specials'(A) -->
  *(tspecials_except0, Cs), !,
  {atom_codes(A, Cs)}.

tspecials_except0(C) -->
  tspecials(C),
  {C \== 0'", C\== 0'}}. %"



%! 'extension-value'(-L:list(compound))// is det.
%
% ```abnf
% extension-value = *( token | quoted-string | LWS | extension-specials )
% ```

'extension-value'([H|T]) -->
  token(H),
  'extension-value'(T).
'extension-value'([H|T]) -->
  'quoted-string'(H),
  'extension-value'(T).
'extension-value'(L) -->
  'LWS',
  'extension-value'(L).
'extension-value'([H|T]) -->
  'extension-specials'(H),
  'extension-value'(T).
'extension-value'([]) --> "".



%! 'fallback-variant'(-Uri:compound)// .
%
% ```abnf
% fallback-variant = "{" <"> URI <"> "}"
% ```

'fallback-variant'(Uri) -->
  "{\"",
  'URI'(Uri),
  "\"}".



%! 'list-directive'(-Dir:compound)// is det.
%
% ```abnf
% list-directive = ( "proxy-rvsa" "=" <"> 0#rvsa-version <"> )
%                | extension-list-directive
% ````

'list-directive'(proxy_rvsa(Version)) -->
  "proxy-rvsa=\"", !,
  *(rvsa_version, Version),
  "\"".
'list-directive'(Dir) -->
  'extension-list-directive'(Dir).



%! 'rvsa-version'(-Version:pair(nonneg))// .
%
% ```abnf
% rvsa-version = major "." minor
% ```

'rvsa-version'(Major-Minor) -->
  major(Major),
  ".",
  minor(Minor).



%! major(-Major:nonneg)// is det.
%
% ```abnf
% major = 1*4DIGIT
% ```

major(Major) -->
  'm*n'(1, 4, 'DIGIT', Ds),
  {pos_num(Ds, Major)}.



%! minor(-Minor:nonneg)// is det.
%
% ```abnf
% minor = 1*4DIGIT
% ```

minor(Minor) -->
  'm*n'(1, 4, 'DIGIT', Ds),
  {pos_num(Ds, Minor)}.



%! 'variant-description'(-Variant:compount)// is det.
%
% ```abnf
% variant-description = "{" <"> URI <"> source-quality *variant-attribute"}"
% ```
%
% @bug: Should there be a space before the closing curly bracket to
%       separate it from the preceding production rule?

'variant-description'(variant(Uri,Q,Attrs)) -->
  "{\"",
  'URI'(Uri),
  "\"",
  'source-quality'(Q),
  *(variant_attribute, Attrs),
  "}".



%! 'source-quality'// .
%
% ```abnf
% source-quality = qvalue
% ```

'source-quality'(Q) -->
  qvalue(Q).



%! 'variant-attribute'(-Attr:compound)// .
%
% ```abnf
% variant-attribute = "{" "type" media-type "}"
%                   | "{" "charset" charset "}"
%                   | "{" "language"  1#language-tag "}"
%                   | "{" "length" 1*DIGIT "}"
%                   | "{" "features" feature-list "}"
%                   | "{" "description" quoted-string [ language-tag ] "}"
%                   | extension-attribute

'variant-attribute'(MT) -->
  "{type", !,
  'media-type'(MT),
  "}".
'variant-attribute'(charset(Charset)) -->
  "{charset", !,
  charset(Charset),
  "}".
'variant-attribute'(language(LTags)) -->
  "{language", !,
  +('language-tag', LTags),
  "}".
'variant-attribute'(length(Len)) -->
  "{length", !,
  +('DIGIT', Ds),
  {pos_sum(Ds, Len)}.
'variant-attribute'(features(L)) -->
  "{features",
  'feature-list'(L).
'variant-attribute'(description(Desc)) -->
  "{description", !,
  'quoted-string'(Str),
  ('language-tag'(LTag) -> {Desc = Str@LTag} ; {Desc = Str}),
  "}".
'variant-attribute'(Attr) -->
  'extension-attribute'(Attr).



%! 'variant-list'(-L:list(compound))// is det.
%
% ```abnf
% variant-list = 1#( variant-description | fallback-variant | list-directive )
% ```

'variant-list'(L) -->
  +(variant_comp, L).

variant_comp(Variant) -->
  'variant-description'(Variant).
variant_comp(Uri) -->
  'fallback-variant'(Uri).
variant_comp(Dir) -->
  'list-directive'(Dir).
