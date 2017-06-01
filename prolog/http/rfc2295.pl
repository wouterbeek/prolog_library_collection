:- module(
  rfc2295,
  [
    alternates//1, % -Alternates:list(compound)
    tcn//1         % -L
  ]
).

/** <module> RFC 2295: Transparent Content Negotiation in HTTP

@author Wouter beek
@compat https://tools.ietf.org/html/rfc2295
@version 2016/12
*/

:- use_module(library(dcg/dcg_ext), except([number//1])).
:- use_module(library(http/http11), [
     charset//1,      % -Charset:atom
     'media-type'//1, % -MT:compound
     qvalue//1        % -Val:between(0.0,1.0)
   ]).
:- use_module(library(http/rfc1945)).
:- use_module(library(http/rfc2616), [
     'quoted-string'//1, % -String:atom
     'LWS'//0,
     token//1            % -Token:atom
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 as 'language-tag' % -LTag:list(atom)
   ]).
:- use_module(library(semweb/rdf11), [
     op(110, xfx, @)
   ]).
:- use_module(library(uri/rfc1738), [
     digit//1 as 'DIGIT' % ?Weight:between(0,9)
   ]).
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
% ```bnf
% Alternates = "Alternates" ":" variant-list
% ```

alternates(Alternates) -->
  'variant-list'(Alternates).



%! 'extension-attribute'(-Attr:compound)// is det.
%
% ```bnf
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
% ```bnf
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
% ```bnf
% extension-name = token
% ```

'extension-name'(Name) -->
  token(Name).



%! 'extension-specials'(-A:atom)// is det.
%
% ```bnf
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
% ```bnf
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
% ```bnf
% fallback-variant = "{" <"> URI <"> "}"
% ```

'fallback-variant'(Uri) -->
  "{\"",
  'URI'(Uri),
  "\"}".



%! 'false-degradation'(-N:float)// .
%
% ```bnf
% false-degradation = short-float
% ```

'false-degradation'(N) -->
  'short-float'(N).



%! 'feature-list'(-Pairs)// .
%
% ```bnf
% feature-list = 1%feature-list-element
% ```

'feature-list'([H|T]) -->
  'feature-list-element'(H),
  'feature-list-element*'(T).

'feature-list-element*'([H|T]) -->
  whites,
  'feature-list-element'(H), !,
  'feature-list-element*'(T).
'feature-list-element*'([]) --> "".



%! 'feature-list-element'(-Pair)// .
%
% ```bnf
% feature-list-element = ( fpred | fpred-bag )
%                        [ ";" [ "+" true-improvement  ]
%                              [ "-" false-degradation ]
%                        ]
% ```

'feature-list-element'(N-L) -->
  (fpred(H) -> {L = [H]} ; 'fpred-bag'(L)),
  (   ";"
  ->  ("+" -> 'true-improvement'(N) ; ""),
      ("-" -> 'false-degradation'(N0), {N is -N0} ; "")
  ;   ""
  ).



%! fpred(-X)// .
%
% ```
% fpred = [ "!" ] ftag
%       | ftag ( "=" | "!=" ) tag-value
%       | ftag "=" "[" numeric-range "]"
% ```

fpred(not(Tag)) -->
  "!", !,
  ftag(Tag).
fpred(Key=Val) -->
  ftag(Key),
  "=",
  'tag-value'(Val).
fpred(not(Key=Val)) -->
  ftag(Key),
  "!=",
  'tag-value'(Val).
fpred(Key=Range) -->
  ftag(Key),
  "=[",
  'numeric-range'(Range),
  "]".
fpred(Tag) -->
  ftag(Tag).



%! 'fpred-bag'(-L)// .
%
% ```bnf
% fpred-bag = "[" 1%fpred "]"
% ```

'fpred-bag'(L) -->
  "[",
  'fpred+'(L),
  "]".

'fpred+'([H|T]) -->
  fpred(H),
  'fpred*'(T).

'fpred*'([H|T]) -->
  whites,
  fpred(H), !,
  'fpred*'(T).
'fpred*'([]) --> "".



%! ftag(-Tag)// .
%
% ```bnf
% ftag = token | quoted-string
% ```

ftag(Tag) --> token(Tag).
ftag(Tag) --> 'quoted-string'(Tag).



%! 'list-directive'(-Dir:compound)// is det.
%
% ```bnf
% list-directive = ( "proxy-rvsa" "=" <"> 0#rvsa-version <"> )
%                | extension-list-directive
% ````

'list-directive'(proxy_rvsa(Version)) -->
  "proxy-rvsa=\"", !,
  *('rvsa-version', Version),
  "\"".
'list-directive'(Dir) -->
  'extension-list-directive'(Dir).



%! major(-Major:nonneg)// is det.
%
% ```bnf
% major = 1*4DIGIT
% ```

major(Major) -->
  'm*n'(1, 4, 'DIGIT', Weights),
  {integer_weights(Major, Weights)}.



%! minor(-Minor:nonneg)// is det.
%
% ```bnf
% minor = 1*4DIGIT
% ```

minor(Minor) -->
  'm*n'(1, 4, 'DIGIT', Weights),
  {integer_weights(Minor, Weights)}.



%! number(-N:nonneg)// .
%
% ```bnf
% number = 1*DIGIT
% ```

number(N) -->
  +('DIGIT', Weights),
  {integer_weights(N, Weights)}.



%! 'numeric-range'(-Range:pair)// .
%
% ```
% numeric-range = [ number ] "-" [ number ]
% ```

'numeric-range'(N1-N2) -->
  ?(number, N1),
  "-",
  ?(number, N2).



%! 'response-type'(-Type)// .
%
% ```bnf
% response-type = "list" | "choice" | "adhoc"
% ```

'response-type'(list) --> "list".
'response-type'(choice) --> "choice".
'response-type'(adhoc) --> "adhoc".



%! 'rvsa-version'(-Version:pair(nonneg))// .
%
% ```bnf
% rvsa-version = major "." minor
% ```

'rvsa-version'(Major-Minor) -->
  major(Major),
  ".",
  minor(Minor).



%! 'server-side-override-directive'// .
%
% ```bnf
% server-side-override-directive = "re-choose" | "keep"
% ```

'server-side-override-directive'('re-choose') --> "re-choose".
'server-side-override-directive'(keep) --> "keep".



%! 'short-float'(-N:float)// .
%
% ```
% short-float = 1*3DIGIT [ "." 0*3DIGIT ]
% ```

'short-float'(N2) -->
  'm*n'(1, 3, 'DIGIT', Weights1),
  {integer_weights(N1, Weights1)},
  ("." -> 'm*n'(0, 3, 'DIGIT', Weights2) ; ""),
  {
    fractional_weights(Frac, Weights2),
    N2 is N1 + Frac
  }.



%! 'source-quality'// .
%
% ```bnf
% source-quality = qvalue
% ```

'source-quality'(Q) -->
  qvalue(Q).



%! 'tag-value'(-Tag:atom)// .
%
% ```bnf
% tag-value = token | quoted-string
% ```

'tag-value'(Tag) --> token(Tag).
'tag-value'(Tag) --> 'quoted-string'(Tag).



%! tcn(-L)// is det.
%
% ```bnf
% TCN = "TCN" ":" #( response-type
%                  | server-side-override-directive
%                  | tcn-extension )
% ```

tcn(L) --> *(tcn0, L).

tcn0(X) --> 'response-type'(X).
tcn0(X) --> 'server-side-override-directive'(X).
tcn0(X) --> 'tcn-extension'(X).



%! 'tcn-extension'// .
%
% ```bnf
% tcn-extension = token [ "=" ( token | quoted-string ) ]
% ```

'tcn-extension'(Ext) -->
  token(Key),
  (   "="
  ->  (token(Val) -> "" ; 'quoted-string'(Val)),
      {Ext =.. [Key,Val]}
  ;   {Ext = Key}
  ).


                                                      
%! 'true-improvement'(-N:float)// .
%
% ```bnf
% true-improvement = short-float
% ```

'true-improvement'(N) -->
  'short-float'(N).



%! 'variant-attribute'(-Attr:compound)// .
%
% ```bnf
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
  +('DIGIT', Weights),
  {integer_weights(Len, Weights)}.
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



%! 'variant-description'(-Variant:compount)// is det.
%
% ```bnf
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
  *('variant-attribute', Attrs),
  "}".



%! 'variant-list'(-L:list(compound))// is det.
%
% ```bnf
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
