:- module(
  rfc5646,
  [
    'Language-Tag'//1 % -LanguageTag:list(atom)
  ]
).

/** <module> RFC 5646 - Tags for Identifying Languages

@author Wouter Beek
@compat RFC 5646
@see https://tools.ietf.org/html/rfc5646
@version 2017/05-2017/06
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc5234)).
:- use_module(library(lists)).





%! alphanum(?Code:code)// .
%
% ```abnf
% alphanum = (ALPHA / DIGIT)   ; letters and numbers
% ```



%! extlang(-Extension:list(atom))// .
%
% RFC 4646 allows this to match the empty string as well.
%
% ```abnf
% extlang = 3ALPHA           ; selected ISO 639 codes
%           *2("-" 3ALPHA)   ; permanently reserved
% ```

extlang([H|T]) -->
  #(3, 'ALPHA', Cs),
  {atom_codes(H, Cs)},
  '*n'(2, permanently_reserved0, T).

permanently_reserved0(S) -->
  "-",
  #(3, 'ALPHA', Cs),
  {atom_codes(S, Cs)}.




%! extension(-Extension:list(atom))// .
%
% Single alphanumerics "x" reserved for private use.
%
% ```abnf
% extension = singleton 1*("-" (2*8alphanum))

extension([H|T]) -->
  singleton(H),
  +(extension0, T).

extension0(Extension) -->
  "-",
  'm*n'(2, 8, alphanum, Cs),
  {atom_codes(Extension, Cs)}.



%! granDfathered(-GrandfatheredTag:list(atom))// .
%
% ```abnf
% grandfathered = irregular   ; non-redundant tags registered
%               / regular     ; during the RFC 3066 era
% ```

grandfathered(L) --> irregular(L), !.
grandfathered(L) --> regular(L).



%! irregular(-IrregularTag:list(atom))// .
%
% ```abnf
% irregular = "en-GB-oed"    ; irregular tags do not match
%           / "i-ami"        ; the 'langtag' production and
%           / "i-bnn"        ; would not otherwise be
%           / "i-default"    ; considered 'well-formed'
%           / "i-enochian"   ; These tags are all valid,
%           / "i-hak"        ; but most are deprecated
%           / "i-klingon"    ; in favor of more modern
%           / "i-lux"        ; subtags or subtag
%           / "i-mingo"      ; combination
%           / "i-navajo"
%           / "i-pwn"
%           / "i-tao"
%           / "i-tay"
%           / "i-tsu"
%           / "sgn-BE-FR"
%           / "sgn-BE-NL"
%           / "sgn-CH-DE"
% ```

irregular([en,'GB',oed]) --> "en-GB-oed", !.
irregular([i,ami]) --> "i-ami", !.
irregular([i,bnn]) --> "i-bnn", !.
irregular([i,default]) --> "i-default", !.
irregular([i,enochian]) --> "i-enochian", !.
irregular([i,hak]) --> "i-hak", !.
irregular([i,klingon]) --> "i-klingon", !.
irregular([i,lux]) --> "i-lux", !.
irregular([i,mingo]) --> "i-mingo", !.
irregular([i,navajo]) --> "i-navajo", !.
irregular([i,pwn]) --> "i-pwn", !.
irregular([i,tao]) --> "i-tao", !.
irregular([i,tay]) --> "i-tay", !.
irregular([i,tsu]) --> "i-tsu", !.
irregular([sgn,'BE','FR']) --> "sgn-BE-FR", !.
irregular([sgn,'BE','NL']) --> "sgn-BE-NL", !.
irregular([sgn,'CH','DE']) --> "sgn-CH-DE".



%! langtag(-Tag:list(atom))// .
%
% ```abnf
% langtag = language
%           ["-" script]
%           ["-" region]
%           *("-" variant)
%           *("-" extension)
%           ["-" privateuse]
% ```

langtag(Tag) -->
  language(Language),
  ("-" -> script(Script) ; ""),
  ("-" -> region(Region) ; ""),
  *(sep_variant0, Variants),
  *(sep_extension0, Extensions),
  ("-" -> privateuse(PrivateUse) ; ""),
  {
    append([[Language,Script,Region],Variants,Extensions,[PrivateUse]], Tag0),
    include(ground, Tag0, Tag)
  }.

sep_variant0(Variant) --> "-", variant(Variant).
sep_extension0(Extension) --> "-", extension(Extension).



%! language// .
%
% ```abnf
% language = 2*3ALPHA        ; shortest ISO 639 code
%            ["-" extlang]   ; sometimes followed by
%                            ; extended language subtags
%          / 4ALPHA          ; or reserved for future use
%          / 5*8ALPHA        ; or registered language subtag
% ```

language(L) -->
  'm*n'(2, 3, 'ALPHA', Cs),
  {atom_codes(H1, Cs)},
  ("-" -> extlang(H2), {L = [H1,H2]} ; {L = [H1]}).
language([H]) -->
  '#'(4, 'ALPHA', Cs),
  {atom_codes(H, Cs)}.
language([H]) -->
  'm*n'(5, 8, 'ALPHA', Cs),
  {atom_codes(H, Cs)}.



%! 'Language-Tag'(-LanguageTag:atom)// .
%
% ```abnf
% Language-Tag = langtag         ; normal language tags
%              / privateuse      ; private use tag
%              / grandfathered   ; grandfathered tags
% ```

'Language-Tag'(Tag) --> langtag(Tag).
'Language-Tag'(Tag) --> privateuse(Tag).
'Language-Tag'(Tag) --> grandfathered(Tag).



%! 'obs-language-tag'(-ObsoluteTag:list(atom))// .
%
% ```abnf
% obs-language-tag = primary-subtag *( "-" subtag )
% ```

'obs-language-tag'([H|T]) -->
  'primary-subtag'(H),
  *(sep_subtag0, T).

sep_subtag0(S) -->
  "-",
  subtag(S).



%! 'primary-subtag'(-PrimarySubtag:atom)// .
%
% ```abnf
% primary-subtag = 1*8ALPHA
% ```

'primary-subtag'(Subtag) -->
  'm*n'(1, 8, 'ALPHA', Cs),
  {atom_codes(Subtag, Cs)}.



%! privateuse(-PrivateUse:list(atom))// .
%
% ```abnf
% privateuse = "x" 1*("-" (1*8alphanum))
% ```

privateuse([x|T]) -->
  atom_ci(x),
  +(privateuse_tail0, T).

privateuse_tail0(A) -->
  "-",
  'm*n'(1, 8, alphanum, Cs),
  {atom_codes(A, Cs)}.



%! region(-Region:atom)// .
%
% ```abnf
% region = 2ALPHA   ; ISO 3166-1 code
%        / 3DIGIT   ; UN M.49 code
% ```

region(Region) -->
  ('#'(2, 'ALPHA', Cs) -> "" ; '#'(3, 'DIGIT', Cs)),
  {atom_codes(Region, Cs)}.



%! regular(-RegularTag:list(atom))// .
%
% ```abnf
% regular = "art-lojban"    ; these tags match the 'langtag'
%         / "cel-gaulish"   ; production, but their subtags
%         / "no-bok"        ; are not extended language
%         / "no-nyn"        ; or variant subtags: their meaning
%         / "zh-guoyu"      ; is defined by their registration
%         / "zh-hakka"      ; and all of these are deprecated
%         / "zh-min"        ; in favor of a more modern
%         / "zh-min-nan"    ; subtag or sequence of subtags
%         / "zh-xiang"
% ```

regular([art,lojban]) --> "art-lojban", !.
regular([cel,gaulish]) --> "cel-gaulish", !.
regular([no,bok]) --> "no-bok", !.
regular([no,nyn]) --> "no-nyn", !.
regular([zh,guoyu]) --> "zh-guoyu", !.
regular([zh,hakka]) --> "zh-hakka", !.
regular([zh,min]) --> "zh-min", !.
regular([zh,min,nan]) --> "zh-min-nan", !.
regular([zh,xiang]) --> "zh-xiang".



%! script(-Script:atom)// .
%
% ```abnf
% script = 4ALPHA   ; ISO 15924 code
% ```

script(Script) -->
  '#'(4, 'ALPHA', Cs),
  {atom_codes(Script, Cs)}.



%! singleton(?Singleton:code)// .
%
% RFC 4646 defines this rule in a different order.
%
% ```abnf
% singleton = DIGIT     ; 0 - 9
%           / %x41-57   ; A - W
%           / %x59-5A   ; Y - Z
%           / %x61-77   ; a - w
%           / %x79-7A   ; y - z

singleton(C) --> 'DIGIT'(_, C).
singleton(C) -->
  [C],
  {once((
    between(0x41, 0x57, C) ;
    between(0x59, 0x5A, C) ;
    between(0x61, 0x77, C) ;
    between(0x79, 0x7A, C)
  ))}.



%! subtag(-Subtag:atom)// .
%
% ```abnf
% subtag = 1*8(ALPHA / DIGIT)
% ```

subtag(Subtag) -->
  'm*n'(1, 8, subtag_code0, Cs),
  {atom_codes(Subtag, Cs)}.

subtag_code0(C) --> 'ALPHA'(C).
subtag_code0(C) --> 'DIGIT'(_, C).



%! variant(-Variant:atom)// .
%
% ```abnf
% variant       = 5*8alphanum         ; registered variants
%               / (DIGIT 3alphanum)
% ```

variant(Variant) -->
  (   'DIGIT'(H)
  ->  '#'(3, alphanum, T),
      {Cs = [H|T]}
  ;   'm*n'(5, 8, alphanum, Cs)
  ),
  {atom_codes(Variant, Cs)}.
