:- module(
  rfc5646,
  [
    'Language-Tag'//1 % -LanguageTag:dict
  ]
).
:- reexport(
  library(ltag/rfc4646),
  [
    alphanum//1, % -Code:code
    extension//1, % -Extension:list(string)
    langtag//1, % -LanguageTag:list(string)
    language//1, % -LanguageTag:list(string)
    region//1, % -Region:string
    script//1, % -Script:string
    variant//1 % -Variant:string
  ]
).

/** <module> Language tag

@author Wouter Beek
@compat RFC 5646
@see https://tools.ietf.org/html/rfc5646
@version 2015/09, 2015/11-2016/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % -Code:code
     'DIGIT'//2 % -Weight:between(0,9), -Code:code
   ]).
:- use_module(library(lists)).
:- use_module(library(string_ext)).





%! extlang(-Ext:list(string))// is det.
% RFC 4646 allows this to match the empty string as well.
%
% ```abnf
% extlang = 3ALPHA           ; selected ISO 639 codes
%           *2("-" 3ALPHA)   ; permanently reserved
% ```

extlang([H|T]) -->
  #(3, 'ALPHA', Cs),
  {string_codes(H, Cs)},
  '*n'(2, permanently_reserved, T).
permanently_reserved(S) -->
  "-",
  #(3, 'ALPHA', Cs),
  {string_codes(S, Cs)}.



%! gransfathered(-GrandfatheredLanguageTag:list(string))// is det.
% ```abnf
% grandfathered = irregular   ; non-redundant tags registered
%               / regular     ; during the RFC 3066 era
% ```

grandfathered(L) --> irregular(L), !.
grandfathered(L) --> regular(L).



%! irregular(-IrregularLanguageTag:list(string))// is det.
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
% ```

irregular(["en","GB","oed"]) --> "en-GB-oed", !.
irregular(["i","ami"])       --> "i-ami", !.
irregular(["i","bnn"])       --> "i-bnn", !.
irregular(["i","default"])   --> "i-default", !.
irregular(["i","enochian"])  --> "i-enochian", !.
irregular(["i","hak"])       --> "i-hak", !.
irregular(["i","klingon"])   --> "i-klingon", !.
irregular(["i","lux"])       --> "i-lux", !.
irregular(["i","mingo"])     --> "i-mingo", !.
irregular(["i","navajo"])    --> "i-navajo", !.
irregular(["i","pwn"])       --> "i-pwn", !.
irregular(["i","tao"])       --> "i-tao", !.
irregular(["i","tay"])       --> "i-tay", !.
irregular(["i","tsu"])       --> "i-tsu", !.
irregular(["sgn","BE","FR"]) --> "sgn-BE-FR", !.
irregular(["sgn","BE","NL"]) --> "sgn-BE-NL", !.
irregular(["sgn","CH","DE"]) --> "sgn-CH-DE".



%! 'Language-Tag'(-LanguageTag:dict)// is det.
% ```abnf
% Language-Tag = langtag         ; normal language tags
%              / privateuse      ; private use tag
%              / grandfathered   ; grandfathered tags
% ```

'Language-Tag'(language_tag{'@type': Class, 'rdf:value': L}) -->
  (   langtag(L)
  ->  {Class = 'ltag:NormalLanguageTag'}
  ;   privateuse(L)
  ->  {Class = 'ltag:PrivateUseTag'}
  ;   grandfathered(L)
  ->  {Class = 'ltag:GrandfatheredTag'}
  ).



%! 'obs-language-tag'(-ObsoluteLanguageTag:list(string))// is det.
% ```abnf
% obs-language-tag = primary-subtag *( "-" subtag )
% ```

'obs-language-tag'([H|T]) -->
  'primary-subtag'(H),
  *(sep_subtag, T).

sep_subtag(S) --> "-", subtag(S).



%! 'primary-subtag'(-PrimarySubTag:string)// is det.
% ```abnf
% primary-subtag = 1*8ALPHA
% ```

'primary-subtag'(S) -->
  'm*n'(1, 8, 'ALPHA', Cs),
  {string_codes(S, Cs)}.



%! privateuse(-PrivateUse:list(string))// is det.
% ```abnf
% privateuse = "x" 1*("-" (1*8alphanum))
% ```

privateuse(["x"|T]) -->
  atom_ci(x),
  +(privateuse_tail, T).
privateuse_tail(S) -->
  "-",
  'm*n'(1, 8, alphanum, Cs),
  {string_codes(S, Cs)}.



%! regular(-RegularLanguageTag:list(string))// is det.
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

regular(["art","lojban"])   --> "art-lojban", !.
regular(["cel","gaulish"])  --> "cel-gaulish", !.
regular(["no","bok"])       --> "no-bok", !.
regular(["no","nyn"])       --> "no-nyn", !.
regular(["zh","guoyu"])     --> "zh-guoyu", !.
regular(["zh","hakka"])     --> "zh-hakka", !.
regular(["zh","min"])       --> "zh-min", !.
regular(["zh","min","nan"]) --> "zh-min-nan", !.
regular(["zh","xiang"])     --> "zh-xiang".



%! singleton(?Singleton:code)// .
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



%! subtag(-SubTag:string)// is det.
% ```abnf
% subtag = 1*8(ALPHA / DIGIT)
% ```

subtag(S) -->
  'm*n'(1, 8, subtag_code, Cs),
  {string_codes(S, Cs)}.

subtag_code(C) --> 'ALPHA'(C).
subtag_code(C) --> 'DIGIT'(_, C).
