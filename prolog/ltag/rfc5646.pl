:- module(
  rfc5646,
  [
    extlang//1, % ?Extension:list(string)
    grandfathered//1, % ?LanguageTag:list(string)
    irregular//1, % ?LanguageTag:list(string)
    'Language-Tag'//1, % ?LanguageTag:list(string)
    'obs-language-tag'//1, % ?LanguageTag:list(string)
    'primary-subtag'//1, % ?SubTag:string
    privateuse//1, % ?LanguageTag:list(string)
    regular//1, % ?LanguageTag:list(string)
    singleton//1, % ?Code:code
    subtag//1 % ?SubTag:string
  ]
).
:- reexport(
  library(ltag/rfc4646),
  [
    alphanum//1, % ?Code:code
    extension//1, % ?Extension:list(string)
    langtag//1, % ?LanguageTag:list(string)
    language//1, % ?LanguageTag:list(string)
    region//1, % ?Region:string
    script//1, % ?Script:string
    variant//1 % ?Variant:string
  ]
).

/** <module> Language tag

@author Wouter Beek
@compat RFC 5646
@version 2015/09, 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(lists)).





%! extlang(?Ext:list(string))// .
% RFC 4646 allows this to match the empty string as well.
%
% ```abnf
% extlang = 3ALPHA              ; selected ISO 639 codes
%           *2("-" 3ALPHA)      ; permanently reserved
% ```

extlang([H|T]) -->
  #(3, 'ALPHA', H, [convert(1-string)]),
  '*n'(2, permanently_reserved, T, []).
permanently_reserved(S) --> "-", #(3, 'ALPHA', S, [convert(1-string)]).



%! gransfathered(?GrandfatheredLanguageTag:list(string))// .
% ```abnf
% grandfathered = irregular   ; non-redundant tags registered
%               / regular     ; during the RFC 3066 era
% ```

grandfathered(L) --> irregular(L).
grandfathered(L) --> regular(L).



%! irregular(?IrregularLanguageTag:list(string))// .
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

irregular(["en","GB","oed"]) --> "en-GB-oed".
irregular(["i","ami"]) --> "i-ami".
irregular(["i","bnn"]) --> "i-bnn".
irregular(["i","default"]) --> "i-default".
irregular(["i","enochian"]) --> "i-enochian".
irregular(["i","hak"]) --> "i-hak".
irregular(["i","klingon"]) --> "i-klingon".
irregular(["i","lux"]) --> "i-lux".
irregular(["i","mingo"]) --> "i-mingo".
irregular(["i","navajo"]) --> "i-navajo".
irregular(["i","pwn"]) --> "i-pwn".
irregular(["i","tao"]) --> "i-tao".
irregular(["i","tay"]) --> "i-tay".
irregular(["i","tsu"]) --> "i-tsu".
irregular(["sgn","BE","FR"]) --> "sgn-BE-FR".
irregular(["sgn","BE","NL"]) --> "sgn-BE-NL".
irregular(["sgn","CH","DE"]) --> "sgn-CH-DE".



%! 'Language-Tag'(?LanguageTag:list(string))// .
% ```abnf
% Language-Tag  = langtag             ; normal language tags
%               / privateuse          ; private use tag
%               / grandfathered       ; grandfathered tags
% ```

'Language-Tag'(L) --> langtag(L).
'Language-Tag'(L) --> privateuse(L).
'Language-Tag'(L) --> grandfathered(L).



%! 'obs-language-tag'(?LanguageTag:list(string))// .
% ```abnf
% obs-language-tag = primary-subtag *( "-" subtag )
% ```

'obs-language-tag'([H|T]) --> 'primary-subtag'(H), subtags(T).
subtags([H|T]) --> "-", !, subtag(H), subtags(T).
subtags([]) --> "".



%! 'primary-subtag'(?SubTag:string)// .
% ```abnf
% primary-subtag = 1*8ALPHA
% ```

'primary-subtag'(S) --> 'm*n'(1, 8, 'ALPHA', S, [convert(1-string)]).



%! privateuse(PrivateUse:list(string))// .
% ```abnf
% privateuse = "x" 1*("-" (1*8alphanum))
% ```

privateuse(["x"|T]) --> "x", +(privateuse0, T, []).
privateuse0(S) --> "-", 'm*n'(1, 8, alphanum, S, [covnert(1-string)]).



%! regular(?LanguageTag:list(string))// .
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

regular(["art","lojban"]) --> "art-lojban".
regular(["cel","gaulish"]) --> "cel-gaulish".
regular(["no","bok"]) --> "no-bok".
regular(["no","nyn"]) --> "no-nyn".
regular(["zh","guoyu"]) --> "zh-guoyu".
regular(["zh","hakka"]) --> "zh-hakka".
regular(["zh","min"]) --> "zh-min".
regular(["zh","min","nan"]) --> "zh-min-nan".
regular(["zh","xiang"]) --> "zh-xiang".



%! singleton(?Singleton:code)// .
% RFC 4646 defines this rule in a different order.
%
% ```abnf
% singleton = DIGIT     ; 0 - 9
%           / %x41-57   ; A - W
%           / %x59-5A   ; Y - Z
%           / %x61-77   ; a - w
%           / %x79-7A   ; y - z

singleton(C) --> 'DIGIT'(C).
singleton(C) --> between_code_radix(hex('41'), hex('57'), C).
singleton(C) --> between_code_radix(hex('59'), hex('5A'), C).
singleton(C) --> between_code_radix(hex('61'), hex('77'), C).
singleton(C) --> between_code_radix(hex('79'), hex('7A'), C).



%! subtag(?SubTag:string)// .
% ```abnf
% subtag = 1*8(ALPHA / DIGIT)
% ```

subtag(S) --> 'm*n'(1, 8, subtag0, S, [convert(1-string)]).
subtag0(C) --> 'ALPHA'(C).
subtag0(C) --> 'DIGIT'(_, C).
