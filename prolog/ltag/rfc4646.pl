:- module(
  rfc4646,
  [
    alphanum//1, % ?Code:code
    extension//1, % ?Extension:list(string)
    extlang//1, % ?Extension:list(string)
    grandfathered//1, % ?LanguageTag:list(string)
    langtag//1, % ?LanguageTag:list(string)
    language//1, % ?LanguageTag:list(string)
    'Language-Tag'//1, % ?LanguageTag:list(string)
    privateuse//1, % ?LanguageTag:list(string)
    region//1, % ?Region:string
    script//1, % ?Script:string
    singleton//1, % ?Code:code
    variant//1 % ?Variant:string
  ]
).

/** <module> RFC 4646: Tags for Identifying Languages

# Mistakes

The empty string complies with extlang//1, resulting in two parses for
a language tag with no extlang//1 component.

The comment for singleton//1 swaps upper and lowercase letters.

---

@author Wouter Beek
@compat RFC 4646
@deprecated Use module `rfc5646` instead.
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc5234)).





%! alphanum(?Code:code)// .
% ```abnf
% alphanum = (ALPHA / DIGIT)   ; letters and numbers
% ```

alphanum(C) --> 'ALPHA'(C).
alphanum(C) --> 'DIGIT'(C).



%! extension(?Extension:list(string))// .
% ```abnf
% extension = singleton 1*("-" (2*8alphanum))
% ```

extension([H|T]) -->
  singleton(H),
  +(extension0, T, []).
extension0(S) --> "-", 'm*n'(2, 8, alphanum, S, [convert(1-string)]).



%! extlang(?Extension:list(string))// .
% ```abnf
% extlang = *3("-" 3ALPHA)   ; reserved for future use
% ```

extlang(L) -->
  '*n'(3, reserved, L, []).
reserved(S) --> "-", #(3, 'ALPHA', S, [convert(1-string)]).


%! grandfathered(?LanguageTag:list(string))// .
% ```abnf
% grandfathered = 1*3ALPHA 1*2("-" (2*8alphanum))
%               ; grandfathered registration
%               ; Note: i is the only singleton
%               ; that starts a grandfathered tag
% ```

grandfathered([H|T]) -->
  'm*n'(1, 3, 'ALPHA', H, [convert(1-string)]),
  'm*n'(1, 2, grandfathered0, T, []).
grandfathered0(S) --> "-", 'm*n'(2, 8, alphanum, S, [convert(1-string)]).



%! langtag(?LanguageTag:list(string))// .
% ```abnf
% langtag = language
%           ["-" script]
%           ["-" region]
%           *("-" variant)
%           *("-" extension)
%           ["-" privateuse]
% ```

langtag(L) -->
  language(L1),
  ("-", script(X) ; ""),
  ("-", region(Y) ; ""),
  variants(L2),
  extensions(L3),
  ("-", privateuse(L4) ; ""),
  {append([L1,[X],[Y],L2,L3,L4], L)}.
extensions([H|T]) --> "-", extension(H), !, extensions(T).
extensions([]) --> "".
variants([H|T]) --> "-", variant(H), !, variants(T).
variants([]) --> "".



%! language(?Language:list(string))// .
% ```abnf
% language = 2*3ALPHA        ; shortest ISO 639 code
%            ["-" extlang]   ; sometimes followed by extended language subtags
%          / 4ALPHA          ; or reserved for future use
%          / 5*8ALPHA        ; or registered language subtag
% ```
				     
language([H|T]) -->
  'm*n'(2, 3, 'ALPHA', H, [convert(1-string)]),
  ("-", extlang(T) ; "").
language([H]) --> #(4, 'ALPHA', H, [convert(1-string)]).
language([H]) --> 'm*n'(5, 8, 'ALPHA', H, [convert(1-string)]).



%! 'Language-Tag'(?LanguageTag:list(string))// .
% ```abnf
% Language-Tag  = langtag             ; normal language tags
%               / privateuse          ; private use tag
%               / grandfathered       ; grandfathered tags
% ```

'Language-Tag'(L) --> langtag(L).
'Language-Tag'(L) --> privateuse(L).
'Language-Tag'(L) --> grandfathered(L).



%! privateuse(PrivateUse:list(string))// .
% ```abnf
% privateuse = ("x"/"X") 1*("-" (1*8alphanum))
% ```

privateuse(["x"|T]) --> ("x" ; "X"), +(privateuse0, T, []).
privateuse0(S) --> "-", 'm*n'(1, 8, alphanum, S, [convert(1-string)]).



%! region(?Region:string)
% ```abnf
% region = 2ALPHA   ; ISO 3166-1 code
%        / 3DIGIT   ; UN M.49 code
% ```

region(S) --> #(2, 'ALPHA', S, [convert(1-string)]).
region(S) --> #(3, 'DIGIT', _, S, [convert(2-string)]).



%! script(?Script:string)// .
% ```abnf
% script = 4ALPHA   ; ISO 15924 code
% ```

script(S) --> #(4, 'ALPHA', S, [convert(1-string)]).



%! singleton(?Code:code)// .
% ```abnf
% singleton = %x41-57 / %x59-5A / %x61-77 / %x79-7A / DIGIT
%           ; "a"-"w" / "y"-"z" / "A"-"W" / "Y"-"Z" / "0"-"9"
%           ; Single letters: x/X is reserved for private use
% ```

singleton(C) --> between_code_radix(hex('41'), hex('57'), C).
singleton(C) --> between_code_radix(hex('59'), hex('5A'), C).
singleton(C) --> between_code_radix(hex('61'), hex('77'), C).
singleton(C) --> between_code_radix(hex('79'), hex('7A'), C).
singleton(C) --> 'DIGIT'(C).



%! variant(?Variant:string)// .
% ```abnf
% variant = 5*8alphanum         ; registered variants
%         / (DIGIT 3alphanum)
% ```

variant(S) --> 'm*n'(5, 8, alphanum, S, [convert(1-string)]).
variant(S) --> dcg_string(variant_codes, S).
variant_codes([H|T]) --> 'DIGIT'(_, H), #(3, alphanum, T, []).
