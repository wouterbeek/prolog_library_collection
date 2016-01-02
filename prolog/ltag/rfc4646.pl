:- module(
  rfc4646,
  [
    alphanum//1, % -Code:code
    extension//1, % -Extension:list(string)
    langtag//1, % -LanguageTag:list(string)
    language//1, % -LanguageTag:list(string)
    'Language-Tag'//1, % -LanguageTag:list(string)
    region//1, % -Region:string
    script//1, % -Script:string
    variant//1 % -Variant:string
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
@see http://tools.ietf.org/html/rfc4646
@version 2015/11-2016/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % -Code:code
     'DIGIT'//2 % -Weight:between(0,9)
                % -Code:code
   ]).
:- use_module(library(dcg/record_jar)).
:- use_module(library(dict_ext)).
:- use_module(library(lambda)).
:- use_module(library(memoization)).
:- use_module(library(pio)).

iana(Dicts):- memo(init_iana(Dicts)).
init_iana(Dicts):-
  FileOpts = [access(read),extensions([iana])],
  absolute_file_name(language_subtag_registry, File, FileOpts),
  phrase_from_file('record-jar'(_, Records), File),
  maplist(\Record^Dict^create_dict(Record, record, Dict), Records, Dicts).





%! alphanum(-Code:code)// is det.
% ```abnf
% alphanum = (ALPHA / DIGIT)   ; letters and numbers
% ```

alphanum(C) --> 'ALPHA'(C).
alphanum(C) --> 'DIGIT'(_, C).



%! extension(-Extension:list(string))// is det.
% ```abnf
% extension = singleton 1*("-" (2*8alphanum))
% ```

extension([H|T]) --> singleton(H), +(sep_ext, T).
sep_ext(S) --> "-", 'm*n'(2, 8, alphadigit, Cs), {string_codes(S, Cs)}.



%! extlang(-Extension:list(string))// is det.
% ```abnf
% extlang = *3("-" 3ALPHA)   ; reserved for future use
% ```

extlang(L) --> '*n'(3, sep_extlang, L).
sep_extlang(S) --> "-", #(3, 'ALPHA', Cs), {string_codes(S, Cs)}.



%! grandfathered(-LanguageTag:list(string))// is det.
% ```abnf
% grandfathered = 1*3ALPHA 1*2("-" (2*8alphanum))
%               ; grandfathered registration
%               ; Note: i is the only singleton
%               ; that starts a grandfathered tag
% ```

grandfathered([H|T]) -->
  'm*n'(1, 3, 'ALPHA', Cs), {string_codes(H, Cs)},
  'm*n'(1, 2, sep_grandfathered, T).
sep_grandfathered(S) -->
  "-", 'm*n'(2, 8, alphanum, Cs), {string_codes(S, Cs)}.



%! langtag(-LanguageTag:list(string))// is det.
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
  ("-" -> script(X2), {L2 = [X2]} ; {L2 = []}),
  ("-" -> region(X3), {L3 = [X3]} ; {L3 = []}),
  *(sep_variant, L4),
  *(sep_extension, L5),
  ("-" -> privateuse(L6) ; {L6 = []}),
  {append([L1,L2,L3,L4,L5,L6], L)}.
sep_extension(S) --> "-", extension(S).
sep_variant(S) --> "-", variant(S).



%! language(-Language:list(string))// is det.
% ```abnf
% language = 2*3ALPHA        ; shortest ISO 639 code
%            ["-" extlang]   ; sometimes followed by extended language subtags
%          / 4ALPHA          ; or reserved for future use
%          / 5*8ALPHA        ; or registered language subtag
% ```
				     
language([H|T]) -->
  'm*n'(2, 3, 'ALPHA', Cs), {string_codes(H, Cs)},
  ("-" -> extlang(T) ; {T = []}).
language([H]) --> #(4, 'ALPHA', Cs), {string_codes(H, Cs)}.
language([H]) --> 'm*n'(5, 8, 'ALPHA', Cs), {string_codes(H, Cs)}.



%! 'Language-Tag'(-LanguageTag:list(string))// .
% ```abnf
% Language-Tag = langtag         ; normal language tags
%              / privateuse      ; private use tag
%              / grandfathered   ; grandfathered tags
% ```

'Language-Tag'(L) --> langtag(L), !.
'Language-Tag'(L) --> privateuse(L), !.
'Language-Tag'(L) --> grandfathered(L).



%! privateuse(-PrivateUse:list(string))// is det.
% ```abnf
% privateuse = ("x"/"X") 1*("-" (1*8alphanum))
% ```

privateuse(["x"|T]) --> atom_ci(x), +(sep_privateuse, T).
sep_privateuse(S) --> "-", 'm*n'(1, 8, alphanum, Cs), {string_codes(S, Cs)}.



%! region(-Region:string)
% ```abnf
% region = 2ALPHA   ; ISO 3166-1 code
%        / 3DIGIT   ; UN M.49 code
% ```

region(S) --> #(2, 'ALPHA', Cs), {string_codes(S, Cs)}.
region(S) --> #(3, 'DIGIT', _, Cs), {string_codes(S, Cs)}.



%! script(-Script:string)// is det.
% ```abnf
% script = 4ALPHA   ; ISO 15924 code
% ```

script(S) --> #(4, 'ALPHA', Cs), {string_codes(S, Cs)}.



%! singleton(-Code:code)// .
% ```abnf
% singleton = %x41-57 / %x59-5A / %x61-77 / %x79-7A / DIGIT
%           ; "a"-"w" / "y"-"z" / "A"-"W" / "Y"-"Z" / "0"-"9"
%           ; Single letters: x/X is reserved for private use
% ```

singleton(C) -->
  [C],
  {once((
    between(0x41, 0x57, C) ;
    between(0x59, 0x5A, C) ;
    between(0x61, 0x77, C) ;
    between(0x79, 0x7A, C)
  ))}, !.
singleton(C) --> 'DIGIT'(_, C).



%! variant(-Variant:string)// is det.
% ```abnf
% variant = 5*8alphanum         ; registered variants
%         / (DIGIT 3alphanum)
% ```

variant(S) --> 'm*n'(5, 8, alphanum, Cs), !, {string_codes(S, Cs)}.
variant(S) --> 'DIGIT'(_, H), #(3, alphanum, T), {string_codes(S, [H|T])}.
