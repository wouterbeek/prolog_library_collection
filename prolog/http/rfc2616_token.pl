:- module(
  rfc2616_token,
  [
    'acceptable-ranges'//1, % ?Ranges:list(or([oneof([bytes]),string]))
    attribute//1, % ?Attribute:string
    'bytes-unit'//1, % ?BytesUnit:oneof([bytes])
    'cache-directive'//1, % ?Directive:compound
    comment//1, % ?Comment:string
    'connection-token'//1, % -ConnectionToken:string
    'entity-tag'//2, % ?Weak:boolean
                     % ?OpaqueTag:string
    'language-tag'//1, % ?LanguageTag:list(string)
    'media-type'//1, % ?MediaType:dict
    'opaque-tag'//1, % ?OpaqueTag:string
    'other-range-unit'//1, % ?RangeUnit:string
    parameter//1, % ?Parameter:pair(string)
    'primary-tag'//1, % ?PrimaryTag:string
    product//1, % ?Product:dict
    'product-version'//1, % ?ProductVersion:string
    'quoted-string'//1, % ?String:string
    'range-unit'//1, % ?RangeUnit:or([oneof([bytes]),string])
    subtag//1, % ?Subtag:string
    subtype//1, % ?Subtype:string
    token//1, % ?Token:string
    'transfer-coding'//1, % ?TransferCoding:or([oneof([chunked]),dict])
    'transfer-extension'//1, % ?TransferExtension:dict
    type//1, % ?Type:string
    value//1, % ?Value:string
    weak//0
  ]
).

/** <module> RFC 2616: Tokens

@author Wouter Beek
@compat RFC 2616
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234_re)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_header)).
:- use_module(library(http/rfc2616_helpers)).
:- use_module(library(string_ext)).





%! 'acceptable-ranges'(?Ranges:list(or([oneof([bytes]),string])))// .
% ```abnf
% acceptable-ranges = 1#range-unit | "none"
% ```

'acceptable-ranges'(L)  --> +#('range-unit', L).
'acceptable-ranges'([]) --> "none".



%! attribute(?Attribute:string)// .
% ```abnf
% attribute = token
% ```

attribute(S) --> token(S).



%! 'bytes-unit'(?BytesUnit:oneof([bytes]))// .
% ```abnf
% bytes-unit = "bytes"
% ```

'bytes-unit'(bytes) --> "bytes".



%! 'cache-directive'(?Directive:compound)// .
% ```abnf
% cache-directive = cache-request-directive | cache-response-directive
% ```

'cache-directive'(Dir) --> 'cache-request-directive'(Dir).
'cache-directive'(Dir) --> 'cache-response-directive'(Dir).



%! 'cache-extension'(?CacheExtension:or([string,pair(string)]))// .
% ```abnf
% cache-extension = token [ "=" ( token | quoted-string ) ]
% ```

'cache-extension'(Ext) -->
  token(N),
  ("=" -> (token(V) ; 'quoted-string'(V)), {Ext = N-V} ; {Ext = N}).



%! 'cache-request-directive'// .
% ```abnf
% cache-request-directive = "no-cache"
%                         | "no-store"
%                         | "max-age" "=" delta-seconds
%                         | "max-stale" [ "=" delta-seconds ]
%                         | "min-fresh" "=" delta-seconds
%                         | "no-transform"
%                         | "only-if-cached"
%                         | cache-extension
% ```

'cache-request-directive'("no-cache")             --> "no-cache".
'cache-request-directive'("no-store")             --> "no-store".
'cache-request-directive'('max-age'(Delta))       --> "max-age=", 'delta-seconds'(Delta).
'cache-request-directive'('max-stale'(Delta))     --> "max-stale", ("=" -> 'delta-seconds'(Delta) ; "").
'cache-request-directive'('min-fresh'(Delta))     --> "min-fresh=", 'delta-seconds'(Delta).
'cache-request-directive'("no-transform")         --> "no-transform".
'cache-request-directive'("only-if-cached")       --> "only-if-cached".
'cache-request-directive'('cache-extension'(Ext)) --> 'cache-extension'(Ext).

%! 'cache-response-directive'// .
% ```abnf
% cache-response-directive = "public"
%                          | "private" [ "=" <"> 1#field-name <"> ]
%                          | "no-cache" [ "=" <"> 1#field-name <"> ]
%                          | "no-store"
%                          | "no-transform"
%                          | "must-revalidate"
%                          | "proxy-revalidate"
%                          | "max-age" "=" delta-seconds
%                          | "s-maxage" "=" delta-seconds
%                          | cache-extension

'cache-response-directive'("public")           --> "public".
'cache-response-directive'(private(L))         --> "private", ("=" -> "\"", +#('field-name', L), "\"" ; {L = []}).
'cache-response-directive'('no-cache'(L))      --> "no-cache", ("=" -> "\"", +#('field-name', L), "\"" ; {L = []}).
'cache-response-directive'("no-store")         --> "no-store".
'cache-response-directive'("no-transform")     --> "no-transform".
'cache-response-directive'("must-revalidate")  --> "must-revalidate".
'cache-response-directive'("proxy-revalidate") --> "proxy-revalidate".
'cache-response-directive'('max-age'(Delta))   --> "max-age=", 'delta-seconds'(Delta).
'cache-response-directive'('s-maxage'(Delta))  --> "s-maxage=", 'delta-seconds'(Delta).
'cache-response-directive'(Ext)                --> 'cache-extension'(Ext).


%! comment(?Comment:string)// .
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment(S) --> dcg_string(comment_codes1, S).
comment_codes1(L)     --> "(", comment_codes2(L), ")".
comment_codes2([H|T]) --> ctext(H),           !, comment_codes2(T).
comment_codes2([H|T]) --> 'quoted-pair'(H),   !, comment_codes2(T).
comment_codes2(L)     --> comment_codes1(L1), !, comment_codes2(L2), {append(L1, L2, L)}.
comment_codes2([])    --> "".



%! 'connection-token'(-ConnectionToken:string)// .
% ```abnf
% connection-token = token
% ```

'connection-token'(T) --> token(T).



%! 'delta-seconds'(?Delta:nonneg)// .
% ```abnf
% delta-seconds = 1*DIGIT
% ```

'delta-seconds'(I) --> '+DIGIT'(I).



%! 'entity-tag'(?Weak:boolean, ?OpaqueTag:string)// .
% ```abnf
% entity-tag = [ weak ] opaque-tag
% ```

'entity-tag'(Weak, OTag) -->
  (weak -> {Weak = true} ; {Weak = false}),
  'opaque-tag'(OTag).



%! 'language-tag'(?LanguageTag:list(string))// .
% ```abnf
% language-tag = primary-tag *( "-" subtag )
% ```

'language-tag'([H|T]) --> 'primary-tag'(H), subtags(T).
subtags([H|T]) --> "-", !, subtag(H), subtags(T).
subtags([])    --> "".



%! 'media-type'// .
% ```abnf
% media-type = type "/" subtype *( ";" parameter )
% ```

'media-type'('media-type'{type: Type, subtype: Subtype, parameters: L}) -->
  type(Type),
  "/",
  subtype(Subtype),
  parameters(L).



%! 'opaque-tag'(?OpaqueTag:string)// .
% ```abnf
% opaque-tag = quoted-string
% ```

'opaque-tag'(OTag) --> 'quoted-string'(OTag).



%! 'other-range-unit'(?RangeUnit:string)// .
% ```abnf
% other-range-unit = token
% ```

'other-range-unit'(S) --> token(S).



%! parameter(?Parameter:pair(string))// .
% ```abnf
% parameter = attribute "=" value
% ```

parameter(Key-Val) --> attribute(Key), "=", value(Val).



%! 'primary-tag'(?PrimaryTag:string)// .
% ```abnf
% primary-tag = 1*8ALPHA
% ```

'primary-tag'(S) --> 'ALPHA'(1, 8, S).



%! product(?Product:dict)// .
% ```abnf
% product = token ["/" product-version]
% ```

product(D) -->
  token(S1),
  (   "/", 'product-version'(S2), {D = product{name: S1, version: S2}}
  ;   {D = product{name: S1}}
  ).



%! 'product-version'(?ProductVersion:string)// .
% ```abnf
% 'product-version' = token
% ```

'product-version'(S) --> token(S).



%! 'quoted-string'(?String:string)// .
% ```abnf
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ```

'quoted-string'(S) --> "\"", dcg_string(quoted_string_codes, S), "\"".
quoted_string_codes([H|T]) --> qdtext(H),        !, quoted_string_codes(T).
quoted_string_codes([H|T]) --> 'quoted-pair'(H), !, quoted_string_codes(T).
quoted_string_codes([])    --> "".



%! 'range-unit'(?RangeUnit:or([oneof([bytes]),string]))// .
% ```abnf
% range-unit = bytes-unit | other-range-unit
% ```

'range-unit'(A) --> 'bytes-unit'(A), !.
'range-unit'(S) --> 'other-range-unit'(S).



%! subtag(?Subtag:string)// .
% ```abnf
% subtag = 1*8ALPHA
% ```

subtag(S) --> 'ALPHA'(1, 8, S).



%! subtype// .
% ```abnf
% subtype = token
% ```

subtype(S) --> token(S).



%! token(?Token:string)// .
% ```abnf
% token = 1*<any CHAR except CTLs or separators>
% ```
%
% ```library(dcg/dcg_abnf)
% +(token_code, S, [convert1(codes_string)])
% ```

token(S) --> dcg_string(token_codes1, S).
token_codes1([H|T]) --> token_code(H), !, token_codes2(T).
token_codes2([H|T]) --> token_code(H), !, token_codes2(T).
token_codes2([]) --> "".
token_code(C) --> 'CHAR'(C), {\+ 'CTL'(C, _, _), \+ separators(C, _, _)}.



%! 'transfer-coding'(?TransferCoding:or([oneof([chunked]),dict]))// .
% ```abnf
% transfer-coding = "chunked" | transfer-extension
% ```

'transfer-coding'(chunked) --> "chunked".
'transfer-coding'(D) --> 'transfer-extension'(D).



%! 'transfer-extension'(?TransferExtension:dict)// .
% ```abnf
% transfer-extension = token *( ";" parameter )
% ```

'transfer-extension'('transfer-extension'{token: H, parameters: T}) -->
  token(H),
  parameters(T).



%! type(?Type:string)// .
% ```abnf
% type = token
% ```

type(S) --> token(S).



%! value(?Value:string)// .
% ```abnf
% value = token | quoted-string
% ```

value(S) --> token(S), !.
value(S) --> 'quoted-string'(S).



%! weak// .
% ```abnf
% weak = "W/"
% ```

weak --> "W/".





% HELPERS %

'ALPHA'(M, N, S) --> dcg_string(alpha_codes(M, N, 0), S).
alpha_codes(_, C, C, [])     --> !, "".
alpha_codes(M, N, C1, [H|T]) -->
  'ALPHA'(H), !,
  {C2 is C1 + 1},
  alpha_codes(M, N, C2, T).
alpha_codes(M, _, C, []) --> {C >= M}, "".
