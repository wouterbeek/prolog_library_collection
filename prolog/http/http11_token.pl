%! 'acceptable-ranges'(?Ranges:list(or([oneof([bytes]),string])))// .
% ```abnf
% acceptable-ranges = 1#range-unit | "none"
% ```

'acceptable-ranges'(L)  --> +#('range-unit', L), !.
'acceptable-ranges'([]) --> "none".



%! 'age-value'(-Age:nonneg)// is det.
% ```abnf
% age-value = delta-seconds
% ```

'age-value'(N) --> 'delta-seconds'(N).



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

'cache-directive'(Dir) --> 'cache-request-directive'(Dir), !.
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



%! comment(?Comment:string)//
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment(S) -->
  "(", *(comment_part, Css), ")",
  {append(Css, Cs), string_codes(S, Cs)}.
comment_part([C]) --> ctext(C).
comment_part([C]) --> 'quoted-pair'(C).
comment_part(Cs) --> comment(Cs).



%! 'delta-seconds'(?Delta:nonneg)// .
% ```abnf
% delta-seconds = 1*DIGIT
% ```

'delta-seconds'(I) --> '+digit'(I).



%! 'entity-tag'(?Weak:boolean, ?OpaqueTag:string)// .
% ```abnf
% entity-tag = [ weak ] opaque-tag
% ```

'entity-tag'(Weak, OTag) -->
  (weak -> {Weak = true} ; {Weak = false}),
  'opaque-tag'(OTag).



%! 'extension-method'(?Method:atom)// is det.
% ```abnf
% extension-method = token
% ```

'extension-method'(S) --> token(S).



%! 'extension-pragma'(?Extension:or([string,pair(string)]))// .
% ```abnf
% extension-pragma = token [ "=" ( token | quoted-string ) ]
% ```

'extension-pragma'(T) -->
  token(N),
  ("=" -> (token(V), ! ; 'quoted-string'(V)), {T = N-V} ; {T = N}).



%! 'language-tag'(?LanguageTag:list(string))// .
% ```abnf
% language-tag = primary-tag *( "-" subtag )
% ```

'language-tag'([H|T]) --> 'primary-tag'(H), *(next_subtag, T).
next_subtag(Subtag) --> "-", !, subtag(Subtag).



%! 'md5-digest'(-Md5:string)// is det.
% ```abnf
% md5-digest = <base64 of 128 bit MD5 digest as per RFC 1864>
% ```

'md5-digest' --> <base64 of 128 bit MD5 digest as per RFC 1864>



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



%! 'pragma-directive'(?Directive:or([oneof(['no-cache']),string,pair(string)]))// .
% ```abnf
% pragma-directive = "no-cache" | extension-pragma
% ```

'pragma-directive'('no-cache') --> "no-cache", !.
'pragma-directive'(X)          --> 'extension-pragma'(X).



%! 'primary-tag'(?PrimaryTag:string)// .
% ```abnf
% primary-tag = 1*8ALPHA
% ```

'primary-tag'(S) --> 'm*n'(1, 8, 'ALPHA', Cs), {string_codes(S, Cs)}.



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

subtag(S) --> 'm*n'(1, 8, 'ALPHA', Cs), {string_codes(S, Cs)}.



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
