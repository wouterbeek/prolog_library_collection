:- module(
  http11,
  [
    'field-name'//1,   % -Name:string
    'header-field'//1, % -Header:pair
    method//1,         % -Method:string
    'OWS'//0,
    'rfc850-date'//1   % -Date:dict
  ]
).

/** <module> HTTP 1.1: Hypertext Transfer Protocol (HTTP/1.1)

# Common mistakes

## Access-Control-Allow-Credentials

Value `*' i.o. `true'.

```http
Access-Control-Allow-Credentials: *
```


## ETag

No double quotes (`DQUOTE') surrounding the opaque identifier.

```http
ETag: e90ec0728cc9d1a7dd2c917923275fb9
```


## Expires

Timezone other than `"GMT"'.

```http
Expires: Sat, 26 Dec 2015 010:30:29 UTC
```

The numer zero i.o. a date-time value.

```http
Expires: 0
```


## Last-modified

Timezone other than `"GMT"'.

```http
Last-Modified: Sat, 26 Dec 2015 010:30:29 UTC
```


## Link

The query component of an URI cannot contain unescaped angular brackets.

```http
Link: <http://el.dbpedia.org/data/Linux.rdf>; rel="alternate"; type="application/rdf+xml"; title="Structured Descriptor Document (RDF/XML format)", <http://el.dbpedia.org/data/Linux.n3>; rel="alternate"; type="text/n3"; title="Structured Descriptor Document (N3/Turtle format)", <http://el.dbpedia.org/data/Linux.json>; rel="alternate"; type="application/json"; title="Structured Descriptor Document (RDF/JSON format)", <http://el.dbpedia.org/data/Linux.atom>; rel="alternate"; type="application/atom+xml"; title="OData (Atom+Feed format)", <http://el.dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fel.dbpedia.org&query=DESCRIBE+<http://el.dbpedia.org/resource/Linux>&format=text%2Fcsv>; rel="alternate"; type="text/csv"; title="Structured Descriptor Document (CSV format)", <http://el.dbpedia.org/data/Linux.ntriples>; rel="alternate"; type="text/plain"; title="Structured Descriptor Document (N-Triples format)", <http://el.dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fel.dbpedia.org&query=DESCRIBE+<http://el.dbpedia.org/resource/Linux>&output=application/microdata+json>; rel="alternate"; type="application/microdata+json"; title="Structured Descriptor Document (Microdata/JSON format)", <http://el.dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fel.dbpedia.org&query=DESCRIBE+<http://el.dbpedia.org/resource/Linux>&output=text/html>; rel="alternate"; type="text/html"; title="Structured Descriptor Document (Microdata/HTML format)", <http://el.dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fel.dbpedia.org&query=DESCRIBE+<http://el.dbpedia.org/resource/Linux>&output=application/ld+json>; rel="alternate"; type="application/ld+json"; title="Structured Descriptor Document (JSON-LD format)", <http://el.dbpedia.org/resource/Linux>; rel="http://xmlns.com/foaf/0.1/primaryTopic", <http://el.dbpedia.org/resource/Linux>; rev="describedby", <http://mementoarchive.lanl.gov/dbpedia/timegate/http://el.dbpedia.org/page/Linux>; rel="timegate"
```

## Location

Lowercase hexadecimal digits in percent encoded characters of URIs
(disallowed by RFC 3986).

```http
Location: https://login2.gnoss.com/obtenerCookie.aspx?UQ5IKlMXioK1tiCnK0oh7y%2fPdqTnyAdHPDV0j1Ox5tEdM14pGmhdaSTZhT3hezOjI4lDAwx%2fOghE%2fLJk7Ce%2ff%2ft%2bsC%2bywTcFPSXaYZhh2Wg9q5mWlDWDvLHMReIWmqWzGhnxNpc4DnbUDjewEMV5vfTVzKD%2bx3gMLq9vZvV%2fL8aIAQOcWkSRam0OyOCkc2KV2zUx24WqdUo9oS5od1ILHDmDwYJxq7WwRgFnHP73WfYJuQJNhwolaTkH7%2blbmx7V4K7bF12Van5ArXjz6umEpg%3d%3d
```


## Server

No linear white space (`LWS') between product and comment.

```http
Server: Jetty(8.1.1.v20120215)
```


## Set-Cookie

Omitting the requires space (`SP') between the separator (`;')
and the cookie parameter.

```http
set-cookie: NSC_tfep-83+63+5+220-91=ffffffff516a73d445525d5f4f58455e445a4a423660;path=/;httponly
```


## Via

Via components must consist of two parts ('protocol' and 'by')
that must be separated by white space (`RWS').

```http
Via: mt-s6, fs4
```


## X-Frame-Options

A valid value that appears multiple times.
(RFC 7034 does not allow comma-separated values.)

```http
X-Frame-Options: SAMEORIGIN, SAMEORIGIN
```

---

@author Wouter Beek
@compat RFC 7230
@compat RFC 7231
@compat RFC 7232
@compat RFC 7233
@compat RFC 7234
@compat RFC 7235
@see https://tools.ietf.org/html/rfc7230
@see https://tools.ietf.org/html/rfc7231
@see https://tools.ietf.org/html/rfc7232
@see https://tools.ietf.org/html/rfc7233
@see https://tools.ietf.org/html/rfc7234
@see https://tools.ietf.org/html/rfc7235
@version 2015/11-2016/02
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1,  % ?Code:code
     'CHAR'//1,   % ?Code:code
     'CR'//0,
     'CRLF'//0,
     'CTL'//0,
     'CTL'//1,    % ?Code:code
     'DIGIT'//1,  % ?Weight:nonneg
     'DIGIT'//2,  % ?Weight:nonneg, ?Code:code
     'DQUOTE'//0,
     'HEXDIG'//1, % ?Weight:nonneg
     'HTAB'//0,
     'HTAB'//1,   % ?Code:code
     'LF'//0,
     'OCTET'//1,  % ?Code:code
     'SP'//0,
     'SP'//1,     % ?Code:code
     'VCHAR'//1   % ?Code:code
   ]).
:- use_module(library(http/cors)).
:- use_module(library(http/csp2)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/rfc5988)).
:- use_module(library(http/rfc6265)).
:- use_module(library(http/rfc6266)).
:- use_module(library(http/rfc6797)).
:- use_module(library(http/rfc7034)).
:- use_module(library(ltag/rfc4647), [
     'language-range'//1 % -LanguageRange:list(string)
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 as 'language-tag' % -LanguageTag:dict
   ]).
:- use_module(library(mail/rfc5322), [
     mailbox//1 % -Pair:pair(string)
   ]).
:- use_module(library(pair_ext)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(sgml)).
:- use_module(library(uri/rfc3986), [
     'absolute-URI'//1,     % -AbsoluteUri:dict
     fragment//1,           % -Fragment:string
     host//1 as 'uri-host', % -Host:dict
     port//1,               % -Port:nonneg
     query//1,              % -Query:string
     'relative-part'//1,    % -RelativeUri:dict
     'URI-reference'//1     % -UriReference:dict
   ]).





%! accept(-AcceptValues:list(dict))// is det.
% ```abnf
% Accept = #( media-range [ accept-params ] )
% ```

accept(L) --> '*#'(accept_value, L).

accept_value(D2) -->
  {D1 = _{'@type': 'llo:AcceptValue', 'llo:media_range': MediaRange}},
  'media-range'(MediaRange),
  (   'accept-params'(Params)
  ->  {D2 = D1.put(_{'llo:parameters': Params})}
  ;   {D2 = D1}
  ).



%! 'accept-charset'(-Charactersets:list(dict))// is det.
% ```abnf
% Accept-Charset = 1#( ( charset | "*" ) [ weight ] )
% ```

'accept-charset'(L) --> +#(accept_charset_value, L).

accept_charset_value(D2) -->
  ("*" -> {Charset = "*"} ; charset(Charset)),
  {D1 = _{'@type': 'llo:CharacterSet', 'llo:character_set': Charset}},
  (weight(Weight) -> {D2 = D1.put(_{'llo:weight': Weight})} ; {D2 = D1}).



%! 'accept-encoding'(-AcceptEncodingValues:list(dict))// is det.
% ```abnf
% Accept-Encoding = #( codings [ weight ] )
% ```

'accept-encoding'(L) --> '*#'(accept_encoding_value, L).

accept_encoding_value(D2) -->
  {D1 = _{'llo:codings': Codings}},
  codings(Codings),
  (weight(Weight) -> {D2 = D1.put(_{'llo:weight': Weight})} ; {D2 = D1}).



%! 'accept-ext'(-AcceptExtension:dict)// is det.
% ```abnf
% accept-ext = OWS ";" OWS token [ "=" ( token | quoted-string ) ]
% ```

'accept-ext'(D2) -->
  'OWS', ";", 'OWS',
  token(Key),
  {D1 = _{'@type': 'llo:Parameter', 'llo:key': Key}},
  (   "="
  ->  (token(Value), ! ; 'quoted-string'(Value)),
      {D2 = D1.put(_{'llo:value': Value})}
  ;   {D2 = D1}
  ).



%! 'accept-language'(-AcceptLanguageValues:list(dict))// is det.
% ```abnf
% Accept-Language = 1#( language-range [ weight ] )
% ```

'accept-language'(L) --> +#(accept_language_value, L).

accept_language_value(D2) -->
  'language-range'(LanguageRange),
  {D1 = _{'llo:language_range': LanguageRange}},
  (weight(Weight) -> {D2 = D1.put(_{'llo:weight': Weight})} ; {D2 = D1}).



%! 'accept-params'(-AcceptParameters:dict)// is det.
% ```abnf
% accept-params = weight *( accept-ext )
% ```

'accept-params'(_{
  '@type': 'llo:AcceptParameters',
  'llo:weight': Weight,
  'llo:accept_extensions': Exts
}) -->
  weight(Weight), *('accept-ext', Exts).



%! 'accept-ranges'(-AcceptRanges:list(dict))// is det.
% ```abnf
% Accept-Ranges = acceptable-ranges
% ```

'accept-ranges'(L) --> 'acceptable-ranges'(L).



%! 'acceptable-ranges'(-AcceptableRanges:list(dict))// is det.
% ```abnf
% acceptable-ranges = 1#range-unit | "none"
% ```

'acceptable-ranges'(L) -->
  (+#('range-unit', L) -> "" ; atom_ci(none) -> {L = []}).



%! age(-Age:dict)// is det.
% ```abnf
% Age = delta-seconds
% ```

age(_{'@type': 'xsd:nonNegativeInteger', '@value': N}) -->
  'delta-seconds'(N).



%! allow(-Methods:list(string))// is det.
% ```abnf
% Allow = #method
% ```

allow(L) --> '*#'(method, L).



%! 'asctime-date'(-D)// is det.
% ```abnf
% asctime-date = day-name SP date3 SP time-of-day SP year
% ```

'asctime-date'(_{'@type': Type, '@value': Lex}) -->
  'day-name'(D),
  'SP',
  date3(Mo, D),
  'SP',
  'time-of-day'(H, Mi, S),
  'SP',
  year(Y),
  {
    rdf_equal(xsd:dateTime, Type),
    xsd_time_string(date_time(Y,Mo,D,H,Mi,S), Type, Lex)
  }.



%! 'auth-param'(-Parameter:dict)// is det.
% ```abnf
% auth-param = token BWS "=" BWS ( token | quoted-string )
% ```

'auth-param'(_{
  '@type': 'llo:Parameter',
  'llo:key': Key,
  'llo:value': Value
}) -->
  token(Key),
  'BWS', "=", 'BWS',
  (token(Value), ! ; 'quoted-string'(Value)).



%! 'auth-scheme'(-Scheme:string)// is det.
% ```abnf
% auth-scheme = token
% ```

'auth-scheme'(S) --> token(S).



%! authorization(-Credentials:dict)// is det.
% ```abnf
% Authorization = credentials
% ```

authorization(D) --> credentials(D).



%! 'BWS'// is det.
% ```abnf
% BWS = OWS   ; "bad" whitespace
% ```

'BWS' --> 'OWS'.



%! 'byte-content-range'(-ByteContentRange:dict)// is det.
% ```abnf
% byte-content-range = bytes-unit SP ( byte-range-resp | unsatisfied-range )
% ```

'byte-content-range'(D2) -->
  'bytes-unit',
  'SP',
  ('byte-range-resp'(Range, Length), ! ; 'unsatisfied-range'(Range, Length)),
  {
    D1 = _{
      '@type': 'llo:ByteContentRange',
      'llo:range': Range,
      'llo:unit': "bytes"
    },
    (var(Length) -> D2 = D1 ; D2 = D1.put(_{'llo:length': Length}))
  }.



%! 'byte-range'(-ByteRange:dict)// is det.
% ```abnf
% byte-range = first-byte-pos "-" last-byte-pos
% ```

'byte-range'(_{
  '@type': 'llo:ByteRange',
  'llo:first_byte_position': _{'@type': 'xsd:nonNegativeInteger', '@value': First},
  'llo:last_byte_position': _{'@type': 'xsd:nonNegativeInteger', '@value': Last}
}) -->
  'first-byte-pos'(First), "-", 'last-byte-pos'(Last).



%! 'byte-range-resp'(-Range:dict, ?Length:dict)// is det.
% ```abnf
% byte-range-resp = byte-range "/" ( complete-length | "*" )
% ```

'byte-range-resp'(Range, Length) -->
  'byte-range'(Range), "/", ("*" -> "" ; 'complete-length'(Length)).



%! 'byte-range-spec'(-ByteRangeSpec:dict)// is det.
% ```abnf
% byte-range-spec = first-byte-pos "-" [ last-byte-pos ]
% ```

'byte-range-spec'(D2) -->
  'first-byte-pos'(First),
  "-",
  {D1 = _{
    '@type': 'llo:ByteRangeSpec',
    'llo:first': _{'@type': 'xsd:nonNegativeInteger', '@value': First}
  }},
  (   'last-byte-pos'(Last)
  ->  {D2 = D1.put(_{
        'llo:last': _{'@type': 'xsd:nonNegativeInteger', '@value': Last}
      })}
  ;   {D2 = D1}
  ).



%! 'byte-ranges-specifier'(-ByteRangesSpecifier:dict)// is det.
% ```abnf
% byte-ranges-specifier = bytes-unit "=" byte-range-set
% ```

'byte-ranges-specifier'(_{
  '@type': 'llo:ByteRangesSpecifier',
  'llo:byte_range_set': Ranges,
  'llo:bytes_unit': "bytes"
}) -->
  'bytes-unit', "=", 'byte-range-set'(Ranges).



%! 'byte-range-set'(-ByteRangeSet:list(dict))// is det.
% ```abnf
% byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
% ```

'byte-range-set'(L) --> +#(byte_range_set_part, L).

byte_range_set_part(D) --> 'byte-range-spec'(D).
byte_range_set_part(D) --> 'suffix-byte-range-spec'(D).



%! 'bytes-unit'// is det.
% ```abnf
% bytes-unit = "bytes"
% ```

'bytes-unit' --> atom_ci(bytes).



%! 'cache-control'(-Directives:list(dict))// is det.
% ```abnf
% Cache-Control = 1#cache-directive
% ```

'cache-control'(L) --> +#('cache-directive', L).



%! 'cache-directive'(-CacheDirective:dict)// is det.
% ```abnf
% cache-directive = token [ "=" ( token | quoted-string ) ]
% ```

'cache-directive'(D2) -->
  token(Key),
  {D1 = _{'@type': 'llo:CacheDirective', 'llo:key': Key}},
  (   "="
  ->  (token(Value), ! ; 'quoted-string'(Value)),
      {D2 = D1.put(_{'llo:value': Value})}
  ;   {D2 = D1}
  ).



%! challenge(-Challenge:dict)// is det.
% ```abnf
% challenge = auth-scheme [ 1*SP ( token68 | #auth-param ) ]
% ```

challenge(D2) -->
  'auth-scheme'(AuthScheme),
  {D1 = _{'@type': 'llo:Challenge', 'llo:authority_scheme': AuthScheme}},
  (   +('SP')
  ->  ('*#'('auth-param', Params), ! ; token68(S), {Params = [S]}),
      {D2 = D1.put(_{'llo:parameters': Params})}
  ;   {D2 = D1}
  ).



%! charset(-Charset:string)// is det.
% ```abnf
% charset = token
% ```

charset(S) --> token(S).



%! chunk(-Chunk:dict)// is det.
% ```abnf
% chunk = chunk-size [ chunk-ext ] CRLF chunk-data CRLF
% ```
%
% @bug It's a mistake to make chunk-ext optional when its also Kleene star.

chunk(_{
  '@type': 'llo:Chunk',
  'llo:chunk_data': Cs,
  'llo:chunk_extensions': Exts,
  'llo:chunk_size': _{'@type': 'xsd:nonNegativeInteger', '@value': Size}
}) -->
  'chunk-size'(Size),
  'chunk-ext'(Exts), 'CRLF',
  'chunk-data'(Cs), 'CRLF'.



%! 'chunk-data'(-Codes:list(code))// is det.
% ```abnf
% chunk-data = 1*OCTET ; a sequence of chunk-size octets
% ```

'chunk-data'(Cs) --> +('OCTET', Cs).



%! 'chunk-ext'(-Extensions:list(dict))// is det.
% ```abnf
% chunk-ext = *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
% ```

'chunk-ext'(L) --> *(sep_chunk_ext, L).

sep_chunk_ext(D2) -->
  ";", 'chunk-ext-name'(Key),
  {D1 = _{'llo:key': Key}},
  (   "="
  ->  'chunk-ext-val'(Value),
      {D2 = D1.put(_{'llo:value': Value})}
  ;   {D2 = D1}
  ).



%! 'chunk-ext-name'(-Name:string)// is det.
% ```abnf
% chunk-ext-name = token
% ```

'chunk-ext-name'(S) --> token(S).



%! 'chunk-ext-val'(-Value:string)// is det.

'chunk-ext-val'(S) --> token(S), !.
'chunk-ext-val'(S) --> 'quoted-string'(S).



%! 'chunk-size'(-Size:nonneg)// is det.
% ```abnf
% chunk-size = 1*HEXDIG
% ```

'chunk-size'(N) --> +('HEXDIG', Ds), {pos_sum(Ds, 16, N)}.



%! codings(-Codings:string)// is det.
% ```abnf
% codings = content-coding | "identity" | "*"
% ```

codings(S)          --> 'content-coding'(S).
codings("identity") --> atom_ci(identity).
codings("*")        --> "*".



%! comment(-Comment:string)// is det.
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment(S) --> dcg_string(comment_codes1, S).

comment_codes1([0'(|T]) -->
  "(", comment_codes2(T0), ")",
  {append(T0, [0')], T)}.

comment_codes2([H|T]) --> ctext(H), !, comment_codes2(T).
comment_codes2([H|T]) --> 'quoted-pair'(H), !, comment_codes2(T).
comment_codes2(L)     --> comment_codes1(L), !.
comment_codes2([])    --> "".



%! 'complete-length'(-Length:dict)// is det.
% ```abnf
% complete-length = 1*DIGIT
% ```

'complete-length'(_{'@type': 'xsd:nonNegativeInteger', '@value': N}) -->
  +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! connection(-ConnectionOptions:list(string))// is det.
% ```abnf
% 'Connection'(S) --> 1#(connection-option)
% ```

connection(L) --> +#('connection-option', L).



%! 'connection-option'(-ConnectionOption:string)// .
% ```abnf
% connection-option = token
% ```

'connection-option'(S) --> token(S).



%! 'content-coding'(-ContentCoding:string)// is det.
% ```abnf
% content-coding = token
% ```

'content-coding'(S) --> token(S).



%! 'content-encoding'(-Encodings:list(string))// is det.
% ```abnf
% Content-Encoding = 1#content-coding
% ```

'content-encoding'(L) --> +#('content-coding', L).



%! 'content-language'(-LanguageTags:list(dict))// .
% ```abnf
% Content-Language = 1#language-tag
% ```

'content-language'(L) --> +#('language-tag', L).



%! 'content-length'(-D)// is det.
% ```abnf
% Content-Length = 1*DIGIT
% ```

'content-length'(_{'@type': 'xsd:nonNegativeInteger', '@value': N}) -->
  +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'content-location'(-Uri:dict)// is det.
% ```abnf
% Content-Location = absolute-URI | partial-URI
% ```

'content-location'(Uri) --> ('absolute-URI'(Uri), ! ; 'partial-URI'(Uri)).



%! 'content-range'(-ContentRange:dict)// is det.
% ```abnf
% Content-Range = byte-content-range | other-content-range
% ```

'content-range'(D) --> ('byte-content-range'(D), ! ; 'other-content-range'(D)).



%! 'content-type'(-MediaType:dict)// is det.
% ```abnf
% Content-Type = media-type
% ```

'content-type'(D) --> 'media-type'(D).



%! credentials(-Credentials:dict)// is det.
% ```abnf
% credentials = auth-scheme [ 1*SP ( token68 | #auth-param ) ]
% ```

credentials(D2) -->
  'auth-scheme'(AuthScheme),
  {D1 = _{'llo:authority_scheme': AuthScheme}},
  (   +('SP')
  ->  (token68(S) -> {Params = [S]} ; '*#'('auth-param', Params)),
      {D2 = D1.put(_{'llo:parameters': Params})}
  ;   {D2 = D1}
  ).



%! ctext(?Code:code)// is det.
% ```abnf
% ctext = HTAB | SP | %x21-27 | %x2A-5B | %x5D-7E | obs-text
% ```

ctext(C) --> 'HTAB'(C).
ctext(C) --> 'SP'(C).
ctext(C) --> [C], {(between(0x21, 0x27, C), !
                  ; between(0x2A, 0x5B, C), !
                  ; between(0x5D, 0x7E, C))}.
ctext(C) --> 'obs-text'(C).



%! date(-Datetime:dict)// is det.
% ```abnf
% Date = HTTP-date
% ```

date(D) --> 'HTTP-date'(D).



%! date1(-Year:between(0,9999), -Month:between(1,12), -Day:between(0,99))// is det.
% ```abnf
% date1 = day SP month SP year   ; e.g., 02 Jun 1982
% ```

date1(Y, Mo, D) --> day(D), 'SP', month(Mo), 'SP', year(Y).



%! date2(-Year:between(0,9999), -Month:between(1,12), -Day:beween(0,99))// is det.
% ```abnf
% date2 = day "-" month "-" 2DIGIT   ; e.g., 02-Jun-82
% ```

date2(Y, Mo, D) -->
  day(D), "-",
  month(Mo), "-",
  #(2, 'DIGIT', Ds), {pos_sum(Ds, Y)}.



%! date3(-Month:between(1,12), -Day:between(0,99))// is det.
% ```abnf
% date3 = month SP ( 2DIGIT | ( SP 1DIGIT ))   ; e.g., Jun  2
% ```

date3(Mo, D) -->
  month(Mo), 'SP',
  (#(2, 'DIGIT', Ds) -> {pos_sum(Ds, D)} ; 'SP', #(1, 'DIGIT', D)).



%! day(-Day:between(0,99))// is det.
% ```abnf
% day = 2DIGIT
% ```

day(D) --> #(2, 'DIGIT', Ds), {pos_sum(Ds, D)}.



%! 'day-name'(-Day:between(1,7))// is det.
% ```abnf
% day-name = %x4D.6F.6E   ; "Mon", case-sensitive
%          | %x54.75.65   ; "Tue", case-sensitive
%          | %x57.65.64   ; "Wed", case-sensitive
%          | %x54.68.75   ; "Thu", case-sensitive
%          | %x46.72.69   ; "Fri", case-sensitive
%          | %x53.61.74   ; "Sat", case-sensitive
%          | %x53.75.6E   ; "Sun", case-sensitive
% ```

'day-name'(1) --> atom_ci('Mon'), !.
'day-name'(2) --> atom_ci('Tue'), !.
'day-name'(3) --> atom_ci('Wed'), !.
'day-name'(4) --> atom_ci('Thu'), !.
'day-name'(5) --> atom_ci('Fri'), !.
'day-name'(6) --> atom_ci('Sat'), !.
'day-name'(7) --> atom_ci('Sun').



%! 'day-name-l'(-Day:between(1,7))// is det.
% ```abnf
% day-name-l = %x4D.6F.6E.64.61.79            ; "Monday", case-sensitive
%            | %x54.75.65.73.64.61.79         ; "Tuesday", case-sensitive
%            | %x57.65.64.6E.65.73.64.61.79   ; "Wednesday", case-sensitive
%            | %x54.68.75.72.73.64.61.79      ; "Thursday", case-sensitive
%            | %x46.72.69.64.61.79            ; "Friday", case-sensitive
%            | %x53.61.74.75.72.64.61.79      ; "Saturday", case-sensitive
%            | %x53.75.6E.64.61.79            ; "Sunday", case-sensitive
% ```

%! weekday(?Weekday:between(1,7))// .
% ```abnf
% weekday = "Monday" | "Tuesday" | "Wednesday"
%         | "Thursday" | "Friday" | "Saturday" | "Sunday"
% ```

'day-name-l'(1) --> atom_ci('Monday'), !.
'day-name-l'(2) --> atom_ci('Tuesday'), !.
'day-name-l'(3) --> atom_ci('Wednesday'), !.
'day-name-l'(4) --> atom_ci('Thursday'), !.
'day-name-l'(5) --> atom_ci('Friday'), !.
'day-name-l'(6) --> atom_ci('Saturday'), !.
'day-name-l'(7) --> atom_ci('Sunday').



%! 'delay-seconds'(-D)// is det.
% ```abnf
% delay-seconds = 1*DIGIT
% ```

'delay-seconds'(_{'@type': 'xsd:nonNegativeInteger', '@value': N}) -->
  +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'delta-seconds'(-Delta:nonneg)// is det.
% ```abnf
% delta-seconds = 1*DIGIT
% ```

'delta-seconds'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'entity-tag'(-EntityTag:dict)// is det.
% ```abnf
% entity-tag = [ weak ] opaque-tag
% ```

'entity-tag'(_{
  '@type': 'llo:EntityTag',
  'llo:weak': Weak,
  'llo:opaque_tag': OTag
}) -->
  (weak -> {Weak = true} ; {Weak = false}),
  'opaque-tag'(OTag).



%! expires(-Datetime:dict)// is det.
% ```abnf
% Expires = HTTP-date
% ```

expires(D) --> 'HTTP-date'(D).



%! etag(-ETag:dict)// is det.
% Used for Web cache validation and optimistic concurrency control.
%
% ```abnf
% ETag = "ETag" ":" entity-tag
% ```

etag(D) --> 'entity-tag'(D).



%! etagc(?Code:code)// .
% ```abnf
% etagc = %x21 | %x23-7E | obs-text   ; VCHAR except double quotes, plus obs-text
% ```

etagc(0x21) --> [0x21].
etagc(C)    --> [C], {between(0x23, 0x7E, C)}.
etagc(C)    --> 'obs-text'(C).



%! expect(-Expectation:string)// is det.
% ```abnf
% Expect = "100-continue"
% ```

expect("100-continue") --> atom_ci('100-continue').



%! 'extension-pragma'(-Parameter:dict)// is det.
% ```abnf
% extension-pragma = token [ "=" ( token | quoted-string ) ]
% ```

'extension-pragma'(D2) -->
  token(Key),
  {D1 = _{'@type': 'llo:Parameter', 'llo:key': Key}},
  (   "="
  ->  (token(Value), ! ; 'quoted-string'(Value)),
      {D2 = D1.put(_{'llo:value': Value})}
  ;   {D2 = D1}
  ).



%! 'field-content'(+Key:atom, -Value:dict)// .
% ```abnf
% field-content = field-vchar [ 1*( SP | HTAB ) field-vchar ]
% ```

'field-content'(Key, D) -->
  (   {current_predicate(Key/3)}
  ->  (   % Valid value.
          dcg_call(Key, Value),
          'OWS',
          % This should fail in case only /part/ of the HTTP header is parsed.
          eos
      ->  {D = _{'@type': 'llo:ValidHttpHeader', 'llo:value': Value}}
      ;   % Empty value.
          phrase('obs-fold')
      ->  {D = _{'@type': 'llo:EmptyHttpHeader'}}
      ;    % Buggy value.
          rest(Cs),
          {
            D = _{'@type': 'llo:InvalidHttpHeader'},
            debug(http(parse), "Buggy HTTP header ~a: ~s", [Key,Cs])
          }
      )
  ;   % Unknown unknown key.
      rest(Cs),
      {
        D = _{'@type': 'llo:UnknownHttpHeader'},
        (   known_unknown(Key)
        ->  true
        ;   debug(http(parse), "No parser for HTTP header ~a: ~s", [Key,Cs])
        )
      }
  ).
known_unknown('cf-ray').
known_unknown('fuseki-request-id').
known_unknown(servidor).
known_unknown('x-acre-source-url').
known_unknown('x-adblock-key').
known_unknown('x-backend').
known_unknown('x-cache').
known_unknown('x-cache-action').
known_unknown('x-cache-age').
known_unknown('x-cache-hits').
known_unknown('x-cache-lookup').
known_unknown('x-cache-operation').
known_unknown('x-cache-rule').
known_unknown('x-cacheable').
known_unknown('x-content-type-options'). % Has grammar.  Implemented.
known_unknown('x-dropbox-http-protocol').
known_unknown('x-dropbox-request-id').
known_unknown('x-drupal-cache').
known_unknown('x-ec-custom-error').
known_unknown('x-fastly-request-id').
known_unknown('x-generator').
known_unknown('x-github-request-id').
known_unknown('x-goog-generation').
known_unknown('x-goog-hash').
known_unknown('x-goog-meta-uploaded-by').
known_unknown('x-goog-metageneration').
known_unknown('x-goog-storage').
known_unknown('x-goog-storage-class').
known_unknown('x-goog-stored-content-encoding').
known_unknown('x-goog-stored-content-length').
known_unknown('x-http-host').
known_unknown('x-hosted-by').
known_unknown('x-metaweb-cost').
known_unknown('x-metaweb-tid').
known_unknown('x-pad').
known_unknown('x-pal-host').
known_unknown('x-pingback').
known_unknown('x-powered-by').
known_unknown('x-productontology-limit').
known_unknown('x-productontology-offset').
known_unknown('x-productontology-results').
known_unknown('x-purl').
known_unknown('x-robots-tag'). % Has grammar.  Implemented.
known_unknown('x-rack-cache').
known_unknown('x-request-id').
known_unknown('x-response-id').
known_unknown('x-runtime').
known_unknown('x-served-by').
known_unknown('x-served-from-cache').
known_unknown('x-sparql').
known_unknown('x-sparql-default-graph').
known_unknown('x-timer').
known_unknown('x-total-results').
known_unknown('x-ua-compatible').
known_unknown('x-uniprot-release').
known_unknown('x-varnish').
known_unknown('x-varnish-caching-rule-id').
known_unknown('x-varnish-header-set-id').
known_unknown('x-xss-protection'). % Has grammar.  Implemented.



%! 'field-name'(-Name:string)// is det.
% ```abnf
% field-name = token
% ```

'field-name'(LowerS) --> token(S), {string_lower(S, LowerS)}.



%! 'field-vchar'(?Code:code)// .
% ```abnf
% field-vchar = VCHAR | obs-text
% ```

'field-vchar'(C) --> 'VCHAR'(C).
'field-vchar'(C) --> 'obs-text'(C).



%! 'field-value'(+Codes, +Key, -Value:dict)// is det.
% ```abnf
% field-value = *( field-content | 'obs-fold' )
% ```

'field-value'(Cs, Key, D2) :-
  phrase('field-content'(Key, D1), Cs),
  string_codes(Raw, Cs),
  D2 = D1.put(_{'llo:raw': Raw}).



%! 'first-byte-pos'(-Position:nonneg)// is det.
% ```abnf
% first-byte-pos = 1*DIGIT
% ```

'first-byte-pos'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! from(-Mailbox:dict)// is det.
% ```abnf
% From = mailbox
% ```

from(D) --> mailbox(D).



%! 'GMT'// is det.
% ```abnf
% GMT = %x47.4D.54   ; "GMT", case-sensitive
% ```

'GMT' --> atom_ci('GMT').



%! 'header-field'(-Header:pair)// is det.
% ```abnf
% header-field = field-name ":" OWS field-value OWS
% ```

'header-field'(Key3-D) -->
  'field-name'(Key2), {atom_string(Key1, Key2)},
  ":", 'OWS',
  rest(Cs), {'field-value'(Cs, Key1, D)},
  {atomic_list_concat([llo,Key1], :, Key3)}.



%! host(-Host:dict)// is det.
% ```abnf
% Host = uri-host [ ":" port ] ; Section 2.7.1
% ```

host(D2) -->
  'uri-host'(UriHost),
  {D1 = _{'@type': 'llo:Host', 'llo:uri_host': UriHost}},
  (":" -> port(Port), {D2 = D1.put(_{'llo:port': Port})} ; {D2 = D1}).



%! hour(-Hour:between(0,99))// is det.
% ```abnf
% hour = 2DIGIT
% ```

hour(H) --> #(2, 'DIGIT', Ds), {pos_sum(Ds, H)}.



%! 'HTTP-date'(-D)// is det.
% ```abnf
% HTTP-date = IMF-fixdate | obs-date
% ```

'HTTP-date'(D) --> 'IMF-fixdate'(D).
'HTTP-date'(D) --> 'obs-date'(D).



%! 'if-match'(-IfMatch:dict)// is det.
% ```abnf
% If-Match = "*" | 1#entity-tag
% ```

'if-match'(_{'@type': 'llo:IfMatch', 'rdf:type': L}) -->
  ("*" -> {L = []} ; +#('entity-tag', L)).



%! 'if-modified-since'(-Datetime:dict)// is det.
% ```abnf
% If-Modified-Since = HTTP-date
% ```

'if-modified-since'(D) --> 'HTTP-date'(D).



%! 'if-none-match'(-IfNoneMatch:dict)// is det.
% ```abnf
% If-None-Match = "*" | 1#entity-tag
% ```

'if-none-match'(_{'@type': 'llo:IfNoneMatch', 'rdf:value': L}) -->
  ("*" -> {L = []} ; +#('entity-tag', L)).



%! 'if-range'(-Value:dict)// is det.
% ```abnf
% If-Range = entity-tag | HTTP-date
% ```

'if-range'(D) --> 'entity-tag'(D), !.
'if-range'(D) --> 'HTTP-date'(D).



%! 'if-unmodified-since'(-Datetime:dict)// is det.
% ```abnf
% If-Unmodified-Since = HTTP-date
% ```

'if-unmodified-since'(D) --> 'HTTP-date'(D).



%! 'IMF-fixdate'(-D)// is det.
% ```abnf
% IMF-fixdate = day-name "," SP date1 SP time-of-day SP GMT
%             ; fixed length/zone/capitalization subset of the format
%             ; see Section 3.3 of [RFC5322]
% ```

'IMF-fixdate'(_{'@type': Type, '@value': Lex}) -->
  'day-name'(_DayInWeek), ",", 'SP',
  date1(Y, Mo, D), 'SP',
  'time-of-day'(H, Mi, S), 'SP',
  'GMT',
  {
    rdf_equal(xsd:dateTime, Type),
    xsd_time_string(date_time(Y,Mo,D,H,Mi,S), Type, Lex)
  }.



%! 'last-byte-pos'(-Position:nonneg)// is det.
% ```abnf
% last-byte-pos = 1*DIGIT
% ```

'last-byte-pos'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'last-chunk'(-Extensions:list)// is det.
% ```abnf
% last-chunk = 1*("0") [ chunk-ext ] CRLF
% ```
%
% @bug It's a mistake to make chunk-ext optional when its also Kleene star.

'last-chunk'(Exts) --> +("0"), 'chunk-ext'(Exts), 'CRLF'.



%! 'last-modified'(-Datetime:dict)// is det.
% ```abnf
% Last-Modified = HTTP-date
% ```

'last-modified'(D) --> 'HTTP-date'(D).



%! location(-Uri:dict)// is det.
% ```abnf
% Location = URI-reference
% ```

location(D) --> 'URI-reference'(D).



%! 'max-forwards'(-Max:nonneg)// is det.
% ```abnf
% Max-Forwards = 1*DIGIT
% ```

'max-forwards'(_{'@type': 'xsd:nonNegativeInteger', '@value': N}) -->
  +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'media-range'(-MediaRange:dict)// is det.
% ```abnf
% media-range = ( "*/*"
%               | ( type "/" "*" )
%               | ( type "/" subtype )
%               ) *( OWS ";" OWS parameter )
% ```

'media-range'(
  _{
    '@type': 'llo:MediaRange',
    'llo:parameters': Parameters,
    'llo:subtype': Subtype,
    'llo:type': Type
  }
) -->
  (   "*/*"
  ->  {Subtype = "*", Type = "*"}
  ;   type(Type), "/*"
  ->  {Subtype = "*"}
  ;   type(Type), "/", subtype(Subtype)
  ),
  *(sep_parameter, Parameters).



%! 'media-type'(-MediaType:dict)// is det.
% ```abnf
% media-type = type "/" subtype *( OWS ";" OWS parameter )
% ```

'media-type'(D2) -->
  {D1 = _{'@type':'llo:MediaType','llo:type':Type,'llo:subtype':Subtype}},
  type(Type), "/", subtype(Subtype),
  (+(sep_parameter, L) -> {D2 = D1.put(_{'llo:parameters': L})} ; {D2 = D1}).

sep_parameter(Parameter) --> 'OWS', ";", 'OWS', parameter(Parameter).



%! 'message-body'(-Body:list(code))// is det.
% ```abnf
% message-body = *OCTET
% ```

'message-body'(Cs) --> *('OCTET', Cs).



%! method(-Method:string)// is det.
% ```abnf
% method = token
% ```
%
% The following methods are defined by HTTP 1.1:
%   * CONNECT
%   * DELETE
%   * GET
%   * HEAD
%   * OPTIONS
%   * POST
%   * PUT
%   * TRACE

method(S) --> token(S).



%! minute(-Minute:between(0,99))// is det.
% ```abnf
% minute = 2DIGIT
% ```

minute(H) --> #(2, 'DIGIT', Ds), {pos_sum(Ds, H)}.



%! month(-Month:between(1,12))// is det.
% ```abnf
% month = %x4A.61.6E ;   "Jan", case-sensitive
%       | %x46.65.62 ;   "Feb", case-sensitive
%       | %x4D.61.72 ;   "Mar", case-sensitive
%       | %x41.70.72 ;   "Apr", case-sensitive
%       | %x4D.61.79 ;   "May", case-sensitive
%       | %x4A.75.6E ;   "Jun", case-sensitive
%       | %x4A.75.6C ;   "Jul", case-sensitive
%       | %x41.75.67 ;   "Aug", case-sensitive
%       | %x53.65.70 ;   "Sep", case-sensitive
%       | %x4F.63.74 ;   "Oct", case-sensitive
%       | %x4E.6F.76 ;   "Nov", case-sensitive
%       | %x44.65.63 ;   "Dec", case-sensitive
% ```

month(1)  --> atom_ci('Jan'), !.
month(2)  --> atom_ci('Feb'), !.
month(3)  --> atom_ci('Mar'), !.
month(4)  --> atom_ci('Apr'), !.
month(5)  --> atom_ci('May'), !.
month(6)  --> atom_ci('Jun'), !.
month(7)  --> atom_ci('Jul'), !.
month(8)  --> atom_ci('Aug'), !.
month(9)  --> atom_ci('Sep'), !.
month(10) --> atom_ci('Oct'), !.
month(11) --> atom_ci('Nov'), !.
month(12) --> atom_ci('Dec').



%! 'obs-date'(-D)// is det.
% ```abnf
% obs-date = rfc850-date | asctime-date
% ```

'obs-date'(D)  --> 'rfc850-date'(D), !.
'obs-date'(D) --> 'asctime-date'(D).



%! 'obs-fold'// is det.
% ```abnf
% obs-fold = CRLF 1*( SP | HTAB )   ; obsolete line folding
% ```

'obs-fold' --> 'CRLF', +(sp_or_htab).



%! 'obs-text'(?Code:code)// is det.
% ```abnf
% obs-text = %x80-FF
% ```

'obs-text'(C) --> [C], {between(0x80, 0xFF, C)}.



%! 'opaque-tag'(-OpaqueTag:string)// .
% ```abnf
% opaque-tag = DQUOTE *etagc DQUOTE
% ```

'opaque-tag'(S) --> 'DQUOTE', *(etagc, Cs), 'DQUOTE', {string_codes(S, Cs)}.



%! 'other-content-range'(-ContentRange:dict)// is det.
% ```abnf
% other-content-range = other-range-unit SP other-range-resp
% ```

'other-content-range'(_{
  '@type': 'llo:OtherContentRange',
  'llo:range': Range,
  'llo:unit': Unit
}) -->
  'other-range-unit'(Unit),
  'SP',
  'other-range-resp'(Range).



%! 'other-range-resp'(-String:string)// is det.
% ```abnf
% other-range-resp = *CHAR
% ```

'other-range-resp'(S) --> *('CHAR', Cs), {string_codes(S, Cs)}.



%! 'other-range-set'(-RangeSet:string)// is det.
% ```abnf
% other-range-set = 1*VCHAR
% ```

'other-range-set'(S) --> +('VCHAR', Cs), {pos_sum(Cs, S)}.



%! 'other-range-unit'(-RangeUnit:string)// is det.
% ```abnf
% other-range-unit = token
% ```

'other-range-unit'(S) --> token(S).



%! 'other-ranges-specifier'(-RangeSpecifier:dict)// is det.
% ```abnf
% other-ranges-specifier = other-range-unit "=" other-range-set
% ```

'other-ranges-specifier'(_{
  '@type': 'llo:OtherRangesSpecifier',
  'llo:other_range_unit': OtherRangeUnit,
  'llo:other_range_set': OtherRangeSet
}) -->
  'other-range-unit'(OtherRangeUnit), "=", 'other-range-set'(OtherRangeSet).



%! 'OWS'// is det.
% ```abnf
% OWS = *( SP | HTAB )   ; optional whitespace
% ```

'OWS' --> *(sp_or_htab).



%! parameter(-Parameter:dict)// is det.
% ```abnf
% parameter = token "=" ( token | quoted-string )
% ```

parameter(_{'@type': 'llo:Parameter', 'llo:key': Key, 'llo:value': Value}) -->
  token(Key), "=", (token(Value), ! ; 'quoted-string'(Value)).



%! 'partial-URI'(-PartialUri:dict)// is det.
% ```abnf
% partial-URI = relative-part [ "?" query ]
% ```

'partial-URI'(D2) -->
  'relative-part'(D1),
  ("?" -> query(Query), {D2 = D1.put(_{'uri:query': Query})} ; {D2 = D1}).



%! pragma(?Directives:list)// is det.
% ```abnf
% Pragma = 1#pragma-directive
% ```

pragma(L) --> +#('pragma-directive', L).



%! 'pragma-directive'(-Value)// is det.
% ```abnf
% pragma-directive = "no-cache" | extension-pragma
% ```

'pragma-directive'("no-cache") --> atom_ci('no-cache'), !.
'pragma-directive'(D)          --> 'extension-pragma'(D).



%! product(-Product:dict)// is det.
% ```abnf
% product = token ["/" product-version]
% ```

product(D2) -->
  token(Name),
  {D1 = _{'@type': 'llo:Product', 'llo:name': Name}},
  (   "/"
  ->  'product-version'(Version),
      {D2 = D1.put(_{'llo:version': Version})}
  ;   {D2 = D1}
  ).



%! 'product-version'(-Version:string)// is det.
% ```abnf
% product-version = token
% ```

'product-version'(S) --> token(S).



%! 'proxy-authenticate'(-Challenges:list(dict))// is det.
% ```abnf
% Proxy-Authenticate = 1#challenge
% ```

'proxy-authenticate'(L) --> +#(challenge, L).



%! 'proxy-authorization'(-Credentials:dict)// is det.
% ```abnf
% Proxy-Authorization = credentials
% ```

'proxy-authorization'(D) --> credentials(D).



%! protocol(-Protocol:dict)// is det.
% ```abnf
% protocol = protocol-name ["/" protocol-version]
% ```

protocol(D2) -->
  'protocol-name'(Name),
  {D1 = _{'@type': 'llo:Protocol', 'llo:name': Name}},
  (   "/"
  ->  'protocol-version'(Version),
      {D2 = D1.put(_{'llo:version': Version})}
  ;   {D2 = D1}
  ).



%! 'protocol-name'(-Name:string)// .
% ```abnf
% protocol-name = token
% ```

'protocol-name'(S) --> token(S).



%! 'protocol-version'(-Version:string)// .
% ```abnf
% protocol-version = token
% ```

'protocol-version'(S) --> token(S).



%! pseudonym(-Pseudonym:string)// .
% ```abnf
% pseudonym = token
% ```

pseudonym(S) --> token(S).



%! qdtext(?Code:code)// is det.
% ```abnf
% qdtext = HTAB | SP | %x21 | %x23-5B | %x5D-7E | obs-text
% ```

qdtext(C)    --> 'HTAB'(C).
qdtext(C)    --> 'SP'(C).
qdtext(0x21) --> [0x21].
qdtext(C)    --> [C], {(between(0x23, 0x5B, C), ! ; between(0x5D, 0x7E, C))}.
qdtext(C)    --> 'obs-text'(C).



%! 'quoted-pair'(?Code:code)// .
% ```abnf
% quoted-pair = "\" ( HTAB | SP | VCHAR | obs-text )
% ```

'quoted-pair'(C) --> "\\", ('HTAB'(C) ; 'SP'(C) ; 'VCHAR'(C) ; 'obs-text'(C)).



%! 'quoted-string'(-String:string)// is det.
% ```abnf
% quoted-string = DQUOTE *( qdtext | quoted-pair ) DQUOTE
% ```

'quoted-string'(S) -->
  'DQUOTE', *(quoted_string_code, Cs), 'DQUOTE',
  {string_codes(S, Cs)}.

quoted_string_code(C) --> qdtext(C).
quoted_string_code(C) --> 'quoted-pair'(C).



%! qvalue(-Value:between(0.0,1.0))// is det.
% ```abnf
% qvalue = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ```

qvalue(N)   -->
  "0",
  (   "."
  ->  'm*n'(0, 3, 'DIGIT', Ds),
      {pos_frac(Ds, N0), N is float(N0)}
  ;   {N = 0}
  ).
qvalue(1.0) -->
  "1",
  ("." -> 'm*n'(0, 3, "0") ; "").



%! range(-Range:dict)// is det.
% ```abnf
% Range = byte-ranges-specifier | other-ranges-specifier
% ```

range(_{'@type': 'llo:Range', 'rdf:value': D}) -->
  ('byte-ranges-specifier'(D), ! ; 'other-ranges-specifier'(D)).



%! 'range-unit'(-Value)// is det.
% ```abnf
% range-unit = bytes-unit | other-range-unit
% ```

'range-unit'("bytes") --> 'bytes-unit', !.
'range-unit'(S)       --> 'other-range-unit'(S).



%! rank(-Rank:between(0.0,1.0))// is det.
% ```abnf
% rank = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ```

rank(N)   --> "0", ("." ->  'm*n'(0, 3, 'DIGIT', Ds), {pos_frac(Ds, N0), N is float(N0)} ; {N = 0}).
rank(1.0) --> "1", ("." -> 'm*n'(0, 3, "0") ; "").



%! 'received-by'(-Receiver:dict)// is det.
% ```abnf
% received-by = ( uri-host [ ":" port ] ) | pseudonym
% ```

'received-by'(_{'@type': 'llo:Receiver', 'llo:uri_host': UriHost, 'llo:port': Port}) -->
  'uri-host'(UriHost), !,
  (":" -> port(Port) ; {Port = 80}).
'received-by'(_{'@type': 'llo:Receiver', 'llo:pseudonym': Pseudonym}) --> pseudonym(Pseudonym).



%! 'received-protocol'(-Protocol:dict)// is det.
% ```abnf
% received-protocol = [ protocol-name "/" ] protocol-version
% ```

'received-protocol'(D2) -->
  {D1 = _{'@type': 'llo:Protocol', 'llo:version': Version}},
  ('protocol-name'(Name), "/" -> {D2 = D1.put(_{'llo:name': Name})} ; {D2 = D1}),
  'protocol-version'(Version).



%! referer(-Uri:dict)// is det.
% ```abnf
% Referer = absolute-URI | partial-URI
% ```

referer(D) --> ('absolute-URI'(D), ! ; 'partial-URI'(D)).



%! 'retry-after'(-Value:dict)// is det.
% ```abnf
% Retry-After = HTTP-date | delay-seconds
% ```

'retry-after'(D) --> 'HTTP-date'(D), !.
'retry-after'(D) --> 'delay-seconds'(D).



%! 'rfc850-date'(-Date:dict)// is det.
% ```abnf
% rfc850-date  = day-name-l "," SP date2 SP time-of-day SP GMT
% ```

'rfc850-date'(_{'@type': Type, '@value': Lex}) -->
  'day-name-l'(D),
  ",",
  'SP',
  date2(Y, Mo, D),
  'SP',
  'time-of-day'(H, Mi, S),
  'SP',
  'GMT',
  {
    rdf_equal(xsd:dateTime, Type),
    xsd_time_string(date_time(Y,Mo,D,H,Mi,S), Type, Lex)
  }.



%! 'RWS'// is det.
% ```abnf
% RWS = 1*( SP | HTAB )   ; required whitespace
% ```

'RWS' --> +(sp_or_htab).



%! second(-Second:between(0,99))// is det.
% ```abnf
% second = 2DIGIT
% ```
%
% @tbd Define an XSD for the range [0,99].

second(N) --> #(2, 'DIGIT', Ds), {pos_sum(Ds, N)}.



%! server(-Values:list(string))// is det.
% ```abnf
% Server = product *( RWS ( product | comment ) )
% ```

server([H|T]) --> product(H), *(sep_product_or_comment, T).



%! subtype(-Subtype:string)// is det.
% ```abnf
% subtype = token
% ```

subtype(S) --> token(S).



%! 'suffix-byte-range-spec'(-Value:dict)// is det.
% ```abnf
% suffix-byte-range-spec = "-" suffix-length
% ```

'suffix-byte-range-spec'(_{'@type': 'xsd:negativeInteger', '@value': N2}) -->
  "-", 'suffix-length'(N1), {N2 is -N1}.



%! 'suffix-length'(-Length:nonneg)// is det.
% ```abnf
% suffix-length = 1*DIGIT
% ```

'suffix-length'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 't-codings'(-TCodings)// is det.
% ```abnf
% t-codings = "trailers" | ( transfer-coding [ t-ranking ] )
% ```

't-codings'("trailers") --> atom_ci(trailers).
't-codings'(D2) -->
  'transfer-coding'(TransferCoding),
  {D1 = _{'@type': 'llo:TCoding', 'llo:transfer_coding': TransferCoding}},
  ('t-ranking'(Rank) -> {D2 = D1.put(_{'llo:t-ranking': Rank})} ; {D2 = D1}).



%! 't-ranking'(-Rank:dict)// is det.
% ```abnf
% t-ranking = OWS ";" OWS "q=" rank
% ```
%
% @tbd Define an XSD for the range [0.0,1.0].

't-ranking'(_{'@type': 'xsd:decimal', '@value': Rank}) -->
  'OWS', ";", 'OWS', atom_ci('q='), rank(Rank).



%! tchar(?Code:code)// is det.
% ```abnf
% tchar = "!" | "#" | "$" | "%" | "&" | "'" | "*"
%       | "+" | "-" | "." | "^" | "_" | "`" | "|" | "~"
%       | DIGIT | ALPHA   ; any VCHAR, except delimiters


tchar(C)   --> 'ALPHA'(C).
tchar(C)   --> 'DIGIT'(_, C).
tchar(0'!) --> "!".
tchar(0'#) --> "#".
tchar(0'$) --> "$".
tchar(0'%) --> "%".
tchar(0'&) --> "&".
tchar(0'') --> "'".
tchar(0'*) --> "*".
tchar(0'+) --> "+".
tchar(0'-) --> "-".
tchar(0'.) --> ".".
tchar(0'^) --> "^".
tchar(0'_) --> "_".
tchar(0'`) --> "`".
tchar(0'|) --> "|".
tchar(0'~) --> "~".



%! te(-TCodings:list)// is det.
% ```abnf
% TE = #t-codings
% ```

te(L) --> '*#'('t-codings', L).



%! 'time-of-day'(-H, -Mi, -S)// is det.
% ```abnf
% time-of-day = hour ":" minute ":" second
%             ; 00:00:00 - 23:59:60 (leap second)
% ```

'time-of-day'(H, Mi, S) --> hour(H), ":", minute(Mi), ":", second(S).



%! token(-Token:string)// is det.
% ```abnf
% token = 1*tchar
% ```

token(S) --> +(tchar, Cs), {string_codes(S, Cs)}.



%! token68(-Token:string)// is det.
% ```
% token68 = 1*( ALPHA | DIGIT | "-" | "." | "_" | "~" | "+" | "/" ) *"="

token68(S) --> +(token68_code, Cs), {string_codes(S, Cs)}, *("=").

token68_code(C)   --> 'ALPHA'(C).
token68_code(C)   --> 'DIGIT'(_, C).
token68_code(0'-) --> "-".
token68_code(0'.) --> ".".
token68_code(0'_) --> "_".
token68_code(0'~) --> "~".
token68_code(0'+) --> "+".
token68_code(0'/) --> "/".



%! trailer(-Fields:list(string))// is det.
% ```abnf
% Trailer = 1#field-name
% ```

trailer(L) --> +#('field-name', L).



%! 'transfer-coding'(-TransferCoding)// is det.
% ```abnf
% transfer-coding = "chunked"
%                 | "compress"
%                 | "deflate"
%                 | "gzip"
%                 | transfer-extension
% ```

'transfer-coding'("chunked")  --> atom_ci(chunked), !.
'transfer-coding'("compress") --> atom_ci(compress), !.
'transfer-coding'("deflate")  --> atom_ci(deflate), !.
'transfer-coding'("gzip")     --> atom_ci(gzip), !.
'transfer-coding'(D)          --> 'transfer-extension'(D).



%! 'transfer-encoding'(-TransferCodings:list(dict))// is det.
% ```abnf
% Transfer-Encoding = 1#transfer-coding
% ```

'transfer-encoding'(L) --> +#('transfer-coding', L).



%! 'transfer-extension'(-TransferExtension:dict)// is det.
% ```abnf
% transfer-extension = token *( OWS ";" OWS transfer-parameter )
% ```

'transfer-extension'(_{
  '@type': 'llo:TransferExtension',
  'llo:token': H,
  'llo:parameters': T
}) -->
  token(H),
  *(sep_transfer_parameter, T).

sep_transfer_parameter(Param) --> 'OWS', ";", 'OWS', 'transfer-parameter'(Param).



%! 'transfer-parameter'(-Parameter:dict)// is det.
% ```abnf
% transfer-parameter = token BWS "=" BWS ( token | quoted-string )
% ```

'transfer-parameter'(_{
  '@type': 'llo:Parameter',
  'llo:key': Key,
  'llo:value': Value
}) -->
  token(Key),
  'BWS',
  "=",
  'BWS',
  (token(Value), ! ; 'quoted-string'(Value)).



%! type(-Type:string)// .
% ```abnf
% type = token
% ```

type(S) --> token(S).



%! 'unsatisfied-range'(-Range:string, -CompleteLength:dict)// is det.
% ```abnf
% unsatisfied-range = "*/" complete-length
% ```

'unsatisfied-range'("*", Length) --> "*/", 'complete-length'(Length).



%! upgrade(-Protocols:list(dict))// is det.
% ```abnf
% upgrade = 1#protocol
% ```

upgrade(L) --> +#(protocol, L).



%! 'user-agent'(-UserAgent:list(string))// is det.
% ```abnf
% User-Agent = product *( RWS ( product | comment ) )
% ```

'user-agent'([H|T]) --> product(H), *(sep_product_or_comment, T).



%! vary(-FieldNames:list(dict))// is det.
% ```abnf
% Vary = "*" | 1#field-name
% ```

vary(L) --> ("*" -> {L = []} ; +#('field-name', L)).



%! via(-Receiver:list(dict))// is det.
% ```abnf
% Via = 1#( received-protocol RWS received-by [ RWS comment ] )
% ```

via(L) --> '+#'(via_component, L).

via_component(D2) -->
  'received-protocol'(ReceivedProtocol),
  'RWS',
  'received-by'(ReceivedBy),
  {D1 = _{
    '@type': 'llo:ViaComponent',
    'llo:received_protocol': ReceivedProtocol,
    'llo:received_by': ReceivedBy
  }},
  ('RWS' -> comment(Z), {D2 = D1.put('llo:comment', Z)} ; {D2 = D1}).



%! 'warn-agent'(-Agent:dict)// is det.
% ```abnf
% warn-agent = ( uri-host [ ":" port ] ) | pseudonym
%            ; the name or pseudonym of the server adding
%            ; the Warning header field, for use in debugging
%            ; a single "-" is recommended when agent unknown
% ```

'warn-agent'(D3) -->
  {D1 = _{'@type': 'llo:WarningAgent'}},
  (   'uri-host'(UriHost),
      (":" -> port(Port), {D2 = D1.put(_{'llo:port': Port})} ; {D2 = D1})
  ->  {D3 = D2.put(_{'llo:uri_host': UriHost})}
  ;   pseudonym(Pseudonym)
  ->  {D3 = D1.put(_{'llo:pseudonym': Pseudonym})}
  ).




%! 'warn-code'(-WarningCode:dict)// is det.
% ```abnf
% warn-code = 3DIGIT
% ```

'warn-code'(_{'@type': 'xsd:nonNegativeInteger', '@value': N}) -->
  #(3, 'DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'warn-date'(-Datetime:dict)// is det.
% ```abnf
% warn-date = DQUOTE HTTP-date DQUOTE
% ```

'warn-date'(D) --> 'DQUOTE', 'HTTP-date'(D), 'DQUOTE'.



%! 'warn-text'(?Text:string)// is det.
% ```abnf
% warn-text = quoted-string
% ```

'warn-text'(S) --> 'quoted-string'(S).



%! warning(-Warnings:list(dict))// is det.
% ```abnf
% warning = 1#warning-value
% ```

warning(L) --> +#('warning-value', L).



%! 'warning-value'(-WarningValue:dict)// is det.
% ```abnf
% warning-value = warn-code SP warn-agent SP warn-text [ SP warn-date ]
% ```

'warning-value'(D2) -->
  'warn-code'(WarnCode), 'SP',
  'warn-agent'(WarnAgent), 'SP',
  'warn-text'(WarnText),
  {
    D1 = _{
      '@type': 'llo:WarningValue',
      'llo:warning_agent': WarnAgent,
      'llo:warning_code': WarnCode,
      'llo:warning_text': WarnText
    }
  },
  (   'SP'
  ->  'warn-date'(WarnDate),
      {D2 = D1.put(_{'llo:warning_date': WarnDate})}
  ;   {D2 = D1}
  ).



%! weak// is det.
% ```abnf
% weak = %x57.2F   ; "W/", case-sensitive
% ```

weak --> "W/".



%! weight(-Weight:between(0.0,1.0))// is det.
% ```abnf
% weight = OWS ";" OWS "q=" qvalue
% ```

weight(_{'@type': 'xsd:double', '@value': N}) -->
  'OWS', ";", 'OWS', atom_ci('q='), qvalue(N).



%! 'www-authenticate'(-Challenges:list(dict))// is det.
% ```abnf
% WWW-Authenticate = 1#challenge
% ```

'www-authenticate'(L) --> +#(challenge, L).



%! year(-Year:between(0,9999))// is det.
% ```
% year = 4DIGIT
% ```

year(Y) --> #(4, 'DIGIT', Ds), {pos_sum(Ds, Y)}.





% HELPERS %

header_field_eol(Header) -->
  'header-field'(Header),
  'CRLF'.


sep_product_or_comment(X) -->
  'RWS',
  (product(X), ! ; comment(X)).


sp_or_htab --> 'SP'.
sp_or_htab --> 'HTAB'.
