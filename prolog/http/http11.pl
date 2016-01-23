:- module(
  http11,
  [
    'field-name'//1, % -Name:string
    'header-field'//1, % -Header:pair
    method//1, % -Method:string
    'OWS'//0,
    'rfc850-date'//1 % -Date:datetime
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

The numer zero i.o. a datetime value.

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
@version 2015/11-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'CHAR'//1, % ?Code:code
     'CR'//0,
     'CRLF'//0,
     'CTL'//0,
     'CTL'//1, % ?Code:code
     'DIGIT'//1, % ?Weight:nonneg
     'DIGIT'//2, % ?Weight:nonneg
                 % ?Code:code
     'DQUOTE'//0,
     'HEXDIG'//1, % ?Weight:nonneg
     'HTAB'//0,
     'HTAB'//1, % ?Code:code
     'LF'//0,
     'OCTET'//1, % ?Code:code
     'SP'//0,
     'SP'//1, % ?Code:code
     'VCHAR'//1 % ?Code:code
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
     'Language-Tag'//1 as 'language-tag' % -LanguageTag:list(string)
   ]).
:- use_module(library(mail/rfc5322), [
     mailbox//1 % -Pair:pair(string)
   ]).
:- use_module(library(pair_ext)).
:- use_module(library(uri/rfc3986), [
     'absolute-URI'//1, % -AbsoluteUri:dict
     authority//1, % -Authority:dict
     fragment//1, % -Fragment:string
     host//1 as 'uri-host', % -Host:dict
     'path-abempty'//1, % -Segments:list(string)
     port//1, % -Port:nonneg
     query//1, % -Query:string
     'relative-part'//1, % -RelativeUri:dict
     segment//1, % -Segment:string
     'URI-reference'//1 % -UriReference:dict
   ]).

:- meta_predicate('field-content'(3,-,?,?)).
:- meta_predicate('field-value'(3,-,?,?)).





%! 'absolute-form'(-AbsoluteUri:dict)// is det.
% ```abnf
% absolute-form = absolute-URI
% ```

'absolute-form'(Uri) --> 'absolute-URI'(Uri).



%! 'absolute-path'(-AbsolutePath:list(string))// is det.
% ```abnf
% absolute-path = 1*( "/" segment )
% ```

'absolute-path'(L) --> +(sep_segment, L).
sep_segment(S) --> "/", segment(S).



%! accept(-Pairs:list(pair(dict,pair(between(0.0,1.0),list))))// is det.
% ```abnf
% Accept = #( media-range [ accept-params ] )
% ```

accept(L) --> '*#'(accept_part, L).
accept_part(MediaRange-Params) -->
  'media-range'(MediaRange),
  ?('accept-params', Params).



%! 'accept-charset'(-Charactersets:list(dict))// is det.
% ```abnf
% Accept-Charset = 1#( ( charset | "*" ) [ weight ] )
% ```

'accept-charset'(L) --> +#(accept_charset_part, L).
accept_charset_part(X) -->
  (charset(Charset) -> "" ; "*", {Charset = "*"}),
  (weight(Weight) -> {X = Weight-Charset} ; {X = Charset}).



%! 'accept-encoding'(-Codings:list)// is det.
% ```abnf
% Accept-Encoding = #( codings [ weight ] )
% ```

'accept-encoding'(L) --> '*#'(accept_encoding_part, L).
accept_encoding_part(X) -->
  codings(Coding),
  (weight(Weight) -> {X = Weight-Coding} ; {X = Coding}).



%! 'accept-ext'(-Extension)// is det.
% ```abnf
% accept-ext = OWS ";" OWS token [ "=" ( token | quoted-string ) ]
% ```

'accept-ext'(Ext) -->
  'OWS', ";", 'OWS',
  token(N),
  ("=" -> (token(V), ! ; 'quoted-string'(V)), {Ext = N-V} ; {Ext = N}).



%! 'accept-language'(-LanguageRanges:list)// is det.
% ```abnf
% Accept-Language = 1#( language-range [ weight ] )
% ```

'accept-language'(L) --> +#(accept_language_part, L).
accept_language_part(X) -->
  'language-range'(LRange),
  (weight(Weight) -> {X = Weight-LRange} ; {X = LRange}).



%! 'accept-params'(-Pair:pair(between(0.0,1.0),list))// is det.
% ```abnf
% accept-params = weight *( accept-ext )
% ```

'accept-params'(Weight-Exts) --> weight(Weight), *('accept-ext', Exts).



%! 'accept-ranges'(-Ranges:list(or([oneof([bytes]),string])))// is det.
% ```abnf
% Accept-Ranges = acceptable-ranges
% ```

'accept-ranges'(L) --> 'acceptable-ranges'(L).



%! 'acceptable-ranges'(-Ranges:list(or([oneof([bytes]),string])))// is det.
% ```abnf
% acceptable-ranges = 1#range-unit | "none"
% ```

'acceptable-ranges'(L)  --> +#('range-unit', L), !.
'acceptable-ranges'([]) --> atom_ci(none).



%! age(-Age:nonneg)// is det.
% ```abnf
% Age = delta-seconds
% ```

age(N) --> 'delta-seconds'(N).



%! allow(-Methods:list(string))// is det.
% ```abnf
% Allow = #method
% ```

allow(L) --> '*#'(method, L).



%! 'asctime-date'(-Date:compound)// is det.
% ```abnf
% asctime-date = day-name SP date3 SP time-of-day SP year
% ```

'asctime-date'(datetime(Y,Mo,D,H,Mi,S,0)) -->
  'day-name'(D),
  'SP',
  date3(Mo, D),
  'SP',
  'time-of-day'(time(H,Mi,S)),
  'SP',
  year(Y).



%! 'asterisk-form'// is det.
% ```abnf
% asterisk-form = "*"
% ```

'asterisk-form' --> "*".



%! 'auth-param'(-Param:list(string))// is det.
% ```abnf
% auth-param = token BWS "=" BWS ( token | quoted-string )
% ```

'auth-param'(N-V) -->
  token(N),
  'BWS',
  "=",
  'BWS',
  (token(V), ! ; 'quoted-string'(V)).



%! 'auth-scheme'(-Scheme:string)// is det.
% ```abnf
% auth-scheme = token
% ```

'auth-scheme'(S) --> token(S).



%! 'authority-form'(-Authority:dict)// is det.
% ```abnf
% authority-form = authority
% ```

'authority-form'(D) --> authority(D).



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



%! 'byte-content-range'(-ContentRange:dict)// is det.
% ```abnf
% byte-content-range = bytes-unit SP ( byte-range-resp | unsatisfied-range )
% ```

'byte-content-range'(
  content_range{length: Length, range: Range, unit: bytes}
) -->
  'bytes-unit',
  'SP',
  ('byte-range-resp'(Range, Length), ! ; 'unsatisfied-range'(Range, Length)).



%! 'byte-range'(-Range:pair(nonneg))// is det.
% ```abnf
% byte-range = first-byte-pos "-" last-byte-pos
% ```

'byte-range'(First-Last) -->
  'first-byte-pos'(First),
  "-",
  'last-byte-pos'(Last).



%! 'byte-range-resp'(-Range:pair(nonneg), -Length:nonneg)// is det.
% ```abnf
% byte-range-resp = byte-range "/" ( complete-length | "*" )
% ```

'byte-range-resp'(Range, Length) -->
  'byte-range'(Range),
  "/",
  ("*" -> {Length = *} ; 'complete-length'(Length)).



%! 'byte-range-spec'(-Value:or([nonneg,pair(nonneg)]))// is det.
% ```abnf
% byte-range-spec = first-byte-pos "-" [ last-byte-pos ]
% ```

'byte-range-spec'(X) -->
  'first-byte-pos'(First),
  "-",
  ('last-byte-pos'(Last) -> {X = First-Last} ; {X = First}).



%! 'byte-ranges-specifier'(-RangeSpecifier:dict)// is det.
% ```abnf
% byte-ranges-specifier = bytes-unit "=" byte-range-set
% ```

'byte-ranges-specifier'(range_specifier{ranges: Ranges, unit: bytes}) -->
  'bytes-unit',
  "=",
  'byte-range-set'(Ranges).



%! 'byte-range-set'(-Ranges:list(or([integer,pair(nonneg)])))// is det.
% ```abnf
% byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
% ```

'byte-range-set'(Ranges) --> +#(byte_range_set_part, Ranges).
byte_range_set_part(Range) --> 'byte-range-spec'(Range).
byte_range_set_part(Range) --> 'suffix-byte-range-spec'(Range).



%! 'bytes-unit'// is det.
% ```abnf
% bytes-unit = "bytes"
% ```

'bytes-unit' --> atom_ci(bytes).



%! 'cache-control'(?Directives:list(compound))// .
% ```abnf
% Cache-Control = 1#cache-directive
% ```

'cache-control'(L) --> +#('cache-directive', L).



%! 'cache-directive'(?Directive)// is det.
% ```abnf
% cache-directive = token [ "=" ( token | quoted-string ) ]
% ```

'cache-directive'(X) -->
  token(N),
  (   "="
  ->  (token(V), ! ; 'quoted-string'(V)),
      {atom_string(N0, N), X =.. [N0,V]}
  ;   {X = N}
  ).

/*
cache_request_directive('no-cache') --> "no-cache".
cache_request_directive('no-store') --> "no-store".
cache_request_directive('max-age'(Delta)) --> 'delta-seconds'(Delta).
cache_request_directive('max-stale'(Delta)) --> 'delta-seconds'(Delta).
cache_request_directive('min-fresh'(Delta)) --> 'delta-seconds'(Delta).
cache_request_directive('no-transform') --> "no-transform".
cache_request_directive('only-if-cached') --> "only-if-cached".
cache_response_directive(public) --> "public".
cache_response_directive(private(L)) --> '*#'('field-name', L).
cache_response_directive('no-cache'(L)) --> '*#'('field-name', L).
cache_response_directive('no-store') --> "no-store".
cache_response_directive('no-transform') --> "no-transform".
cache_response_directive('must-revalidate') --> "must-revalidate".
cache_response_directive('proxy-revalidate') --> "proxy-revalidate".
cache_response_directive('max-age'(Delta)) --> 'delta-seconds'(Delta).
cache_response_directive('s-maxage'(Delta)) --> 'delta-seconds'(Delta).
*/



%! challenge(-Challenge:dict)// is det.
% ```abnf
% challenge = auth-scheme [ 1*SP ( token68 | #auth-param ) ]
% ```

challenge(challenge{scheme: Scheme, parameters: Params}) -->
  'auth-scheme'(Scheme),
  (   +('SP')
  ->  ('*#'('auth-param', Params), ! ; token68(S), {Params = [S]})
  ;   {Params = []}
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

chunk(chunk{data: Cs, extensions: Exts, size: Size}) -->
  'chunk-size'(Size),
  'chunk-ext'(Exts),
  'CRLF',
  'chunk-data'(Cs),
  'CRLF'.



%! 'chunk-data'(-Codes:list(code))// is det.
% ```abnf
% chunk-data = 1*OCTET ; a sequence of chunk-size octets
% ```

'chunk-data'(Cs) --> +('OCTET', Cs).



%! 'chunked-body'(-ChunkedBody:dict)// is det.
% ```abnf
% chunked-body = *chunk last-chunk trailer-part CRLF
% ```

'chunked-body'(chunked_body{chunks: Chunks, extensions: Exts, headers: Headers}) -->
  *(chunk, Chunks),
  'last-chunk'(Exts),
  'trailer-part'(Headers),
  'CRLF'.



%! 'chunk-ext'(-Extension:list)// is det.
% ```abnf
% chunk-ext = *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
% ```

'chunk-ext'(L) --> *(sep_chunk_ext, L).
sep_chunk_ext(Ext) -->
  ";",
  'chunk-ext-name'(N),
  ("=" -> 'chunk-ext-val'(V), {Ext = N-V} ; {Ext = N}).



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



%! codings(-Coding)// is det.
% ```abnf
% codings = content-coding | "identity" | "*"
% ```

codings(S) --> 'content-coding'(S).
codings(identity) --> atom_ci(identity).
codings(*) --> "*".



%! comment(-Comment:string)// is det.
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment(S) --> dcg_string(comment_codes1, S).
comment_codes1([0'(|T]) -->
  "(",
  comment_codes2(T0),
  ")",
  {append(T0, [0')], T)}.
comment_codes2([H|T]) --> ctext(H), !, comment_codes2(T).
comment_codes2([H|T]) --> 'quoted-pair'(H), !, comment_codes2(T).
comment_codes2(L) --> comment_codes1(L), !.
comment_codes2([]) --> "".



%! 'complete-length'(-Length:nonneg)// is det.
% ```abnf
% complete-length = 1*DIGIT
% ```

'complete-length'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! connection(-Options:list(string))// is det.
% ```abnf
% 'Connection'(S) --> 1#(connection-option)
% ```

connection(L) --> +#('connection-option', L).



%! 'connection-option'(-Option:string)// .
% ```abnf
% connection-option = token
% ```

'connection-option'(S) --> token(S).



%! 'content-coding'(-Coding:string)// is det.
% ```abnf
% content-coding = token
% ```

'content-coding'(S) --> token(S).



%! 'content-encoding'(-Encodings:list)// is det.
% ```abnf
% Content-Encoding = 1#content-coding
% ```

'content-encoding'(L) --> +#('content-coding', L).



%! 'content-language'(-LanguageTags:list(list(string)))// .
% ```abnf
% Content-Language = 1#language-tag
% ```

'content-language'(L) --> +#('language-tag', L).



%! 'content-length'(-Length:nonneg)// is det.
% ```abnf
% Content-Length = 1*DIGIT
% ```

'content-length'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'content-location'(-Location:atom)// is det.
% ```abnf
% Content-Location = absolute-URI | partial-URI
% ```

'content-location'(Uri) --> 'absolute-URI'(Uri), !.
'content-location'(Uri) --> 'partial-URI'(Uri).



%! 'content-range'(-ContentRange:dict)// is det.
% ```abnf
% Content-Range = byte-content-range | other-content-range
% ```

'content-range'(D) --> 'byte-content-range'(D), !.
'content-range'(D) --> 'other-content-range'(D).



%! 'content-type'(-MediaType:dict)// .
% ```abnf
% Content-Type = media-type
% ```

'content-type'(MT) --> 'media-type'(MT).



%! credentials(-Credentials:dict)// is det.
% ```abnf
% credentials = auth-scheme [ 1*SP ( token68 | #auth-param ) ]
% ```

credentials(credentials{scheme: Scheme, parameters: Params}) -->
  'auth-scheme'(Scheme),
  (   +('SP')
  ->  (token68(S) -> {Params = [S]} ; '*#'('auth-param', Params))
  ;   {Params = []}
  ).



%! ctext(?Code:code)// is det.
% ```abnf
% ctext = HTAB | SP | %x21-27 | %x2A-5B | %x5D-7E | obs-text
% ```

ctext(C) --> 'HTAB'(C).
ctext(C) --> 'SP'(C).
ctext(C) --> [C], {(between(0x21, 0x27, C), ! ; between(0x2A, 0x5B, C), ! ; between(0x5D, 0x7E, C))}.
ctext(C) --> 'obs-text'(C).



%! date(-Datetime:compound)// is det.
% ```abnf
% Date = HTTP-date
% ```

date(DT) --> 'HTTP-date'(DT).



%! date1(-Date:compound)// is det.
% ```abnf
% date1 = day SP month SP year   ; e.g., 02 Jun 1982
% ```

date1(date(Y,Mo,D)) --> day(D), 'SP', month(Mo), 'SP', year(Y).



%! date2(-Date:compound)// is det.
% ```abnf
% date2 = day "-" month "-" 2DIGIT   ; e.g., 02-Jun-82
% ```

date2(date(Y,Mo,D)) -->
  day(D),
  "-",
  month(Mo),
  "-",
  #(2, 'DIGIT', Ds),
  {pos_sum(Ds, Y)}.



%! date3(-Month:between(1,12), -Day:between(0,99))// is det.
% ```abnf
% date3 = month SP ( 2DIGIT | ( SP 1DIGIT ))   ; e.g., Jun  2
% ```

date3(Mo, D) -->
  month(Mo),
  'SP',
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



%! 'delay-seconds'(-Seconds:nonneg)// is det.
% ```abnf
% delay-seconds = 1*DIGIT
% ```

'delay-seconds'(S) --> +('DIGIT', Ds), {pos_sum(Ds, S)}.



%! 'delta-seconds'(-Delta:nonneg)// is det.
% ```abnf
% delta-seconds = 1*DIGIT
% ```

'delta-seconds'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'entity-tag'(-EntityTag:dict)// is det.
% ```abnf
% entity-tag = [ weak ] opaque-tag
% ```

'entity-tag'(entity_tag{weak: Weak, opaque_tag: OTag}) -->
  (weak -> {Weak = true} ; {Weak = false}),
  'opaque-tag'(OTag).



%! expires(?Datetime:compound)// .
% ```abnf
% Expires = HTTP-date
% ```

expires(DT) --> 'HTTP-date'(DT).



%! etag(-ETag:dict)// is det.
% Used for Web cache validation and optimistic concurrency control.
%
% ```abnf
% ETag = "ETag" ":" entity-tag
% ```

etag(D) --> 'entity-tag'(D).



%! etagc(-Code:code)// .
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



%! 'extension-pragma'(?Extension:or([string,pair(string)]))// .
% ```abnf
% extension-pragma = token [ "=" ( token | quoted-string ) ]
% ```

'extension-pragma'(T) -->
  token(N),
  ("=" -> (token(V), ! ; 'quoted-string'(V)), {T = N-V} ; {T = N}).



%! 'field-content'(+Name:atom, -Value)// .
% ```abnf
% field-content = field-vchar [ 1*( SP | HTAB ) field-vchar ]
% ```

'field-content'(ModPred, 'field-content'{status: Status, value: Value}) -->
  {strip_module(ModPred, _, Pred)},
  (   {current_predicate(Pred/3)}
  ->  (   dcg_call(ModPred, Value),
          % This should fail in case only /part/ of the HTTP header is parsed.
          eos
      ->  {Status = valid}
      ;   rest(Cs),
          {
            Status = invalid,
            string_codes(Value, Cs),
            debug(http(parse), "Buggy HTTP header ~a: ~a", [Pred,Value])
          }
      )
  ;   rest(Cs),
      {
        Status = unrecognized,
        string_codes(Value, Cs),
        (   known_unknown(Pred)
        ->  true
        ;   debug(http(parse), "No parser for HTTP header ~a: ~s", [Pred,Value])
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



%! 'field-name'(-Name:string)// .
% ```abnf
% field-name = token
% ```

'field-name'(LowerS) --> token(S), {string_lower(S, LowerS)}.



%! 'field-vchar'(-Code:code)// is det.
% ```abnf
% field-vchar = VCHAR | obs-text
% ```

'field-vchar'(C) --> 'VCHAR'(C).
'field-vchar'(C) --> 'obs-text'(C).



%! 'field-value'(+Name:atom, -Value)// .
% ```abnf
% field-value = *( field-content | 'obs-fold' )
% ```

'field-value'(Name, Value) --> 'field-content'(Name, Value), !.
'field-value'(_, _) --> 'obs-fold'.



%! 'first-byte-pos'(-Position:nonneg)// is det.
% ```abnf
% first-byte-pos = 1*DIGIT
% ```

'first-byte-pos'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! from(-Mailbox:pair(string))// is det.
% ```abnf
% From = mailbox
% ```

from(Pair) --> mailbox(Pair).



%! 'GMT'// is det.
% ```abnf
% GMT = %x47.4D.54   ; "GMT", case-sensitive
% ```

'GMT' --> atom_ci('GMT').



%! 'header-field'(-Header:pair)// is det.
% ```abnf
% header-field = field-name ":" OWS field-value OWS
% ```

'header-field'(Pred-Value) -->
  'field-name'(Name),
  {atom_string(Pred, Name)},
  ":",
  'OWS',
  'field-value'(Pred, Value),
  'OWS'.



%! host(-Host:dict)// is det.
% ```abnf
% Host = uri-host [ ":" port ] ; Section 2.7.1
% ```

host(D) -->
  'uri-host'(D0),
  (":" -> port(Port), {D = D0.put(port, Port)} ; {D = D0}).



%! hour(-Hour:between(0,99))// is det.
% ```abnf
% hour = 2DIGIT
% ```

hour(H) --> #(2, 'DIGIT', Ds), {pos_sum(Ds, H)}.



%! 'HTTP-date'(?Datetime:compound)// is det.
% ```abnf
% HTTP-date = IMF-fixdate | obs-date
% ```

'HTTP-date'(DT) --> 'IMF-fixdate'(DT), !.
'HTTP-date'(DT) --> 'obs-date'(DT).



%! 'HTTP-message'(-Message:dict)// is det.
% ```abnf
% HTTP-message = start-line *( header-field CRLF ) CRLF [ message-body ]
% ```

'HTTP-message'(D) -->
  'start-line'(D0),
  *(header_field_eol, L),
  'CRLF',
  def('message-body', Cs, []),
  {D = D0.put([body-Cs,headers-L])},
  'CRLF'.



%! 'HTTP-name'// is det.
% ```abnf
% HTTP-name = %x48.54.54.50   ; "HTTP", case-sensitive
% ```

'HTTP-name' --> [0x48,0x54,0x54,0x50].



%! 'http-URI'(-HttpUri:dict)// is det.
% ```abnf
% http-URI = "http:" "//" authority path-abempty [ "?" query ] [ "#" fragment ]
% ```

'http-URI'(D) -->
  "http://",
  authority(Auth),
  'path-abempty'(Path),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Frag) ; ""),
  {
    L1 = [authority-Auth,fragment-Frag,path-Path,query-Query],
    exclude(pair_has_var_value, L1, L2),
    dict_pairs(D, http_uri, L2)
  }.



%! 'HTTP-version'(?Version:dict)// is det.
% ```abnf
% HTTP-version = HTTP-name "/" DIGIT "." DIGIT
% ```

'HTTP-version'(version{major: Major, minor: Minor}) -->
  'HTTP-name',
  "/",
  digit(Major),
  ".",
  digit(Minor).



%! 'https-URI'(-HttpsUri:dict)// is det.
% ```abnf
% https-URI = "https:" "//" authority path-abempty [ "?" query ] [ "#" fragment ]
% ```

'https-URI'(D) -->
  atom_ci('https://'),
  authority(Auth),
  'path-abempty'(Path),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Frag) ; ""),
  {
    L1 = [authority-Auth,fragment-Frag,path-Path,query-Query],
    exclude(pair_has_var_value, L1, L2),
    dict_pairs(D, http_uri, L2)
  }.



%! 'if-match'(-Value)// is det.
% ```abnf
% If-Match = "*" | 1#entity-tag
% ```

'if-match'(*) --> "*".
'if-match'(L) --> +#('entity-tag', L).



%! 'if-modified-since'(-Datetime:compound)// is det.
% ```abnf
% If-Modified-Since = HTTP-date
% ```

'if-modified-since'(DT) --> 'HTTP-date'(DT).



%! 'if-none-match'(-Value)// is det.
% ```abnf
% If-None-Match = "*" | 1#entity-tag
% ```

'if-none-match'(*) --> "*".
'if-none-match'(L) --> +#('entity-tag', L).



%! 'if-range'(-Value:or([datetime,dict]))// is det.
% ```abnf
% If-Range = entity-tag | HTTP-date
% ```

'if-range'(EntityTag) --> 'entity-tag'(EntityTag), !.
'if-range'(DT) --> 'HTTP-date'(DT).



%! 'if-unmodified-since'(-Datetime:compound)// is det.
% ```abnf
% If-Unmodified-Since = HTTP-date
% ```

'if-unmodified-since'(DT) --> 'HTTP-date'(DT).



%! 'IMF-fixdate'(-Datetime:datetime)// is det.
% ```abnf
% IMF-fixdate = day-name "," SP date1 SP time-of-day SP GMT
%             ; fixed length/zone/capitalization subset of the format
%             ; see Section 3.3 of [RFC5322]
% ```

'IMF-fixdate'(datetime(Y,Mo,D,H,Mi,S,_)) -->
  'day-name'(_DayInWeek),
  ",",
  'SP',
  date1(date(Y,Mo,D)),
  'SP',
  'time-of-day'(time(H,Mi,S)),
  'SP',
  'GMT'.



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



%! 'last-modified'(-Datetime:compound)// .
% ```abnf
% Last-Modified = HTTP-date
% ```

'last-modified'(DT) --> 'HTTP-date'(DT).



%! location(-Uri:dict)// is det.
% ```abnf
% Location = URI-reference
% ```

location(Uri) --> 'URI-reference'(Uri).



%! 'max-forwards'(-Max:nonneg)// is det.
% ```abnf
% Max-Forwards = 1*DIGIT
% ```

'max-forwards'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'media-range'(-MediaRange:dict)// is det.
% ```abnf
% media-range = ( "*/*"
%               | ( type "/" "*" )
%               | ( type "/" subtype )
%               ) *( OWS ";" OWS parameter )
% ```

'media-range'(media_range{parameters: Parameters, subtype: Subtype, type: Type}) -->
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

'media-type'(media_type{type: Type, subtype: Subtype, parameters: Params}) -->
  type(Type),
  "/",
  subtype(Subtype),
  *(sep_parameter, Params).
sep_parameter(Param) --> 'OWS', ";", 'OWS', parameter(Param).



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



%! 'obs-date'(-Datetime:compound)// is det.
% ```abnf
% obs-date = rfc850-date | asctime-date
% ```

'obs-date'(DT) --> 'rfc850-date'(DT), !.
'obs-date'(DT) --> 'asctime-date'(DT).



%! 'obs-fold'// is det.
% ```abnf
% obs-fold = CRLF 1*( SP | HTAB )   ; obsolete line folding
% ```

'obs-fold' --> 'CRLF', +(sp_or_htab).



%! 'obs-text'(-Code:code)// is det.
% ```abnf
% obs-text = %x80-FF
% ```

'obs-text'(C) --> [C], {between(0x80, 0xFF, C)}.



%! 'opaque-tag'(?OpaqueTag:string)// .
% ```abnf
% opaque-tag = DQUOTE *etagc DQUOTE
% ```

'opaque-tag'(S) --> 'DQUOTE', *(etagc, Cs), 'DQUOTE', {string_codes(S, Cs)}.



%! 'other-content-range'(-ContentRange:dict)// is det.
% ```abnf
% other-content-range = other-range-unit SP other-range-resp
% ```

'other-content-range'(content_range{range: Range, unit: Unit}) -->
  'other-range-unit'(Unit),
  'SP',
  'other-range-resp'(Range).



%! 'other-range-resp'(-String:string)// is det.
% ```abnf
% other-range-resp = *CHAR
% ```

'other-range-resp'(S) --> *('CHAR', Cs), {string_codes(S, Cs)}.



%! 'origin-form'(-Uri:dict)// is det.
% ```abnf
% origin-form = absolute-path [ "?" query ]
% ```

'origin-form'(D) -->
  'absolute-path'(D0),
  ("?" -> query(Query), {D = D0.put(query, Query)} ; {D = D0}).



%! 'other-range-set'(-RangeSet:string)// is det.
% ```abnf
% other-range-set = 1*VCHAR
% ```

'other-range-set'(S) --> +('VCHAR', Cs), {pos_sum(Cs, S)}.



%! 'other-range-unit'(?RangeUnit:string)// .
% ```abnf
% other-range-unit = token
% ```

'other-range-unit'(S) --> token(S).



%! 'other-ranges-specifier'(-RangeSpecifier:dict)// is det.
% ```abnf
% other-ranges-specifier = other-range-unit "=" other-range-set
% ```

'other-ranges-specifier'(range_specifier{ranges: [Range], unit: Unit}) -->
  'other-range-unit'(Unit),
  "=",
  'other-range-set'(Range).



%! 'OWS'// is det.
% ```abnf
% OWS = *( SP | HTAB )   ; optional whitespace
% ```

'OWS' --> *(sp_or_htab).



%! parameter(?Parameter:pair(string))// .
% ```abnf
% parameter = token "=" ( token | quoted-string )
% ```

parameter(N-V) --> token(N), "=", (token(V), ! ; 'quoted-string'(V)).



%! 'partial-URI'(-Uri:dict)// is det.
% ```abnf
% partial-URI = relative-part [ "?" query ]
% ```

'partial-URI'(D) -->
  'relative-part'(D0),
  {dict_pairs(D0, relative_part, T)},
  ("?" -> query(Query) ; ""),
  {
    exclude(pair_has_var_value, [query-Query|T], L),
    dict_pairs(D, partial_uri, L)
  }.



%! pragma(?Directives:list)// .
% ```abnf
% Pragma = 1#pragma-directive
% ```

pragma(L) --> +#('pragma-directive', L).



%! 'pragma-directive'(?Directive:or([oneof(['no-cache']),string,pair(string)]))// .
% ```abnf
% pragma-directive = "no-cache" | extension-pragma
% ```

'pragma-directive'('no-cache') --> atom_ci('no-cache'), !.
'pragma-directive'(X)          --> 'extension-pragma'(X).



%! product(-Product:dict)// is det.
% ```abnf
% product = token ["/" product-version]
% ```

product(D) -->
  token(Name),
  ("/" -> 'product-version'(Version), {T = [version-Version]} ; {T = []}),
  {dict_pairs(D, product, [name-Name|T])}.



%! 'product-version'(-Version:string)// is det.
% ```
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

protocol(D) -->
  'protocol-name'(N),
  ("/" -> 'protocol-version'(V), {T = [version-V]} ; {T = []}),
  {dict_pairs(D, protocol, [name-N|T])}.



%! 'protocol-name'(?Name:string)// .
% ```abnf
% protocol-name = token
% ```

'protocol-name'(S) --> token(S).



%! 'protocol-version'(?Version:string)//.
% ```abnf
% protocol-version = token
% ```

'protocol-version'(S) --> token(S).



%! pseudonym(?Pseudonym:string)// .
% ```abnf
% pseudonym = token
% ```

pseudonym(S) --> token(S).



%! qdtext(-Code:code)// is det.
% ```abnf
% qdtext = HTAB | SP | %x21 | %x23-5B | %x5D-7E | obs-text
% ```

qdtext(C)    --> 'HTAB'(C).
qdtext(C)    --> 'SP'(C).
qdtext(0x21) --> [0x21].
qdtext(C)    --> [C], {(between(0x23, 0x5B, C), ! ; between(0x5D, 0x7E, C))}.
qdtext(C)    --> 'obs-text'(C).



%! 'quoted-pair'(-Code:code)// .
% ```abnf
% quoted-pair = "\" ( HTAB | SP | VCHAR | obs-text )
% ```

'quoted-pair'(C) --> "\\", ('HTAB'(C) ; 'SP'(C) ; 'VCHAR'(C) ; 'obs-text'(C)).



%! 'quoted-string'(?String:string)// .
% ```abnf
% quoted-string = DQUOTE *( qdtext | quoted-pair ) DQUOTE
% ```

'quoted-string'(S) -->
  'DQUOTE',
  *(quoted_string_code, Cs),
  'DQUOTE',
  {string_codes(S, Cs)}.
quoted_string_code(C) --> qdtext(C).
quoted_string_code(C) --> 'quoted-pair'(C).



%! qvalue(-Value:between(0.0,1.0))// is det.
% ```abnf
% qvalue = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ```

qvalue(N) -->
  "0",
  (   "."
  ->  'm*n'(0, 3, 'DIGIT', Ds),
      {pos_frac(Ds, N0), N is float(N0)}
  ;   {N = 0}
  ).
qvalue(1.0) --> "1", ("." -> 'm*n'(0, 3, "0") ; "").



%! range(-RangeSpecifier:dict)// is det.
% ```abnf
% Range = byte-ranges-specifier | other-ranges-specifier
% ```

range(D) --> 'byte-ranges-specifier'(D).
range(D) --> 'other-ranges-specifier'(D).



%! 'range-unit'(-Value)// is det.
% ```abnf
% range-unit = bytes-unit | other-range-unit
% ```

'range-unit'(bytes) --> 'bytes-unit', !.
'range-unit'(S) --> 'other-range-unit'(S).



%! rank(-Rank:between(0.0,1.0))// is det.
% ```abnf
% rank = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ```

rank(N) -->
  "0",
  (   "."
  ->  'm*n'(0, 3, 'DIGIT', Ds),
      {pos_frac(Ds, N0), N is float(N0)}
  ;   {N = 0}
  ).
rank(1.0) --> "1", ("." -> 'm*n'(0, 3, "0") ; "").



%! 'reason-phrase'(-Reason:string)// is det.
% ```abnf
% reason-phrase = *( HTAB | SP | VCHAR | obs-text )
% ```

'reason-phrase'(S) --> *(reason_phrase_code, Cs), {string_codes(S, Cs)}.
reason_phrase_code(C) --> 'HTAB'(C).
reason_phrase_code(C) --> 'SP'(C).
reason_phrase_code(C) --> 'VCHAR'(C).
reason_phrase_code(C) --> 'obs-text'(C).



%! 'received-by'(-Receiver:dict)// .
% ```abnf
% received-by = ( uri-host [ ":" port ] ) | pseudonym
% ```

'received-by'(receiver{host: Host, port: Port}) -->
  'uri-host'(Host), !,
  (":" -> port(Port) ; {Port = 80}).
'received-by'(receiver{pseudonym: S}) --> pseudonym(S).



%! 'received-protocol'(?Protocol:dict)// .
% ```abnf
% received-protocol = [ protocol-name "/" ] protocol-version
% ```

'received-protocol'(protocol{name: N, version: V}) -->
  'protocol-name'(N),
  "/", !,
  'protocol-version'(V).
'received-protocol'(protocol{version: V}) -->
  'protocol-version'(V).



%! referer(-Uri:dict)// is det.
% ```abnf
% Referer = absolute-URI | partial-URI
% ```

referer(Uri) --> 'absolute-URI'(Uri), !.
referer(Uri) --> 'partial-URI'(Uri).



%! 'request-line'(-Message:dict)// is det.
% ```abnf
% request-line = method SP request-target SP HTTP-version CRLF
% ```

'request-line'(
  http_message{
    method: Method,
    request_target: Iri,
    type: request,
    version: Version
  }
) -->
  method(Method),
  'SP',
  'request-target'(Iri),
  'SP',
  'HTTP-version'(Version),
  'CRLF'.



%! 'request-target'(-Target)// is det.
% ```abnf
% request-target = origin-form
%                | absolute-form
%                | authority-form
%                | asterisk-form
% ```

'request-target'(D) --> 'origin-form'(D).
'request-target'(D) --> 'absolute-form'(D).
'request-target'(D) --> 'authority-form'(D).
'request-target'(*) --> 'asterisk-form'.



%! 'retry-after'(-Value:or([compound,nonneg]))// is det.
% ```abnf
% Retry-After = HTTP-date | delay-seconds
% ```

'retry-after'(DT) --> 'HTTP-date'(DT), !.
'retry-after'(S) --> 'delay-seconds'(S).



%! 'rfc850-date'(-Datetime:compound)// is det.
% ```abnf
% rfc850-date  = day-name-l "," SP date2 SP time-of-day SP GMT
% ```

'rfc850-date'(datetime(Y,Mo,D,H,Mi,S,0)) -->
  'day-name-l'(D),
  ",",
  'SP',
  date2(date(Y,Mo,D)),
  'SP',
  'time-of-day'(time(H,Mi,S)),
  'SP',
  'GMT'.



%! 'RWS'// is det.
% ```abnf
% RWS = 1*( SP | HTAB )   ; required whitespace
% ```

'RWS' --> +(sp_or_htab).



%! second(-Second:between(0,99))// is det.
% ```abnf
% second = 2DIGIT
% ```

second(H) --> #(2, 'DIGIT', Ds), {pos_sum(Ds, H)}.



%! server(-Values:list(string))// is det.
% ```abnf
% Server = product *( RWS ( product | comment ) )
% ```

server([H|T]) --> product(H), *(sep_product_or_comment, T).



%! 'start-line'(-StartLine:dict)// is det.
% ```abnf
% start-line = request-line | status-line
% ```

'start-line'(D) --> 'request-line'(D), !.
'start-line'(D) --> 'status-line'(D).



%! 'status-code'(-Status:between(0,999))// is det.
% ```abnf
% status-code = 3DIGIT
% ```

'status-code'(N) --> #(3, 'DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'status-line'(-Message:dict)// is det.
% ```abnf
% status-line = HTTP-version SP status-code SP reason-phrase CRLF
% ```

'status-line'(
  http_message{
    reason: Reason,
    status: Status,
    type: response,
    version: Version
  }
) -->
  'HTTP-version'(Version),
  'SP',
  'status-code'(Status),
  'SP',
  'reason-phrase'(Reason),
  'CRLF'.



%! subtype(-Subtype:string)// is det.
% ```abnf
% subtype = token
% ```

subtype(S) --> token(S).



%! 'suffix-byte-range-spec'(-Value:nonpos)// is det.
% ```abnf
% suffix-byte-range-spec = "-" suffix-length
% ```

'suffix-byte-range-spec'(N) --> "-", 'suffix-length'(N).



%! 'suffix-length'(-Length:nonneg)// is det.
% ```abnf
% suffix-length = 1*DIGIT
% ```

'suffix-length'(N) --> +('DIGIT', Ds), {pos_sum(Ds, N)}.



%! 't-codings'// is det.
% ```abnf
% t-codings = "trailers" | ( transfer-coding [ t-ranking ] )
% ```

't-codings'(trailers) --> atom_ci(trailers).
't-codings'(X) -->
  'transfer-coding'(TransferCoding),
  ('t-ranking'(Rank) -> {X = TransferCoding-Rank} ; {X = TransferCoding}).



%! 't-ranking'(-Rank:between(0.0,1.0))// is det.
% ```abnf
% t-ranking = OWS ";" OWS "q=" rank
% ```

't-ranking'(Rank) --> 'OWS', ";", 'OWS', atom_ci('q='), rank(Rank).



%! tchar(-Code:code)// is det.
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



%! te(-TCodings)// is det.
% ```abnf
% TE = #t-codings
% ```

te(L) --> '*#'('t-codings', L).



%! 'time-of-day'(-Time:compound)// is det.
% ```abnf
% time-of-day = hour ":" minute ":" second
%             ; 00:00:00 - 23:59:60 (leap second)
% ```

'time-of-day'(time(H,Mi,S)) --> hour(H), ":", minute(Mi), ":", second(S).



%! token(?Token:string)// is det.
% ```abnf
% token = 1*tchar
% ```

token(S) --> +(tchar, Cs), {string_codes(S, Cs)}.



%! token68(-Token:code)// .
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



%! 'trailer-part'(-Headers:list(pair(string)))// is det.
% ```abnf
% trailer-part = *( header-field CRLF )
% ```

'trailer-part'(Headers) --> *(header_field_eol, Headers).



%! 'transfer-coding'(?TransferCoding:dict)// is det.
% ```abnf
% transfer-coding = "chunked"
%                 | "compress"
%                 | "deflate"
%                 | "gzip"
%                 | transfer-extension
% ```

'transfer-coding'(transfer_coding{token: chunked, parameter: []}) -->
  atom_ci(chunked), !.
'transfer-coding'(transfer_coding{token: compress, parameter: []}) -->
  atom_ci(compress), !.
'transfer-coding'(transfer_coding{token: deflate, paramers: []}) -->
  atom_ci(deflate), !.
'transfer-coding'(transfer_coding{token: gzip, parameters: []}) -->
  atom_ci(gzip), !.
'transfer-coding'(D) -->
  'transfer-extension'(D).



%! 'transfer-encoding'(-TransferEncoding:list(or([oneof([chunked]),dict])))// .
% ```abnf
% Transfer-Encoding = 1#transfer-coding
% ```

'transfer-encoding'(L) --> +#('transfer-coding', L).



%! 'transfer-extension'(?TransferExtension:dict)// .
% ```abnf
% transfer-extension = token *( OWS ";" OWS transfer-parameter )
% ```

'transfer-extension'(transfer_coding{token: H, parameters: T}) -->
  token(H),
  *(sep_transfer_parameter, T).
sep_transfer_parameter(Param) --> 'OWS', ";", 'OWS', 'transfer-parameter'(Param).



%! 'transfer-parameter'(-Parameter:pair)// is det.
% ```abnf
% transfer-parameter = token BWS "=" BWS ( token | quoted-string )
% ```

'transfer-parameter'(N-V) -->
  token(N),
  'BWS',
  "=",
  'BWS',
  (token(V), ! ; 'quoted-string'(V)).



%! type(?Type:string)// .
% ```abnf
% type = token
% ```

type(S) --> token(S).



%! 'unsatisfied-range'(-Range:*, -Length:nonneg)// is det.
% ```abnf
% unsatisfied-range = "*/" complete-length
% ```

'unsatisfied-range'(*, Length) --> "*/", 'complete-length'(Length).



%! upgrade// is det.
% ```abnf
% upgrade = 1#protocol
% ```

upgrade(L) --> +#(protocol, L).



%! 'user-agent'(-UserAgent:list(string)
% ```abnf
% User-Agent = product *( RWS ( product | comment ) )
% ```

user_agent([H|T]) --> product(H), *(sep_product_or_comment, T).



%! vary(-Value)// is det.
% ```abnf
% Vary = "*" | 1#field-name
% ```

vary(*) --> "*", !.
vary(L) --> +#('field-name', L).



%! via(?Value:list(dict))// .
% ```abnf
% Via = 1#( received-protocol RWS received-by [ RWS comment ] )
% ```

via(L) --> '+#'(via_component, L).

via_component(D) -->
  'received-protocol'(X),
  'RWS',
  'received-by'(Y),
  ('RWS' -> comment(Z), {T = [comment-Z]} ; {T = []}),
  {dict_pairs(D, via, ['received-protocol'-X,'received-by'-Y|T])}.



%! 'warn-agent'(-Agent)// is det.
% ```abnf
% warn-agent = ( uri-host [ ":" port ] ) | pseudonym
%            ; the name or pseudonym of the server adding
%            ; the Warning header field, for use in debugging
%            ; a single "-" is recommended when agent unknown
% ```

'warn-agent'(D) -->
  'uri-host'(D0),
  (":" -> port(Port), {D = D0.put(port, Port)} ; {D = D0}).
'warn-agent'(S) --> pseudonym(S).



%! 'warn-code'(-Code:between(0,999))// is det.
% ```abnf
% warn-code = 3DIGIT
% ```

'warn-code'(N) --> #(3, 'DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'warn-date'(-Datetime:datetime)// is det.
% ```abnf
% warn-date = DQUOTE HTTP-date DQUOTE
% ```

'warn-date'(DT) --> 'DQUOTE', 'HTTP-date'(DT), 'DQUOTE'.



%! 'warn-text'(-Text:string)// is det.
% ```abnf
% warn-text = quoted-string
% ```

'warn-text'(S) --> 'quoted-string'(S).



%! warning(-Values:list(dict))// is det.
% ```abnf
% warning = 1#warning-value
% ```

warning(L) --> +#('warning-value', L).



%! 'warning-value'(-Value:dict)// is det.
% ```abnf
% warning-value = warn-code SP warn-agent SP warn-text [ SP warn-date ]
% ```

'warning-value'(D) -->
  'warn-code'(Code),
  'SP',
  'warn-agent'(Agent),
  'SP',
  'warn-text'(Text),
  ('SP' -> 'warn-date'(DT), {T = [date-DT]} ; {T = []}),
  {dict_pairs(D, warning_value, [agent-Agent,code-Code,text-Text|T])}.



%! weak// is det.
% ```abnf
% weak = %x57.2F   ; "W/", case-sensitive
% ```

weak --> "W/".



%! weight(-Weight:between(0.0,1.0))// is det.
% ```abnf
% weight = OWS ";" OWS "q=" qvalue
% ```

weight(N) --> 'OWS', ";", 'OWS', atom_ci('q='), qvalue(N).



%! 'www-authenticate'(?Challenges:list(dict))// is det.
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

header_field_eol(Header) --> 'header-field'(Header), 'CRLF'.


sep_product_or_comment(X) --> 'RWS', (product(X), ! ; comment(X)).


sp_or_htab --> 'SP'.
sp_or_htab --> 'HTAB'.
