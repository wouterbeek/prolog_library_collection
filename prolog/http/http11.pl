:- module(
  http11,
  [
    charset//1,         % -Charset:atom
    'content-type'//1,  % -Mime:compound
    'field-name'//1,    % -Name:atom
    'header-field'//1,  % -Header:pair
    http_known/1,       % ?Key:atom
    'media-type'//1,    % -MT:compound
    method//1,          % -Method:atom
    'OWS'//0,
    qvalue//1,          % -Val:between(0.0,1.0)
    'rfc850-date'//1    % -DT:compound
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
@version 2015/11-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1,  % ?C
     'CHAR'//1,   % ?C
     'CR'//0,
     'CRLF'//0,
     'CTL'//0,
     'CTL'//1,    % ?C
     'DIGIT'//1,  % ?Weight:nonneg
     'DIGIT'//2,  % ?Weight:nonneg, ?C
     'DQUOTE'//0,
     'HEXDIG'//1, % ?Weight:nonneg
     'HTAB'//0,
     'HTAB'//1,   % ?C
     'LF'//0,
     'OCTET'//1,  % ?C
     'SP'//0,
     'SP'//1,     % ?C
     'VCHAR'//1   % ?C
   ]).
:- use_module(library(dict_ext)).
:- use_module(library(http/cors)).
:- use_module(library(http/csp2)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/http_alternative_services)).
:- use_module(library(http/rfc2295)).
:- use_module(library(http/rfc5988)).
:- use_module(library(http/rfc6265)).
:- use_module(library(http/rfc6266)).
:- use_module(library(http/rfc6797)).
:- use_module(library(http/rfc7034)).
:- use_module(library(lists)).
:- use_module(library(ltag/rfc4647), [
     'language-range'//1 % -LRange:list(atom)
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 as 'language-tag' % -LTag:list(atom)
   ]).
:- use_module(library(mail/rfc5322), [
     mailbox//1 % -Pair:pair(atom)
   ]).
:- use_module(library(pair_ext)).
:- use_module(library(sgml)).
:- use_module(library(uri/rfc3986), [
     'absolute-URI'//1,     % -Uri:compound
     fragment//1,           % -Frag:atom
     host//1 as 'uri-host', % -Host:compound
     port//1,               % -Port:nonneg
     query//1,              % -Query:atom
     'relative-part'//2,    % -Auth:compound, -Segments:list(atom)
     'URI-reference'//1     % -Uri:compound
   ]).
:- use_module(library(uri/uri_ext)).

:- discontiguous
    http_known_known/1.

http_known_known('access-control-allow-credentials').
http_known_known('access-control-allow-headers').
http_known_known('access-control-allow-methods').
http_known_known('access-control-allow-origin').
http_known_known('access-control-expose-headers').%@tbd
http_known_known('access-control-request-method').
http_known_known('alt-svc').
http_known_known(alternates).
http_known_known('content-disposition').
http_known_known('content-security-policy').
http_known_known('content-security-policy-report-only').
http_known_known(csp).
http_known_known(link).
http_known_known(origin).
http_known_known('set-cookie').
http_known_known('set-cookie2').
http_known_known('strict-transport-security').
http_known_known(tcn).
http_known_known('x-content-type-options').
http_known_known('x-frame-options').
http_known_known('x-powered-by').
http_known_known('x-xss-protection').

:- meta_predicate
    'field-content'(3, -, ?, ?).





%! accept(-MTs:list(compound))// is det.
%
% ```abnf
% Accept = #( media-range [ accept-params ] )
% ```

http_known_known(accept).
accept(MTs) -->
  '*#'(accept_value0, Pairs), !,
  {desc_pairs_values(Pairs, MTs)}.

accept_value0(Weight-accept_value(MT,Exts)) -->
  'media-range'(MT),
  ('accept-params'(Weight, Exts) -> "" ; {Exts = [], Weight = 1.0}).



%! 'accept-charset'(-Charsets:list(atom))// is det.
%
% ```abnf
% Accept-Charset = 1#( ( charset | "*" ) [ weight ] )
% ```

http_known_known('accept-charset').
'accept-charset'(Charsets) -->
  +#(accept_charset_value0, Pairs), !,
  {desc_pairs_values(Pairs, Charsets)}.

accept_charset_value0(Weight-Charset) -->
  ("*" -> "" ; charset(Charset)),
  optional_weight(Weight).



%! 'accept-encoding'(-Encodings:list(atom))// is det.
%
% ```abnf
% Accept-Encoding = #( codings [ weight ] )
% ```

http_known_known('accept-encoding').
'accept-encoding'(Encodings) -->
  '*#'(accept_encoding_value0, Pairs), !,
  {desc_pairs_values(Pairs, Encodings)}.

accept_encoding_value0(Weight-Encoding) -->
  codings(Encoding),
  optional_weight(Weight).



%! 'accept-ext'(-Ext:or([atom,pair(atom)]))// is det.
%
% ```abnf
% accept-ext = OWS ";" OWS token [ "=" ( token | quoted-string ) ]
% ```

'accept-ext'(Ext) -->
  'OWS', ";", 'OWS',
  token(Key),
  (   "="
  ->  (token(Val), ! ; 'quoted-string'(Val)),
      {Ext = Key-Val}
  ;   {Ext = Key}
  ).



%! 'accept-language'(-LRanges:list(list(atom)))// is det.
%
% ```abnf
% Accept-Language = 1#( language-range [ weight ] )
% ```

http_known_known('accept-language').
'accept-language'(LRanges) -->
  +#(accept_language_value, Pairs), !,
  {desc_pairs_values(Pairs, LRanges)}.

accept_language_value(Weight-LRange) -->
  'language-range'(LRange),
  optional_weight(Weight).



%! 'accept-params'(-Weight, -Exts)// is det.
%
% ```abnf
% accept-params = weight *( accept-ext )
% ```

'accept-params'(Weight, Exts) -->
  weight(Weight),
  *('accept-ext', Exts).



%! 'accept-ranges'(-Ranges:list(dict))// is det.
%
% ```abnf
% Accept-Ranges = acceptable-ranges
% ```

http_known_known('accept-ranges').
'accept-ranges'(Ranges) -->
  'acceptable-ranges'(Ranges).



%! 'acceptable-ranges'(-AcceptableRanges:list(dict))// is det.
%
% ```abnf
% acceptable-ranges = 1#range-unit | "none"
% ```

'acceptable-ranges'(L) -->
  (+#('range-unit', L) -> "" ; atom_ci(none) -> {L = []}).



%! age(-N)// is det.
%
% ```abnf
% Age = delta-seconds
% ```

http_known_known(age).
age(N) -->
  'delta-seconds'(N).



%! allow(-Methods:list(string))// is det.
%
% ```abnf
% Allow = #method
% ```

http_known_known(allow).
allow(L) -->
  '*#'(method, L).



%! 'asctime-date'(-DT)// is det.
%
% ```abnf
% asctime-date = day-name SP date3 SP time-of-day SP year
% ```

'asctime-date'(date_time(Y,Mo,D,H,Mi,S,0)) -->
  'day-name'(D),
  'SP',
  date3(Mo, D),
  'SP',
  'time-of-day'(H, Mi, S),
  'SP',
  year(Y).



%! 'auth-param'(-Pair)// is det.
%
% ```abnf
% auth-param = token BWS "=" BWS ( token | quoted-string )
% ```

'auth-param'(Key-Val) -->
  token(Key),
  'BWS', "=", 'BWS',
  (token(Val), ! ; 'quoted-string'(Val)).



%! 'auth-scheme'(-Scheme:atom)// is det.
%
% ```abnf
% auth-scheme = token
% ```

'auth-scheme'(Scheme) -->
  token(Scheme).



%! authorization(-Credentials:list(pair(atom,list(pair(atom)))))// is det.
%
% ```abnf
% Authorization = credentials
% ```

http_known_known(authorization).
authorization(Credentials) -->
  credentials(Credentials).



%! 'BWS'// is det.
%
% ```abnf
% BWS = OWS   ; "bad" whitespace
% ```

'BWS' -->
  'OWS'.



%! 'byte-content-range'(-Range:pair(nonneg), -Len:nonneg)// is det.
%
% ```abnf
% byte-content-range = bytes-unit SP ( byte-range-resp | unsatisfied-range )
% ```

'byte-content-range'(Range, Len) -->
  'bytes-unit',
  'SP',
  ('byte-range-resp'(Range, Len) -> "" ; 'unsatisfied-range'(Range, Len)).



%! 'byte-range'(-Range:pair(nonneg))// is det.
%
% ```abnf
% byte-range = first-byte-pos "-" last-byte-pos
% ```

'byte-range'(First-Last) -->
  'first-byte-pos'(First),
  "-",
  'last-byte-pos'(Last).



%! 'byte-range-resp'(-Range:pair(nonneg), -Len:nonneg)// is det.
%
% ```abnf
% byte-range-resp = byte-range "/" ( complete-length | "*" )
% ```

'byte-range-resp'(Range, Len) -->
  'byte-range'(Range),
  "/",
  ("*" -> "" ; 'complete-length'(Len)).



%! 'byte-range-spec'(-Range:pair(nonneg))// is det.
%
% ```abnf
% byte-range-spec = first-byte-pos "-" [ last-byte-pos ]
% ```

'byte-range-spec'(FirstPosition-LastPosition) -->
  'first-byte-pos'(FirstPosition),
  "-",
  ?('last-byte-pos'(LastPosition)).



%! 'byte-ranges-specifier'(-Set:list(or([nonneg,pair(nonneg)])))// is det.
%
% ```abnf
% byte-ranges-specifier = bytes-unit "=" byte-range-set
% ```

'byte-ranges-specifier'(Set) -->
  'bytes-unit',
  "=",
  'byte-range-set'(Set).



%! 'byte-range-set'(-Set:list(or([nonneg,pair(nonneg)])))// is det.
%
% ```abnf
% byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
% ```

'byte-range-set'(Set) -->
  +#(byte_range_set_part, Set), !.

byte_range_set_part(Range) --> 'byte-range-spec'(Range).
byte_range_set_part(Len) --> 'suffix-byte-range-spec'(Len).



%! 'bytes-unit'// is det.
%
% ```abnf
% bytes-unit = "bytes"
% ```

'bytes-unit' -->
  atom_ci(bytes).



%! 'cache-control'(-Directives:list(dict))// is det.
%
% ```abnf
% Cache-Control = 1#cache-directive
% ```

http_known_known('cache-control').
'cache-control'(Directives) -->
  +#('cache-directive', Directives).



%! 'cache-directive'(-Directive:or([atom,pair(atom)]))// is det.
%
% ```abnf
% cache-directive = token [ "=" ( token | quoted-string ) ]
% ```

'cache-directive'(Directive) -->
  token(Key),
  (   "="
  ->  (token(Val) -> "" ; 'quoted-string'(Val)),
      {Directive = Key-Val}
  ;   {Directive = Key}
  ).



%! challenge(-Challenge:pair(atom,list(pair(atom))))// is det.
%
% ```abnf
% challenge = auth-scheme [ 1*SP ( token68 | #auth-param ) ]
% ```

challenge(Scheme-Params) -->
  'auth-scheme'(Scheme),
  (   +('SP')
  ->  (   '*#'('auth-param', Params)
      ->  ""
      ;   token68(Param),
          {Params = [Param]}
      )
  ;   {Params = []}
  ).



%! charset(-Charset:atom)// is det.
%
% ```abnf
% charset = token
% ```

charset(Charset) -->
  token(Charset).



%! chunk(-Chunk:compound)// is det.
%
% ```abnf
% chunk = chunk-size [ chunk-ext ] CRLF chunk-data CRLF
% ```
%
% @bug It's a mistake to make chunk-ext optional when its also Kleene star.

chunk(chunk(Size,Cs,Exts)) -->
  'chunk-size'(Size),
  'chunk-ext'(Exts), 'CRLF',
  'chunk-data'(Cs), 'CRLF'.



%! 'chunk-data'(-Codes:list(code))// is det.
%
% ```abnf
% chunk-data = 1*OCTET ; a sequence of chunk-size octets
% ```

'chunk-data'(Cs) -->
  +('OCTET', Cs), !.



%! 'chunk-ext'(-Exts:list(or([atom,pair(atom)])))// is det.
%
% ```abnf
% chunk-ext = *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
% ```

'chunk-ext'(Exts) -->
  *(sep_chunk_ext, Exts), !.

sep_chunk_ext(Ext) -->
  ";",
  'chunk-ext-name'(Key),
  (   "="
  ->  'chunk-ext-val'(Val),
      {Ext = Key-Val}
  ;   {Ext = Key}
  ).



%! 'chunk-ext-name'(-Name:atom)// is det.
%
% ```abnf
% chunk-ext-name = token
% ```

'chunk-ext-name'(Name) -->
  token(Name).



%! 'chunk-ext-val'(-Val:atom)// is det.
%
% ```abnf
% chunk-ext-val = token / quoted-string
% ```

'chunk-ext-val'(Val) -->
  token(Val), !.
'chunk-ext-val'(Val) -->
  'quoted-string'(Val).



%! 'chunk-size'(-Size:nonneg)// is det.
%
% ```abnf
% chunk-size = 1*HEXDIG
% ```

'chunk-size'(Size) -->
  +('HEXDIG', Ds), !,
  {pos_sum(Ds, 16, Size)}.



%! codings(-Coding:atom)// is det.
%
% ```abnf
% codings = content-coding | "identity" | "*"
% ```

codings(Coding) --> 'content-coding'(Coding).
codings(identity) --> atom_ci(identity).
codings(_) --> "*".



%! comment(-Comment:string)// is det.
%
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment(Str) --> dcg_string(comment_codes1, Str).

comment_codes1([0'(|T]) -->
  "(", comment_codes2(T0), ")",
  {append(T0, [0')], T)}.

comment_codes2([H|T]) --> ctext(H), !, comment_codes2(T).
comment_codes2([H|T]) --> 'quoted-pair'(H), !, comment_codes2(T).
comment_codes2(L)     --> comment_codes1(L), !.
comment_codes2([])    --> "".



%! 'complete-length'(-Len:nonneg)// is det.
%
% ```abnf
% complete-length = 1*DIGIT
% ```

'complete-length'(Len) -->
  +('DIGIT', Ds), !,
  {pos_sum(Ds, Len)}.



%! connection(-Opts:list(atom))// is det.
%
% ```abnf
% 'Connection'(S) --> 1#(connection-option)
% ```

http_known_known(connection).
connection(Opts) -->
  +#('connection-option', Opts).



%! 'connection-option'(-Opt)// is det.
%
% ```abnf
% connection-option = token
% ```

'connection-option'(Opt) -->
  token(Opt).



%! 'content-coding'(-Coding:atom)// is det.
%
% ```abnf
% content-coding = token
% ```

'content-coding'(Coding) -->
  token(Coding).



%! 'content-encoding'(-Encodings:list(atom))// is det.
%
% ```abnf
% Content-Encoding = 1#content-coding
% ```

http_known_known('content-encoding').
'content-encoding'(Encodings) -->
  +#('content-coding', Encodings), !.



%! 'content-language'(-LTags:list(list(atom)))// is det.
%
% ```abnf
% Content-Language = 1#language-tag
% ```

http_known_known('content-language').
'content-language'(LTags) -->
  +#('language-tag', LTags), !.



%! 'content-length'(-Len:nonneg)// is det.
%
% ```abnf
% Content-Length = 1*DIGIT
% ```

http_known_known('content-length').
'content-length'(Len) -->
  +('DIGIT', Ds), !,
  {pos_sum(Ds, Len)}.



%! 'content-location'(-Uri:compound)// is det.
%
% ```abnf
% Content-Location = absolute-URI | partial-URI
% ```

http_known_known('content-location').
'content-location'(Uri) --> 'absolute-URI'(Uri), !.
'content-location'(Uri) --> 'partial-URI'(Uri).



%! 'content-range'(-Unit:atom, -Range:pair(nonneg))// is det.
%
% ```abnf
% Content-Range = byte-content-range | other-content-range
% ```

http_known_known('content-range').
'content-range'(byte, Range-Len) -->
  'byte-content-range'(Range, Len).
'content-range'(Unit, Resp) -->
  'other-content-range'(Unit, Resp).



%! 'content-type'(-MT:compound)// is det.
%
% ```abnf
% Content-Type = media-type
% ```

http_known_known('content-type').
'content-type'(MT) -->
  'media-type'(MT).



%! credentials(-Credentials:pair(atom,list(pair(atom))))// is det.
%
% ```abnf
% credentials = auth-scheme [ 1*SP ( token68 | #auth-param ) ]
% ```

credentials(Scheme-Params) -->
  'auth-scheme'(Scheme),
  (   +('SP')
  ->  (   token68(Token)
      ->  {Params = [Token]}
      ;   '*#'('auth-param', Params)
      )
  ;   {Params = []}
  ).



%! ctext(-Code:code)// is det.
%
% ```abnf
% ctext = HTAB | SP | %x21-27 | %x2A-5B | %x5D-7E | obs-text
% ```

ctext(C) --> 'HTAB'(C).
ctext(C) --> 'SP'(C).
ctext(C) --> [C], {(  between(0x21, 0x27, C), !
                  ;   between(0x2A, 0x5B, C), !
                  ;   between(0x5D, 0x7E, C)
                  )}.
ctext(C) --> 'obs-text'(C).



%! date(-DT:compound)// is det.
%
% ```abnf
% Date = HTTP-date
% ```

http_known_known(date).
date(DT) -->
  'HTTP-date'(DT).



%! date1(
%!   -Year:between(0,9999),
%!   -Month:between(1,12),
%!   -Day:between(0,99)
%! )// is det.
%
% ```abnf
% date1 = day SP month SP year   ; e.g., 02 Jun 1982
% ```

date1(Y, Mo, D) -->
  day(D),
  'SP',
  month(Mo),
  'SP',
  year(Y).



%! date2(
%!   -Year:between(0,9999),
%!   -Month:between(1,12),
%!   -Day:beween(0,99)
%! )// is det.
%
% ```abnf
% date2 = day "-" month "-" 2DIGIT   ; e.g., 02-Jun-82
% ```

date2(Y, Mo, D) -->
  day(D), "-",
  month(Mo), "-",
  #(2, 'DIGIT', Ds),
  {pos_sum(Ds, Y)}.



%! date3(-Month:between(1,12), -Day:between(0,99))// is det.
%
% ```abnf
% date3 = month SP ( 2DIGIT | ( SP 1DIGIT ))   ; e.g., Jun  2
% ```

date3(Mo, D) -->
  month(Mo), 'SP',
  (   #(2, 'DIGIT', Ds)
  ->  {pos_sum(Ds, D)}
  ;   'SP',
      #(1, 'DIGIT', D)
  ).



%! day(-Day:between(0,99))// is det.
%
% ```abnf
% day = 2DIGIT
% ```

day(D) -->
  #(2, 'DIGIT', Ds),
  {pos_sum(Ds, D)}.



%! 'day-name'(-Day:between(1,7))// is det.
%
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
%
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
%
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
%
% ```abnf
% delay-seconds = 1*DIGIT
% ```

'delay-seconds'(Seconds) -->
  +('DIGIT', Ds), !,
  {pos_sum(Ds, Seconds)}.



%! 'delta-seconds'(-Delta:nonneg)// is det.
%
% ```abnf
% delta-seconds = 1*DIGIT
% ```

'delta-seconds'(Delta) -->
  +('DIGIT', Ds), !,
  {pos_sum(Ds, Delta)}.



%! 'entity-tag'(-Tag:pair(atom,boolean))// is det.
%
% ```abnf
% entity-tag = [ weak ] opaque-tag
% ```

'entity-tag'(Tag-Weak) -->
  (weak -> {Weak = true} ; {Weak = false}),
  'opaque-tag'(Tag).



%! expires(-DT:compound)// is det.
%
% ```abnf
% Expires = HTTP-date
% ```

http_known_known(expires).
expires(DT) -->
  'HTTP-date'(DT).



%! etag(-EntityTag:pair(atom,boolean))// is det.
%
% Used for Web cache validation and optimistic concurrency control.
%
% ```abnf
% ETag = "ETag" ":" entity-tag
% ```

http_known_known(etag).
etag(EntityTag) -->
  'entity-tag'(EntityTag).



%! etagc(-Code:code)// is det.
%
% ```abnf
% etagc = %x21 | %x23-7E | obs-text   ; VCHAR except double quotes, plus obs-text
% ```

etagc(0x21) --> [0x21].
etagc(C)    --> [C], {between(0x23, 0x7E, C)}.
etagc(C)    --> 'obs-text'(C).



%! expect(-Expectation:atom)// is det.
%
% ```abnf
% Expect = "100-continue"
% ```

http_known_known(expect).
expect('100-continue') -->
  atom_ci('100-continue').



%! 'extension-pragma'(-Ext:or([atom,pair(atom)]))// is det.
%
% ```abnf
% extension-pragma = token [ "=" ( token | quoted-string ) ]
% ```

'extension-pragma'(Ext) -->
  token(Key),
  (   "="
  ->  (token(Val) -> "" ; 'quoted-string'(Val)),
      {Ext = Key-Val}
  ;   {Ext = Key}
  ).



%! 'field-content'(:Key_3, -Val:dict)// .
%
% ```abnf
% field-content = field-vchar [ 1*( SP | HTAB ) field-vchar ]
% ```

'field-content'(Mod:Key_3, Dict) -->
  (   {current_predicate(Key_3/3)}
  ->  (   % Valid value.
          dcg_call(Mod:Key_3, Val),
          'OWS',
          % This should fail in case only /part/ of the HTTP header is parsed.
          eos
      ->  {Dict = valid_http_header{value: Val}}
      ;   % Empty value.
          phrase('obs-fold')
      ->  {Dict = empty_http_header{}}
      ;    % Buggy value.
          rest(Cs),
          {
            Dict = invalid_http_header{},
            debug(http(parse), "Buggy HTTP header ~a: ~s", [Key_3,Cs])
          }
      )
  ;   % Unknown unknown key.
      rest(Cs),
      {
        Dict = unknown_http_header{},
        (   http_known_unknown(Key_3)
        ->  true
        ;   debug(http(parse), "No parser for HTTP header ~a: ~s", [Key_3,Cs])
        )
      }
  ).



%! 'field-name'(-Name)// is det.
%
% ```abnf
% field-name = token
% ```

'field-name'(LowerA) -->
  token(A),
  {downcase_atom(A, LowerA)}.



%! 'field-vchar'(-Code:code)// is det.
%
% ```abnf
% field-vchar = VCHAR | obs-text
% ```

'field-vchar'(C) --> 'VCHAR'(C).
'field-vchar'(C) --> 'obs-text'(C).



%! 'field-value'(+Cs, +Key, -Val:dict)// is det.
%
% ```abnf
% field-value = *( field-content | 'obs-fold' )
% ```

'field-value'(Cs, Key, D2) :-
  phrase('field-content'(Key, D1), Cs),
  string_codes(Raw, Cs),
  put_dict(raw, D1, Raw, D2).



%! 'first-byte-pos'(-Position:nonneg)// is det.
%
% ```abnf
% first-byte-pos = 1*DIGIT
% ```

'first-byte-pos'(Position) -->
  +('DIGIT', Ds), !,
  {pos_sum(Ds, Position)}.



%! from(-Mailbox:dict)// is det.
%
% ```abnf
% From = mailbox
% ```

http_known_known(from).
from(D) -->
  mailbox(D).



%! 'GMT'// is det.
%
% ```abnf
% GMT = %x47.4D.54   ; "GMT", case-sensitive
% ```

'GMT' -->
  atom_ci('GMT').



%! 'header-field'(-Header:pair)// is det.
%
% ```abnf
% header-field = field-name ":" OWS field-value OWS
% ```

'header-field'(Key-D) -->
  'field-name'(Key),
  ":", 'OWS',
  rest(Cs),
  {'field-value'(Cs, Key, D)}.



%! host(-Host:dict)// is det.
%
% ```abnf
% Host = uri-host [ ":" port ] ; Section 2.7.1
% ```

http_known_known(host).
host(Auth) -->
  'uri-host'(Host),
  (":" -> port(Port) ; ""),
  {auth_comps(Auth, auth(_,Host,Port))}.



%! hour(-Hour:between(0,99))// is det.
%
% ```abnf
% hour = 2DIGIT
% ```

hour(H) -->
  #(2, 'DIGIT', Ds),
  {pos_sum(Ds, H)}.



%! 'HTTP-date'(-DT:compound)// is det.
%
% ```abnf
% HTTP-date = IMF-fixdate | obs-date
% ```

'HTTP-date'(DT) --> 'IMF-fixdate'(DT), !.
'HTTP-date'(DT) --> 'obs-date'(DT).



%! http_known(+Key) is semidet.
%! http_known(-Key) is multi.

http_known(Key) :-
  var(Key), !,
  http_known0(Key).
http_known(Key) :-
  once(http_known0(Key)).

http_known0(Key) :-
  http_known_known(Key).
% @bug Dynamic/multifile does not work.
http_known0(link).
http_known0(Key) :-
  http_known_unknown(Key).



%! http_known_unknown(+Key) is semidet.

http_known_unknown('cf-ray').
http_known_unknown('epke-alive').
http_known_unknown('es.index').
http_known_unknown('es.index_uuid').
http_known_unknown('es.resource.id').
http_known_unknown('es.resource.type').
http_known_unknown('fastly-debug-digest').
http_known_unknown('fastly-debug-path').
http_known_unknown('fastly-debug-ttl').
http_known_unknown('fastly-no-shield').
http_known_unknown('fuseki-request-id').
http_known_unknown('host-header').
http_known_unknown(p3p).
http_known_unknown('r01-domain-origin').
http_known_unknown(servidor).
http_known_unknown('source-age').
http_known_unknown(status).
http_known_unknown(tcn).
http_known_unknown(warning).
http_known_unknown('x-acre-source-url').
http_known_unknown('x-adblock-key').
http_known_unknown('x-amz-id-2').
http_known_unknown('x-amz-request-id').
http_known_unknown('x-aspnet-version').
http_known_unknown('x-backend').
http_known_unknown('x-cache').
http_known_unknown('x-cache-action').
http_known_unknown('x-cache-age').
http_known_unknown('x-cache-hits').
http_known_unknown('x-cache-info').
http_known_unknown('x-cache-lookup').
http_known_unknown('x-cache-operation').
http_known_unknown('x-cache-rule').
http_known_unknown('x-cacheable').
http_known_unknown('x-cloud-trace-context').
http_known_unknown('x-content-type-options'). % Has grammar; implemented.
http_known_unknown('x-dropbox-http-protocol').
http_known_unknown('x-dropbox-request-id').
http_known_unknown('x-drupal-cache').
http_known_unknown('x-drupal-dynamic-cache').
http_known_unknown('x-ec-custom-error').
http_known_unknown('x-fastly-request-id').
http_known_unknown('x-frame-options').
http_known_unknown('x-generator').
http_known_unknown('x-geo-block-list').
http_known_unknown('x-github-request-id').
http_known_unknown('x-goog-generation').
http_known_unknown('x-goog-hash').
http_known_unknown('x-goog-meta-uploaded-by').
http_known_unknown('x-goog-metageneration').
http_known_unknown('x-goog-storage').
http_known_unknown('x-goog-storage-class').
http_known_unknown('x-goog-stored-content-encoding').
http_known_unknown('x-goog-stored-content-length').
http_known_unknown('x-guploader-uploadid').
http_known_unknown('x-http-host').
http_known_unknown('x-hosted-by').
http_known_unknown('x-metaweb-cost').
http_known_unknown('x-metaweb-tid').
http_known_unknown('x-ms-request-id').
http_known_unknown('x-ms-version').
http_known_unknown('x-origin-route').
http_known_unknown('x-pad').
http_known_unknown('x-pal-host').
http_known_unknown('x-permitted-cross-domain-policies').
http_known_unknown('x-pingback').
http_known_unknown('x-powered-by').
http_known_unknown('x-productontology-limit').
http_known_unknown('x-productontology-offset').
http_known_unknown('x-productontology-results').
http_known_unknown('x-proxy-cache').
http_known_unknown('x-purl').
http_known_unknown('x-robots-tag'). % Has grammar.  Implemented.
http_known_unknown('x-rack-cache').
http_known_unknown('x-render-time').
http_known_unknown('x-request-count').
http_known_unknown('x-request-id').
http_known_unknown('x-response-id').
http_known_unknown('x-runtime').
http_known_unknown('x-served-by').
http_known_unknown('x-served-from-cache').
http_known_unknown('x-server').
http_known_unknown('x-sparql').
http_known_unknown('x-sparql-default-graph').
http_known_unknown('x-sparql-maxrows').
http_known_unknown('x-static-version').
http_known_unknown('x-timer').
http_known_unknown('x-total-results').
http_known_unknown('x-ua-compatible').
http_known_unknown('x-uniprot-release').
http_known_unknown('x-varnish').
http_known_unknown('x-varnish-caching-rule-id').
http_known_unknown('x-varnish-header-set-id').
http_known_unknown('x-vcap-request-id').
http_known_unknown('x-version').
http_known_unknown('x-xss-protection'). % Has grammar.  Implemented.



%! 'if-match'(-EntityTags:list(pair(atom,boolean)))// is det.
%
% ```abnf
% If-Match = "*" | 1#entity-tag
% ```

http_known_known('if-match').
'if-match'([]) -->
  "*", !.
'if-match'(EntityTags)  -->
  +#('entity-tag', EntityTags), !.



%! 'if-modified-since'(-DT:compound)// is det.
%
% ```abnf
% If-Modified-Since = HTTP-date
% ```

http_known_known('if-modified=since').
'if-modified-since'(DT) -->
  'HTTP-date'(DT).



%! 'if-none-match'(-EntityTags:list(pair(atom,boolean)))// is det.
%
% ```abnf
% If-None-Match = "*" | 1#entity-tag
% ```

http_known_known('if-none-match').
'if-none-match'([]) -->
  "*", !.
'if-none-match'(EntityTags) -->
  +#('entity-tag', EntityTags).



%! 'if-range'(-Val:compound)// is det.
%
% ```abnf
% If-Range = entity-tag | HTTP-date
% ```

http_known_known('if-range').
'if-range'(EntityTag) -->
  'entity-tag'(EntityTag), !.
'if-range'(DT) -->
  'HTTP-date'(DT).



%! 'if-unmodified-since'(-DT:compound)// is det.
%
% ```abnf
% If-Unmodified-Since = HTTP-date
% ```

http_known_known('if-unmodified-since').
'if-unmodified-since'(DT) -->
  'HTTP-date'(DT).



%! 'IMF-fixdate'(-DT:compound)// is det.
%
% ```abnf
% IMF-fixdate = day-name "," SP date1 SP time-of-day SP GMT
%             ; fixed length/zone/capitalization subset of the format
%             ; see Section 3.3 of [RFC5322]
% ```

'IMF-fixdate'(date_time(Y,Mo,D,H,Mi,S,0)) -->
  'day-name'(_DayInWeek),
  ",",
  'SP',
  date1(Y, Mo, D),
  'SP',
  'time-of-day'(H, Mi, S),
  'SP',
  'GMT'.



%! 'last-byte-pos'(-Position:nonneg)// is det.
%
% ```abnf
% last-byte-pos = 1*DIGIT
% ```

'last-byte-pos'(Position) -->
  +('DIGIT', Ds), !,
  {pos_sum(Ds, Position)}.



%! 'last-chunk'(-Exts:list)// is det.
%
% ```abnf
% last-chunk = 1*("0") [ chunk-ext ] CRLF
% ```
%
% @bug It's a mistake to make chunk-ext optional when its also Kleene star.

'last-chunk'(Exts) -->
  +("0"), !,
  'chunk-ext'(Exts),
  'CRLF'.



%! 'last-modified'(-DT:compound)// is det.
%
% ```abnf
% Last-Modified = HTTP-date
% ```

http_known_known('last-modified').
'last-modified'(DT) -->
  'HTTP-date'(DT).



%! location(-Uri:compound)// is det.
%
% ```abnf
% Location = URI-reference
% ```

http_known_known(location).
location(Uri) -->
  'URI-reference'(Uri).



%! 'max-forwards'(-Max:nonneg)// is det.
%
% ```abnf
% Max-Forwards = 1*DIGIT
% ```

http_known_known('max-forwards').
'max-forwards'(Max) -->
  +('DIGIT', Ds), !,
  {pos_sum(Ds, Max)}.



%! 'media-range'(-MT:compound)// is det.
%
%	Type and/or Subtype is a variable if the specified value is `*`.
%
% ```abnf
% media-range = ( "*/*"
%               | ( type "/" "*" )
%               | ( type "/" subtype )
%               ) *( OWS ";" OWS parameter )
% ```

'media-range'(media(Type/Subtype,Params)) -->
  ("*" -> "/*" ; type(Type), "/", ("*" -> "" ; subtype(Subtype))),
  *(sep_parameter, Params), !.



%! 'media-type'(-MT:compound)// is det.
%
% ```abnf
% media-type = type "/" subtype *( OWS ";" OWS parameter )
% ```

'media-type'(media(Type/Subtype,Params)) -->
  type(Type),
  "/",
  subtype(Subtype),
  (+(sep_parameter, Params) -> "" ; {Params = []}).

sep_parameter(Param) -->
  'OWS',
  ";",
  'OWS',
  parameter(Param).



%! 'message-body'(-Body:list(code))// is det.
%
% ```abnf
% message-body = *OCTET
% ```

'message-body'(Cs) -->
  *('OCTET', Cs), !.



%! method(-Method:atom)// is det.
%
% ```abnf
% method = token
% ```
%
% The following methods are defined by HTTP 1.1:
%   - CONNECT
%   - DELETE
%   - GET
%   - HEAD
%   - OPTIONS
%   - POST
%   - PUT
%   - TRACE

method(Method) -->
  token(Method).



%! minute(-Minute:between(0,99))// is det.
%
% ```abnf
% minute = 2DIGIT
% ```

minute(Mi) -->
  #(2, 'DIGIT', Ds),
  {pos_sum(Ds, Mi)}.



%! month(-Month:between(1,12))// is det.
%
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



%! 'obs-date'(-DT:compound)// is det.
%
% ```abnf
% obs-date = rfc850-date | asctime-date
% ```

'obs-date'(DT) -->
  'rfc850-date'(DT), !.
'obs-date'(DT) -->
  'asctime-date'(DT).



%! 'obs-fold'// is det.
%
% ```abnf
% obs-fold = CRLF 1*( SP | HTAB )   ; obsolete line folding
% ```

'obs-fold' -->
  'CRLF',
  +(sp_or_htab), !.



%! 'obs-text'(-Code:code)// is det.
%
% ```abnf
% obs-text = %x80-FF
% ```

'obs-text'(C) -->
  [C],
  {between(0x80, 0xFF, C)}.



%! 'opaque-tag'(-Tag:atom)// is det.
%
% ```abnf
% opaque-tag = DQUOTE *etagc DQUOTE
% ```

'opaque-tag'(Tag) -->
  'DQUOTE',
  *(etagc, Cs), !,
  'DQUOTE',
  {atom_codes(Tag, Cs)}.



%! 'other-content-range'(-Unit:atom, -Rest:atom)// is det.
%
% ```abnf
% other-content-range = other-range-unit SP other-range-resp
% ```

'other-content-range'(Unit, Resp) -->
  'other-range-unit'(Unit),
  'SP',
  'other-range-resp'(Resp).



%! 'other-range-resp'(-Resp:atom)// is det.
%
% ```abnf
% other-range-resp = *CHAR
% ```

'other-range-resp'(Resp) -->
  *('CHAR', Cs), !,
  {atom_codes(Resp, Cs)}.



%! 'other-range-set'(-Set:atom)// is det.
%
% ```abnf
% other-range-set = 1*VCHAR
% ```

'other-range-set'(Set) -->
  +('VCHAR', Cs), !,
  {atom_codes(Set, Cs)}.



%! 'other-range-unit'(-Unit:atom)// is det.
%
% ```abnf
% other-range-unit = token
% ```

'other-range-unit'(Unit) -->
  token(Unit).



%! 'other-ranges-specifier'(-Unit:atom, -Set:atom)// is det.
%
% ```abnf
% other-ranges-specifier = other-range-unit "=" other-range-set
% ```

'other-ranges-specifier'(Unit, Set) -->
  'other-range-unit'(Unit),
  "=",
  'other-range-set'(Set).



%! 'OWS'// is det.
%
% ```abnf
% OWS = *( SP | HTAB )   ; optional whitespace
% ```

'OWS' -->
  *(sp_or_htab), !.



%! parameter(-Pair:pair(atom))// is det.
%
% ```abnf
% parameter = token "=" ( token | quoted-string )
% ```

parameter(Key-Val) -->
  token(Key),
  "=",
  (token(Val), ! ; 'quoted-string'(Val)).



%! 'partial-URI'(-Uri:atom)// is det.
%
% ```abnf
% partial-URI = relative-part [ "?" query ]
% ```

'partial-URI'(Uri) -->
  'relative-part'(Auth, Segments),
  ("?" -> query(Query) ; ""),
  {uri_comps(Uri, uri(_,Auth,Segments,Query,_))}.



%! pragma(?Pragmas:list(or([atom,pair(atom)])))// is det.
%
% ```abnf
% Pragma = 1#pragma-directive
% ```

http_known_known(pragma).
pragma(Pragmas) -->
  +#('pragma-directive', Pragmas).



%! 'pragma-directive'(-Pragma:or([atom,pair(atom)]))// is det.
%
% ```abnf
% pragma-directive = "no-cache" | extension-pragma
% ```

'pragma-directive'('no-cache') -->
  atom_ci('no-cache'), !.
'pragma-directive'(Pragma) -->
  'extension-pragma'(Pragma).



%! product(-Product:pair(atom))// is det.
%
% ```abnf
% product = token ["/" product-version]
% ```

product(Name-Version) -->
  token(Name),
  ("/" -> 'product-version'(Version) ; "").



%! 'product-version'(-Version:atom)// is det.
%
% ```abnf
% product-version = token
% ```

'product-version'(Version) -->
  token(Version).



%! 'proxy-authenticate'(-Challenges:list(pair(atom,list(pair(atom)))))// is det.
%
% ```abnf
% Proxy-Authenticate = 1#challenge
% ```

http_known_known('proxy-authenticate').
'proxy-authenticate'(Challenges) -->
  +#(challenge, Challenges).



%! 'proxy-authorization'(-Credentials:pair(atom,list(pair(atom))))// is det.
%
% ```abnf
% Proxy-Authorization = credentials
% ```

http_known_known('proxy-authorization').
'proxy-authorization'(Credentials) -->
  credentials(Credentials).



%! protocol(-Protocol:or([atom,pair(atom)]))// is det.
%
% ```abnf
% protocol = protocol-name ["/" protocol-version]
% ```

protocol(Protocol) -->
  'protocol-name'(Name),
  (   "/"
  ->  'protocol-version'(Version),
      {Protocol = Name-Version}
  ;   {Protocol = Name}
  ).



%! 'protocol-name'(-Name:atom)// is det.
%
% ```abnf
% protocol-name = token
% ```

'protocol-name'(Name) -->
  token(Name).



%! 'protocol-version'(-Version:atom)// is det.
%
% ```abnf
% protocol-version = token
% ```

'protocol-version'(Version) -->
  token(Version).



%! pseudonym(-Pseudonym:atom)// is det.
%
% ```abnf
% pseudonym = token
% ```

pseudonym(Pseudonym) -->
  token(Pseudonym).



%! qdtext(-Code:code)// is det.
%
% ```abnf
% qdtext = HTAB | SP | %x21 | %x23-5B | %x5D-7E | obs-text
% ```

qdtext(C)    --> 'HTAB'(C).
qdtext(C)    --> 'SP'(C).
qdtext(0x21) --> [0x21].
qdtext(C)    --> [C], {(between(0x23, 0x5B, C), ! ; between(0x5D, 0x7E, C))}.
qdtext(C)    --> 'obs-text'(C).



%! 'quoted-pair'(-Code:code)// is det.
%
% ```abnf
% quoted-pair = "\" ( HTAB | SP | VCHAR | obs-text )
% ```

'quoted-pair'(C) -->
  "\\",
  ('HTAB'(C) ; 'SP'(C) ; 'VCHAR'(C) ; 'obs-text'(C)).



%! 'quoted-string'(-String:atom)// is det.
%
% ```abnf
% quoted-string = DQUOTE *( qdtext | quoted-pair ) DQUOTE
% ```

'quoted-string'(String) -->
  'DQUOTE',
  *(quoted_string_code, Cs), !,
  'DQUOTE',
  {atom_codes(String, Cs)}.

quoted_string_code(C) --> qdtext(C).
quoted_string_code(C) --> 'quoted-pair'(C).



%! qvalue(-Val:between(0.0,1.0))// is det.
%
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



%! range(-Unit:atom, -Range:pair(nonneg))// is det.
%
% ```abnf
% Range = byte-ranges-specifier | other-ranges-specifier
% ```

http_known_known(range).
range(byte, Range) -->
  'byte-ranges-specifier'(Range).
range(Unit, Set) -->
  'other-ranges-specifier'(Unit, Set).



%! 'range-unit'(-Unit:atom)// is det.
%
% ```abnf
% range-unit = bytes-unit | other-range-unit
% ```

'range-unit'(bytes) -->
  'bytes-unit', !.
'range-unit'(Unit) -->
  'other-range-unit'(Unit).



%! rank(-Rank:between(0.0,1.0))// is det.
%
% ```abnf
% rank = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ```

rank(N)   -->
  "0",
  (   "."
  ->  'm*n'(0, 3, 'DIGIT', Ds),
      {
        pos_frac(Ds, N0),
        N is float(N0)
      }
  ;   {N = 0}
  ).
rank(1.0) -->
  "1",
  ("." -> 'm*n'(0, 3, "0") ; "").



%! 'received-by'(-Receiver:atom)// is det.
%
% ```abnf
% received-by = ( uri-host [ ":" port ] ) | pseudonym
% ```

'received-by'(Auth) -->
  'uri-host'(Host), !,
  (":" -> port(Port) ; {Port = 80}),
  {auth_comps(Auth, auth(_,Host,Port))}.
'received-by'(Pseudonym) -->
  pseudonym(Pseudonym).



%! 'received-protocol'(-Protocol:or([atom,pair(atom)]))// is det.
%
% ```abnf
% received-protocol = [ protocol-name "/" ] protocol-version
% ```

'received-protocol'(Name-Version) -->
  'protocol-name'(Name), !,
  "/",
  'protocol-version'(Version).
'received-protocol'(Version) -->
  'protocol-version'(Version).



%! referer(-Uri:compound)// is det.
%
% ```abnf
% Referer = absolute-URI | partial-URI
% ```

http_known_known(referer).
referer(Uri) -->
  'absolute-URI'(Uri).
referer(Uri) -->
  'partial-URI'(Uri).



%! 'retry-after'(-DT:compound)// is det.
%
% ```abnf
% Retry-After = HTTP-date | delay-seconds
% ```

http_known_known('retry-after').
'retry-after'(DT) -->
  'HTTP-date'(DT).
'retry-after'(D) -->
  'delay-seconds'(D).



%! 'rfc850-date'(-DT:compound)// is det.
%
% ```abnf
% rfc850-date  = day-name-l "," SP date2 SP time-of-day SP GMT
% ```

'rfc850-date'(date_time(Y,Mo,D,H,Mi,S,0)) -->
  'day-name-l'(D),
  ",",
  'SP',
  date2(Y, Mo, D),
  'SP',
  'time-of-day'(H, Mi, S),
  'SP',
  'GMT'.



%! 'RWS'// is det.
%
% ```abnf
% RWS = 1*( SP | HTAB )   ; required whitespace
% ```

'RWS' -->
  +(sp_or_htab), !.



%! second(-Second:between(0,99))// is det.
%
% ```abnf
% second = 2DIGIT
% ```
%
% @tbd Define an XSD for the range [0,99].

second(N) -->
  #(2, 'DIGIT', Ds),
  {pos_sum(Ds, N)}.



%! server(-Server:list(atom))// is det.
%
% ```abnf
% Server = product *( RWS ( product | comment ) )
% ```

http_known_known(server).
server([H|T]) -->
  product(H),
  *(sep_product_or_comment, T), !.



%! subtype(-Subtype:atom)// is det.
%
% ```abnf
% subtype = token
% ```

subtype(Subtype) -->
  token(Subtype).



%! 'suffix-byte-range-spec'(-Len:nonneg)// is det.
%
% ```abnf
% suffix-byte-range-spec = "-" suffix-length
% ```

'suffix-byte-range-spec'(Len) -->
  "-",
  'suffix-length'(Len).



%! 'suffix-length'(-Len)// is det.
%
% ```abnf
% suffix-length = 1*DIGIT
% ```

'suffix-length'(Len) -->
  +('DIGIT', Ds),
  {pos_sum(Ds, Len)}.



%! 't-codings'(-TCodings)// is det.
%
% ```abnf
% t-codings = "trailers" | ( transfer-coding [ t-ranking ] )
% ```

't-codings'(trailers) -->
  atom_ci(trailers).
't-codings'(D2) -->
  'transfer-coding'(TransferCoding),
  {D1 = tcoding{transfer_coding: TransferCoding}},
  ('t-ranking'(Rank) -> {put_dict('t-ranking', D1, Rank, D2)} ; {D2 = D1}).



%! 't-ranking'(-Rank:float)// is det.
%
% ```abnf
% t-ranking = OWS ";" OWS "q=" rank
% ```
%
% @tbd Define an XSD for the range [0.0,1.0].

't-ranking'(Rank) -->
  'OWS',
  ";",
  'OWS',
  atom_ci('q='),
  rank(Rank).



%! tchar(?C)// is det.
%
% ```abnf
% tchar = "!" | "#" | "$" | "%" | "&" | "'" | "*"
%       | "+" | "-" | "." | "^" | "_" | "`" | "|" | "~"
%       | DIGIT | ALPHA   ; any VCHAR, except delimiters
% ```

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
%
% ```abnf
% TE = #t-codings
% ```

http_known_known(te).
te(L) -->
  '*#'('t-codings', L).



%! 'time-of-day'(-H, -Mi, -S)// is det.
%
% ```abnf
% time-of-day = hour ":" minute ":" second
%             ; 00:00:00 - 23:59:60 (leap second)
% ```

'time-of-day'(H, Mi, S) -->
  hour(H),
  ":",
  minute(Mi),
  ":",
  second(S).



%! token(-Token)// is det.
%
% ```abnf
% token = 1*tchar
% ```

token(A) -->
  +(tchar, Cs), !,
  {atom_codes(A, Cs)}.



%! token68(-Token:atom)// is det.
%
% ```abnf
% token68 = 1*( ALPHA | DIGIT | "-" | "." | "_" | "~" | "+" | "/" ) *"="
% ```

token68(Token) -->
  +(token68_code, Cs), !,
  {atom_codes(Token, Cs)},
  *("="), !.

token68_code(C)   --> 'ALPHA'(C).
token68_code(C)   --> 'DIGIT'(_, C).
token68_code(0'-) --> "-".
token68_code(0'.) --> ".".
token68_code(0'_) --> "_".
token68_code(0'~) --> "~".
token68_code(0'+) --> "+".
token68_code(0'/) --> "/".



%! trailer(-Fields:list(string))// is det.
%
% ```abnf
% Trailer = 1#field-name
% ```

http_known_known(trailer).
trailer(L) -->
  +#('field-name', L), !.



%! 'transfer-coding'(-TransferCoding)// is det.
%
% ```abnf
% transfer-coding = "chunked"
%                 | "compress"
%                 | "deflate"
%                 | "gzip"
%                 | transfer-extension
% ```

'transfer-coding'(chunked)  --> atom_ci(chunked), !.
'transfer-coding'(compress) --> atom_ci(compress), !.
'transfer-coding'(deflate)  --> atom_ci(deflate), !.
'transfer-coding'(gzip)     --> atom_ci(gzip), !.
'transfer-coding'(D)        --> 'transfer-extension'(D).



%! 'transfer-encoding'(-TransferCodings:list(dict))// is det.
%
% ```abnf
% Transfer-Encoding = 1#transfer-coding
% ```

http_known_known('transfer-encoding').
'transfer-encoding'(L) -->
  +#('transfer-coding', L).



%! 'transfer-extension'(-TransferExt:dict)// is det.
%
% ```abnf
% transfer-extension = token *( OWS ";" OWS transfer-parameter )
% ```

'transfer-extension'('transfer-extension'{token: H, params: Params}) -->
  token(H),
  *(sep_transfer_parameter, Pairs),
  {dict_pairs(Params, Pairs)}.

sep_transfer_parameter(Pair) -->
  'OWS',
  ";",
  'OWS',
  'transfer-parameter'(Pair).



%! 'transfer-parameter'(-Param:pair)// is det.
%
% ```abnf
% transfer-parameter = token BWS "=" BWS ( token | quoted-string )
% ```

'transfer-parameter'(Key-Val) -->
  token(Key),
  'BWS',
  "=",
  'BWS',
  (token(Val), ! ; 'quoted-string'(Val)).



%! type(-Type)// .
%
% ```abnf
% type = token
% ```

type(A) -->
  token(A).



%! 'unsatisfied-range'(-Range:string, -Len)// is det.
%
% ```abnf
% unsatisfied-range = "*/" complete-length
% ```

'unsatisfied-range'(_-_, Len) -->
  "*/",
  'complete-length'(Len).



%! upgrade(-Protocols:list(dict))// is det.
%
% ```abnf
% upgrade = 1#protocol
% ```

upgrade(L) -->
  +#(protocol, L).



%! 'user-agent'(-UserAgent:list)// is det.
%
% ```abnf
% User-Agent = product *( RWS ( product | comment ) )
% ```

http_known_known('user-agent').
'user-agent'([H|T]) -->
  product(H),
  *(sep_product_or_comment, T).



%! vary(-FieldNames:list(dict))// is det.
%
% ```abnf
% Vary = "*" | 1#field-name
% ```

http_known_known(vary).
vary([]) -->
  "*", !.
vary(L) -->
  +#('field-name', L).



%! via(-Vias:list(compound))// is det.
%
% ```abnf
% Via = 1#( received-protocol RWS received-by [ RWS comment ] )
% ```

http_known_known(via).
via(Vias) -->
  '+#'(via_component, Vias), !.

via_component(via(ReceivedProtocol,ReceivedBy,Comment)) -->
  'received-protocol'(ReceivedProtocol),
  'RWS',
  'received-by'(ReceivedBy),
  ('RWS' -> comment(Comment) ; "").



%! 'warn-agent'(-Agent:atom)// is det.
%
% ```abnf
% warn-agent = ( uri-host [ ":" port ] ) | pseudonym
%            ; the name or pseudonym of the server adding
%            ; the Warning header field, for use in debugging
%            ; a single "-" is recommended when agent unknown
% ```

'warn-agent'(Auth) -->
  'uri-host'(Host),
  (":" -> port(Port) ; ""), !,
  {auth_comps(Auth, auth(_,Host,Port))}.
'warn-agent'(Pseudonym) -->
  pseudonym(Pseudonym).



%! 'warn-code'(-Code:between(0,999))// is det.
%
% ```abnf
% warn-code = 3DIGIT
% ```

'warn-code'(Code) -->
  #(3, 'DIGIT', Ds),
  {pos_sum(Ds, Code)}.



%! 'warn-date'(-DT:compound)// is det.
%
% ```abnf
% warn-date = DQUOTE HTTP-date DQUOTE
% ```

'warn-date'(DT) -->
  'DQUOTE',
  'HTTP-date'(DT),
  'DQUOTE'.



%! 'warn-text'(-Text:atom)// is det.
%
% ```abnf
% warn-text = quoted-string
% ```

'warn-text'(Text) -->
  'quoted-string'(Text).



%! warning(-Warnings:list(compound))// is det.
%
% ```abnf
% warning = 1#warning-value
% ```

warning(Warnings) -->
  +#('warning-value', Warnings).



%! 'warning-value'(-Warning:compound)// is det.
%
% ```abnf
% warning-value = warn-code SP warn-agent SP warn-text [ SP warn-date ]
% ```

'warning-value'(warning(Code,Agent,Text,DT)) -->
  'warn-code'(Code),
  'SP',
  'warn-agent'(Agent),
  'SP',
  'warn-text'(Text),
  ('SP' -> 'warn-date'(DT) ; "").



%! weak// is det.
%
% ```abnf
% weak = %x57.2F   ; "W/", case-sensitive
% ```

weak -->
  "W/".



%! weight(-Weight:between(0.0,1.0))// is det.
%
% ```abnf
% weight = OWS ";" OWS "q=" qvalue
% ```

weight(N) -->
  'OWS',
  ";",
  'OWS',
  atom_ci('q='),
  qvalue(N).



%! 'www-authenticate'(-Challenges:list(compound))// is det.
%
% ```abnf
% WWW-Authenticate = 1#challenge
% ```

http_known_known('www-authenticate').
'www-authenticate'(Challenges) -->
  +#(challenge, Challenges).



%! year(-Year:between(0,9999))// is det.
%
% ```
% year = 4DIGIT
% ```

year(Y) -->
  #(4, 'DIGIT', Ds),
  {pos_sum(Ds, Y)}.





% HELPERS %

%! header_field_eol(-Header)// is det.

header_field_eol(Header) -->
  'header-field'(Header),
  'CRLF'.



%! optional_weight(-Weight:between(0.0,1.0))// is det.

optional_weight(Weight) -->
  weight(Weight), !.
optional_weight(1.0).



%! sep_product_or_comment(-A)// is det.

sep_product_or_comment(A) -->
  'RWS',
  (product(A), ! ; comment(A)).



%! sp_or_htab// is det.

sp_or_htab --> 'SP'.
sp_or_htab --> 'HTAB'.
