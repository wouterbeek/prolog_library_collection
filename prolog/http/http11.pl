:- module(
  http11,
  [
  ]
).

/** <module> HTTP 1.1: Hypertext Transfer Protocol (HTTP/1.1)

@author Wouter Beek
@compat RFC 7230
@version 2015/11-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
    'ALPHA'//1, % ?Code:code
    'CR'//0,
    'CRLF'//0,
    'CTL'//0,
    'CTL'//1, % ?Code:code
    'DIGIT'//1, % ?Weight:nonneg
    'DQUOTE'//0 as '"',
    'HEXDIG'//1 as 'HEX', % ?Weight:nonneg
    'HTAB'//0 as 'HT',
    'HTAB'//1 as 'HT', % ?Code:code
    'LF'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1 % ?Code:code
    'VCHAR'//1 % ?Code:code
  ]).
:- use_module(library(ltag/rfc5646), [
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

accept(L) --> #(accept_part, L).
accept_part(MediaRange-Params) -->
  'media-range'(MediaRange),
  ?('accept-params', Params).



%! 'accept-charset'(-Charactersets:list(dict))// is det.
% ```abnf
% Accept-Charset = 1#( ( charset / "*" ) [ weight ] )
% ```

'accept-charset'(L) --> +#(accept_charset_part, L).
accept_charset_part(X) -->
  (charset(Charset) -> "" ; "*", {Charset = "*"}),
  (weight(Weight) -> {X = Weight-Charset} ; {X = Weight}).



%! 'accept-encoding'(-Codings:list)// is det.
% ```abnf
% Accept-Encoding = #( codings [ weight ] )
% ```

'accept-encoding'(L) --> #(accept_encoding_part, L).
accept_encoding_part(X) -->
  codings(Coding),
  (weight(Weight) -> {X = Weight-Coding} ; {X = Coding}).



%! 'accept-ext'(-Extension)// is det.
% ```abnf
% accept-ext = OWS ";" OWS token [ "=" ( token / quoted-string ) ]
% ```

'accept-ext'(Ext) -->
  'OWS', ";", 'OWS',
  token(N),
  ("=" -> (token(V), ! ; 'quoted-string'(V)), {Ext = N-V} ; {Ext = N}).



%! 'accept-language'(-LanguageRanges:list)// is det.
% ```abnf
% Accept-Language = 1#( language-range [ weight ] )
% ```

'accept-language' --> 1#(accept_language_part, L).
accept_language_part(X) -->
  'language-range'(LRange),
  (weight(Weight) -> {X = Weight-LRange} ; {X = LRange}).



%! 'accept-params'(-Pair:pair(between(0.0,1.0),list))// is det.
% ```abnf
% accept-params = weight *( accept-ext )
% ```

'accept-params'(Weight-Exts) --> weight(Weight), *('accept-ext', Exts).



%! 'asterisk-form'// is det.
% ```abnf
% asterisk-form = "*"
% ```

'asterisk-form' --> "*".



%! 'authority-form'(-Authority:dict)// is det.
% ```abnf
% authority-form = authority
% ```

'authority-form'(D) --> authority(D).



%! 'BWS'// is det.
% ```abnf
% BWS = OWS   ; "bad" whitespace
% ```

'BWS' --> 'OWS'.



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
% codings = content-coding / "identity" / "*"
% ```

codings(S) --> 'content-coding'(S).
codings(identity) --> "identity".
codings(*) --> "*".



%! comment// .
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment --> "(", *(comment_code), ")".
comment_code --> ctext(_).
comment_code --> 'quoted-pair'(_).
comment_code --> comment.



%! 'connection'(-Options:list(string))// .
% ```abnf
% 'Connection'(S) --> 1#(connection-option)
% ```

'connection'(L) --> +#('connection-option', L).



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
% Content-Location = absolute-URI | relative-URI
% ```

'content-location'(Uri) --> 'absolute-URI'(Uri), !.
'content-location'(Uri) --> 'relative-URI'(Uri).



%! 'content-type'(-MediaType:dict)// .
% ```abnf
% Content-Type = media-type
% ```

'content-type'(MT) --> 'media-type'(MT).



%! ctext(?Code:code)// is det.
% ```abnf
% ctext = HTAB / SP / %x21-27 / %x2A-5B / %x5D-7E / obs-text
% ```

ctext(C) --> 'HTAB'(C).
ctext(C) --> 'SP'(C).
ctext(C) --> [C], {(between(0x21, 0x27, C), ! ; between(0x2A, 0x5B, C), ! ; between(0x5D, 0x7E, C))}.
ctext(C) --> 'obs-text'(C).



%! 'entity-header'(-Header:pair)// is det.
% ```abnf
% entity-header = Allow
%               | Content-Encoding
%               | Content-Language
%               | Content-Length
%               | Content-Location
%               | Content-MD5
%               | Content-Range
%               | Content-Type
%               | Expires
%               | Last-Modified
%               | extension-header
% ```

'entity-header'(allow-V) --> allow(V).
'entity-header'('content-encoding'-V) --> 'content-encoding'(V).
'entity-header'('content-language'-V) --> 'content-language'(V).
'entity-header'('content-length'-V) --> 'content-length'(V).
'entity-header'('content-location'-V) --> 'content-location'(V).
'entity-header'('content-md5'-V) --> 'content-md5'(V).
'entity-header'('content-range'-V) --> 'content-range'(V).
'entity-header'('content-type'-V) --> 'vontent-type'(V).
'entity-header'(expires-V) --> expires(V).
'entity-header'('last-modified'-V) --> 'last-modified'(V).
'entity-header'('extension-header'-V) --> 'extension-header'(V).



%! expect(-Expectation:string)// is det.
% ```abnf
% Expect = "100-continue"
% ```

expect("100-continue") --> "100-continue".



%! 'field-content'(+Name:atom, -Value:dict)// .
% ```abnf
% field-content = field-vchar [ 1*( SP / HTAB ) field-vchar ]
% ```

'field-content'(ModPred, 'field-content'{status: Status, value: Value}) -->
  {strip_module(ModPred, _, Pred)},
  (   {current_predicate(Pred/3)}
  ->  (   dcg_call(ModPred, Value),
          % This should fail in case only /part/ of the HTTP header is parsed.
          eos
      ->  {Status = valid}
      ;   dcg_rest(Cs),
          {
            Status = invalid,
            string_codes(Value, Cs),
            debug(http(parse), "Buggy HTTP header ~a: ~a", [Pred,Value])
          }
      )
  ;   dcg_rest(Cs),
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
known_unknown(servidor).
known_unknown('x-acre-source-url').
known_unknown('x-cache').
known_unknown('x-cache-lookup').
known_unknown('x-content-type-options'). % Had grammar.  Implemented.
known_unknown('x-drupal-cache').
known_unknown('x-metaweb-cost').
known_unknown('x-metaweb-tid').
known_unknown('x-pingback').
known_unknown('x-powered-by').
known_unknown('x-robots-tag'). % Has grammar.  Implemented.
known_unknown('x-rack-cache').
known_unknown('x-request-id').
known_unknown('x-response-id').
known_unknown('x-runtime').
known_unknown('x-served-from-cache').
known_unknown('x-sparql').
known_unknown('x-ua-compatible').
known_unknown('x-varnish').
known_unknown('x-xss-protection'). % Has grammar.  Implemented.



%! 'field-name'(-Name:string)// .
% ```abnf
% field-name = token
% ```

'field-name'(LowerS) --> token(S), {string_lower(S, LowerS)}.



%! 'field-value'(+Name:atom, -Value:compound)// .
% ```abnf
% field-value = *( field-content | 'obs-fold' )
% ```

'field-value'(Name, Value) --> 'field-content'(Name, Value).
'field-value' --> 'obs-fold'.



%! 'field-vchar'(-Code:code)// is det.
% ```abnf
% field-vchar = VCHAR / obs-text
% ```

'field-vchar'(C) --> 'VCHAR'(C).
'field-vchar'(C) --> 'obs-text'(C).



%! from(-Mailbox)// is det.
% ```abnf
% From = mailbox
% ```

from --> mailbox.



%! 'generic-message'(
%!   ?Version:pair(nonneg),
%!   ?Status:between(100,599),
%!   ?Headers:list(pair(string,compound)),
%!   ?Body:list(code)
%! )// .
% ```abnf
% generic-message = start-line
%                   *(message-header CRLF)
%                   CRLF
%                   [ message-body ]
% ```

'generic-message'(Version, Status, Headers, Body) -->
  'start-line'(Version, Status),
  meassage_headers(Headers),
  'CRLF',
  ('message-body'(Body) ; {Body = []}).



%! 'header-field'(-Header:pair(string))// is det.
% ```abnf
% header-field = field-name ":" OWS field-value OWS
% ```

'header-field'(N-V) --> 'field-name'(N), ":", 'OWS', 'field-value'(V), 'OWS'.



%! host(-Host:dict)// is det.
% ```abnf
% Host = uri-host [ ":" port ] ; Section 2.7.1
% ```

host(D) -->
  'uri-host'(D0),
  (":" -> port(Port), {D = D0.put(port, Port)} ; {D = D0}).



%! 'http-URI'(-HttpUri:dict)// is det.
% ```abnf
% http-URI = "http:" "//" authority path-abempty [ "?" query ] [ "#" fragment ]
% ```

'http-URI' -->
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



%! 'HTTP-message'// .
% ```abnf
% HTTP-message = start-line *( header-field CRLF ) CRLF [ message-body ]
% ```

'HTTP-message'(http_msg{body: Cs, headers: L}) -->
  'start-line',
  *(header_field_eol, L),
  'CRLF',
  def('message-body', Cs, []).
header_field_eol(X) --> 'header-field'(X), 'CRLF'.



%! 'HTTP-name'// is det.
% ```abnf
% HTTP-name = %x48.54.54.50   ; "HTTP", case-sensitive
% ```

'HTTP-name' --> [0x48,0x54,0x54,0x50].



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
  "https://",
  authority(Auth),
  'path-abempty'(Path),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Frag) ; ""),
  {
    L1 = [authority-Auth,fragment-Frag,path-Path,query-Query],
    exclude(pair_has_var_value, L1, L2),
    dict_pairs(D, http_uri, L2)
  }.



%! 'last-chunk'(-Extensions:list)// is det.
% ```abnf
% last-chunk = 1*("0") [ chunk-ext ] CRLF
% ```
%
% @bug It's a mistake to make chunk-ext optional when its also Kleene star.

'last-chunk'(Exts) --> +("0"), 'chunk-ext'(Exts), 'CRLF'.



%! 'max-forwards'(-Max:nonneg)// is det.
% ```abnf
% Max-Forwards = 1*DIGIT
% ```

'max-forwards'(N) --> +('DIGIT', Ds), {pos_num(Ds, N)}.



%! 'media-range'(-MediaRange:dict)// is det.
% ```abnf
% media-range = ( "*/*"
%               / ( type "/" "*" )
%               / ( type "/" subtype )
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



%! 'obs-fold'// is det.
% ```abnf
% obs-fold = CRLF 1*( SP / HTAB ) ; obsolete line folding
% ```

'obs-fold' --> 'CRLF', +(sp_or_htab).



%! 'obs-text'(-Code:code)// is det.
% ```abnf
% obs-text = %x80-FF
% ```

'obs-text'(C) --> [C], {between(0x80, 0xFF, C)}.



%! 'origin-form'(-Uri:dict)// is det.
% ```abnf
% origin-form = absolute-path [ "?" query ]
% ```

'origin-form'(D) -->
  'absolute-path'(D0),
  ("?" -> query(Query), {D = D0.put(query, Query)} ; {D = D0}).



%! 'OWS'// is det.
% ```abnf
% OWS = *( SP / HTAB )   ; optional whitespace
% ```

'OWS' --> *(sp_or_htab).



%! parameter(?Parameter:pair(string))// .
% ```abnf
% parameter = token "=" ( token / quoted-string )
% ```

parameter(N-V) --> token(N), "=", (token(V), ! ; 'quoted-string'(V)).



%! 'partial-URI'// is det.
% ```abnf
% partial-URI = relative-part [ "?" query ]
% ```

'partial-URI' -->
  'relative-part'(D0),
  {dict_pairs(D0, relative_part, T)},
  ("?" -> query(Query) ; ""),
  {
    exclude(pair_has_var_value, [query-Query|T], L),
    dict_pair(D, partial_uri, L)
  }.



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
% qdtext = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text
% ```

qdtext(C)    --> 'HTAB'(C).
qdtext(C)    --> 'SP'(C).
qdtext(0x21) --> [0x21].
qdtext(C)    --> [C], {(between(0x23, 0x5B, C), ! ; between(0x5D, 0x7E, C))}.
qdtext(C)    --> 'obs-text'(C).
				  


%! 'quoted-pair'(-Code:code)// is det.
% ```abnf
% quoted-pair = "\" ( HTAB / SP / VCHAR / obs-text )
% ```

'quoted-pair'(C) --> "\\", quoted_pair_code(C).
quoted_pair_code(C) --> 'HTAB'(C).
quoted_pair_code(C) --> 'SP'(C).
quoted_pair_code(C) --> 'VCHAR'(C).
quoted_pair_code(C) --> 'obs-text'(C).



%! 'quoted-string'(?String:string)// .
% ```abnf
% quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE
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
% qvalue = ( "0" [ "." 0*3DIGIT ] ) / ( "1" [ "." 0*3("0") ] )
% ```

qvalue(N)
  "0",
  (   "."
  ->  'm*n'(0, 3, 'DIGIT', Ds),
      {pos_frac(Ds, N0), N is float(N0)}
  ;   {N = 0}
  ).
qvalue(1.0) --> "1", ("." -> 'm*n'(0, 3, "0") ; "").



%! rank(-Rank:between(0.0,1.0))// is det.
% ```abnf
% rank = ( "0" [ "." 0*3DIGIT ] ) / ( "1" [ "." 0*3("0") ] )
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
% reason-phrase = *( HTAB / SP / VCHAR / obs-text )
% ```

'reason-phrase' --> *(reason_phrase_code, Cs), {string_codes(S, Cs)}.
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



%! 'Request'// .
% ```abnf
% Request = Request-Line
%           *(( general-header
%             | request-header
%             | entity-header )
%             CRLF )
%            CRLF
%            [ message-body ]
% ```

'Request' -->
  'Request-Line',
  *(header_eol, L),
  'CRLF',
  def('message-body', Cs, []).
header_eol(X) --> 'general-header'(X).
header_eol(X) --> 'request-header'(X).
header_oel(X) --> 'entity-header'(X).



%! 'request-line'// .
% ```abnf
% request-line = method SP request-target SP HTTP-version CRLF
% ```

'request-line'(
  request_line{method: Method, request_target: Iri, version: Version}
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
%                / absolute-form
%                / authority-form
%                / asterisk-form
% ```

'request-target'(D) --> 'origin-form'(D).
'request-target'(D) --> 'absolute-form'(D).
'request-target'(D) --> 'authority-form'(D).
'request-target'(*) --> 'asterisk-form'.



%! 'RWS'// is det.
% ```abnf
% RWS = 1*( SP / HTAB )   ; required whitespace
% ```

'RWS' --> +(sp_or_htab).



%! 'start-line'(-Line:dict)// is det.
% ```abnf
% start-line = request-line | status-line
% ```

'start-line'(X) --> 'request-line'(X), !.
'start-line'(X) --> 'status-line'(X).



%! 'status-code'(-Status:between(0,999))// is det.
% ```abnf
% status-code = 3DIGIT
% ```

'status-code'(N) --> #(3, 'DIGIT', Ds), {pos_sum(Ds, N)}.



%! 'status-line'// is det.
% ```abnf
% status-line = HTTP-version SP status-code SP reason-phrase CRLF
% ```

'status-line'(
  status_line{reason: Reason, status: Status, version: Version}
) -->
  'HTTP-version'(Version),
  'SP',
  'status-code'(Status),
  'SP',
  'reason-phrase'(Reason),
  'CRLF'.



%! subtype// .
% ```abnf
% subtype = token
% ```

subtype(S) --> token(S).



%! 't-codings'// is det.
% ```abnf
% t-codings = "trailers" / ( transfer-coding [ t-ranking ] )
% ```

't-codings'(trailers) --> "trailers".
't-codings'(X) -->
  'transfer-coding'(TransferCoding),
  ('t-ranking'(Rank) -> {X = TransferCoding-Rank} ; {X = TransferCoding}).



%! 't-ranking'(-Rank:between(0.0,1.0))// is det.
% ```abnf
% t-ranking = OWS ";" OWS "q=" rank
% ```

't-ranking'(Rank) --> 'OWS', ";", 'OWS', "q=", rank(Rank).



%! tchar(-Code:code)// is det.
% ```abnf
% tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*"
%       / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
%      / DIGIT / ALPHA   ; any VCHAR, except delimiters


tchar(C)   --> 'ALPHA'(C).
tchar(C)   --> 'DIGIT'(_, C).
tchar(0'!) --> "!".
tchar(0'#) --> "#".
tchar(0'$) --> "$".
tchar(0'%) --> "\%".
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

te(L) --> *#('t-codings', L).



%! token(?Token:string)// .
% ```abnf
% token = 1*tchar
% ```

token(S) --> +(tchar, Cs), {string_codes(S, Cs)}.



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
header_field_eol(Header) --> 'header-field'(Header), 'CRLF'.



%! 'transfer-coding'(?TransferCoding:dict)// is det.
% ```abnf
% transfer-coding = "chunked"
%                 / "compress"
%                 / "deflate"
%                 / "gzip"
%                 / transfer-extension
% ```

'transfer-coding'(transfer_coding{token: chunked, parameter: []}) -->
  "chunked", !.
'transfer-coding'(transfer_coding{token: compress, parameter: []}) -->
  "compress", !.
'transfer-coding'(transfer_coding{token: deflate, paramers: []}) -->
  "deflate", !.
'transfer-coding'(transfer_coding{token: gzip, parameters: []}) -->
  "gzip", !.
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
% transfer-parameter = token BWS "=" BWS ( token / quoted-string )
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



%! upgrade// is det.
% ```abnf
% upgrade = 1#protocol
% ```

upgrade(L) --> +#(protocol, L).



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



%! weight(-Weight:between(0.0,1.0))// is det.
% ```abnf
% weight = OWS ";" OWS "q=" qvalue
% ```

weight(N) --> 'OWS', ";", 'OWS', "q=", qvalue(N).




			   
% HELPERS %

sp_or_htab --> 'SP'.
sp_or_htab --> 'HTAB'.
