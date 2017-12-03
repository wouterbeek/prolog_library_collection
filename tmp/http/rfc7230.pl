:- module(
  rfc7230,
  [
    '+##'//1,              % :Dcg_0
    '+##'//2,              % :Dcg_1, ?Arguments
    '*##'//1,              % :Dcg_0
    '*##'//2,              % :Dcg_1, ?Arguments
    'BWS'//0,
    comment//1,            % -Comment
    'field-name'//1,       % ?Name
    'HTTP-message'//1,     % ?Message
    http_sep//0,
    http_sep//1,           % ?Code
    method//1,             % ?Method
    'obs-text'//1,         % ?Code
    'OWS'//0,
    'partial-URI'//1,      % -Uri
    pseudonym//1,          % -Pseudonym
    'quoted-string'//1,    % ?String
    'RWS'//0,
    token//1,              % ?Token
    'transfer-encoding'//1 % ?TransferCodings
  ]
).
:- reexport(library(uri/rfc3986), [
     'absolute-URI'//1,
     authority//2,
     fragment//1,
     host//1 as 'uri-host',
     'path-abempty'//1,
     port//1,
     query//1,
     'relative-part'//3,
     scheme//1,
     segment//1,
     'URI-reference'//1
   ]).
:- reexport(library(dcg/rfc5234)).
:- reexport(library(http/rfc7231)).

/** <module> RFC 7230 - HTTP/1.1: Message Syntax and Routing

@author Wouter Beek
@compat RFC 7230
@see https://tools.ietf.org/html/rfc7230
@version 2017/05-2017/09
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(http/http_generic)).
:- use_module(library(lists)).
:- use_module(library(math_ext)).
:- use_module(library(uri/uri_ext)).

:- discontiguous
    http:http_header/1,
    http:http_separable/1.

:- dynamic
    http:http_header/1,
    http:http_separable/1.

:- meta_predicate
    +##(//, ?, ?),
    +##(3, ?, ?, ?),
    *##(//, ?, ?),
    *##(3, ?, ?, ?),
    'm##n'(?, ?, //, ?, ?),
    'm##n'(?, ?, 3, ?, ?, ?).

:- multifile
    http:http_header/1,
    http:http_separable/1.





%! +##(:Dcg_0)// .
%! +##(:Dcg_1, ?Arguments:list)// .

+##(Dcg_0) -->
  'm##n'(1, _, Dcg_0).


+##(Dcg_1, Arguments) -->
  'm##n'(1, _, Dcg_1, Arguments).



%! *##(:Dcg_0)// .
%! *##(:Dcg_1, ?Arguments:list)// .

*##(Dcg_0) -->
  'm##n'(0, _, Dcg_0).


*##(Dcg_1, Arguments) -->
  'm##n'(0, _, Dcg_1, Arguments).



%! 'm##n'(?Low:nonneg, ?High:nonneg, :Dcg_0)// .
%! 'm##n'(?Low:nonneg, ?High:nonneg, :Dcg_1, ?Arguments:list)// .
%
% ```abnf
% <n>#<m>element => element <n-1>*<m-1>( OWS "," OWS element )
% ```
%
% Empty elements do not contribute to the count of elements present.

'm##n'(Low, High, Dcg_0) -->
  'm*&n'(Low, High, Dcg_0, http_sep).


'm##n'(Low, High, Dcg_1, Arguments) -->
  'm*&n'(Low, High, Dcg_1, http_sep, Arguments).



%! 'absolute-form'(-Uri:compound)// .
%
% ```abnf
% absolute-form = absolute-URI
% ```

'absolute-form'(Uri) -->
  'absolute-URI'(Uri).



%! 'absolute-path'(?Segments:list(atom))// .
%
% ```abnf
% absolute-path = 1*( "/" segment )
% ```

'absolute-path'(Segments) -->
  +(sep_segment_, Segments).

sep_segment_(Segment) -->
  "/",
  segment(Segment).



%! 'asterisk-form'// .
%
% ```abnf
% asterisk-form = "*"
% ```

'asterisk-form' -->
  "*".



%! 'authority-form'(-Authority:compound)// .
%
% ```abnf
% authority-form = authority
% ```

'authority-form'(Authority) -->
  authority(_, Authority).



%! 'BWS'// .
%
% Bad whitespace.
%
% ```abnf
% BWS = OWS
% ```

'BWS' -->
  'OWS'.



%! chunk(-Chunk:compound)// .
%
% ```abnf
% chunk = chunk-size [ chunk-ext ] CRLF chunk-data CRLF
% ```
%
% @bug It's a mistake to make chunk-ext optional when its also Kleene star.

chunk(chunk(Size,Codes,Extensions)) -->
  'chunk-size'(Size),
  'chunk-ext'(Extensions),
  'CRLF',
  'chunk-data'(Codes),
  'CRLF'.



%! 'chunk-data'(?Codes:list(code))// .
%
% A sequence of chunk-size octets
%
% ```abnf
% chunk-data = 1*OCTET
% ```

'chunk-data'(Codes) -->
  +('OCTET', Codes).



%! 'chunk-ext'(-Extensions:list(or([atom,pair(atom)])))// .
%
% ```abnf
% chunk-ext = *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
% ```

'chunk-ext'(Extensions) -->
  *('chunk-ext_', Extensions).

'chunk-ext_'(Extension) -->
  ";",
  'chunk-ext-name'(Key),
  (   "="
  ->  'chunk-ext-val'(Val),
      {Extension = Key-Val}
  ;   {Extension = Key}
  ).



%! 'chunk-ext-name'(?Name:atom)// .
%
% ```abnf
% chunk-ext-name = token
% ```

'chunk-ext-name'(Name) -->
  token(Name).



%! 'chunk-ext-val'(?Value:atom)// .
%
% ```abnf
% chunk-ext-val = token / quoted-string
% ```

'chunk-ext-val'(Value) -->
  token(Value).
'chunk-ext-val'(Value) -->
  'quoted-string'(Value).



%! 'chunk-size'(?Size:nonneg)// .
%
% ```abnf
% chunk-size = 1*HEXDIG
% ```

'chunk-size'(Size) -->
  dcg_integer(+('HEXDIG'), 16, Size).



%! 'chunked-body'(-Chunks:list, -LastChunk, -Headers:list)// .
%
% ```abnf
% chunked-body = *chunk last-chunk trailer-part CRLF
% ```

'chunked-body'(Chunks, LastChunk, Headers) -->
  *(chunk, Chunks),
  'last-chunk'(LastChunk),
  'trailer-part'(Headers),
  'CRLF'.



%! comment(-Comment:string)// .
%
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment(Comment) -->
  dcg_string(comment_1, Comment).

comment_1([0'(|T]) -->
  "(", comment_2(T0), ")",
  {append(T0, [0')], T)}.

comment_2([H|T]) -->
  ctext(H), !,
  comment_2(T).
comment_2([H|T]) -->
  'quoted-pair'(H), !,
  comment_2(T).
comment_2(L) -->
  comment_1(L), !.
comment_2([]) --> "".



%! connection(-Options:list(atom))// .
%
% ```abnf
% 'Connection'(S) --> 1#(connection-option)
% ```

http:http_header(connection).
http:http_separable(connection).
connection(Options) -->
  +##('connection-option', Options).



%! 'connection-option'(?Option:atom)// .
%
% ```abnf
% connection-option = token
% ```

'connection-option'(Option) -->
  token(Option).



%! 'content-length'(?Length:nonneg)// .
%
% ```abnf
% Content-Length = 1*DIGIT
% ```
%
% Example: `Content-Length: 3495`

http:http_header('content-length').
'content-length'(Length) -->
  dcg_integer(+('DIGIT'), Length).



%! ctext(?Code:code)// .
%
% ```abnf
% ctext = HTAB | SP | %x21-27 | %x2A-5B | %x5D-7E | obs-text
% ```

ctext(Code) --> 'HTAB'(Code).
ctext(Code) --> 'SP'(Code).
ctext(Code) --> dcg_between(0x21, 0x27, Code).
ctext(Code) --> dcg_between(0x2A, 0x5B, Code).
ctext(Code) --> dcg_between(0x5D, 0x7E, Code).
ctext(Code) --> 'obs-text'(Code).



%! 'field-content'(+Name:atom, ?Value:term)// .
%
% ```abnf
% field-content = field-vchar [ 1*( SP | HTAB ) field-vchar ]
% ```

'field-content'(Name, Value) -->
  parsing, !,
  (   {current_predicate(Name/3)}
  ->  (   % (1/4) valid value
          dcg_once(dcg_call(Name, Value)),
          'OWS',
          % @note This should fail in case only _part_ of the HTTP
          %       header is parsed.
          eos
      ->  ""
      ;   % (2/4) empty value
          phrase('obs-fold')
      ->  {
            Value = empty_http_value(Name),
            print_message(warning, Value)
          }
      ;    % (3/4) buggy value
          rest_as_string(Raw),
          {
            Value = buggy_http_header(Name,Raw),
            print_message(warning, Value)
          }
      )
  ;   % (4/4) unknown key
      rest_as_string(Raw),
      {
        (   http:http_header(Name)
        ->  Value = undefined_http_header(Name,Raw)
        ;   Value = unknown_http_header(Name,Raw)
        ),
        print_message(warning, Value)
      }
  ).
'field-content'(Name, Value) -->
  dcg_once(dcg_call(Name, Value)).
/*
'field-content'(L) -->
  'field-vchar'(H1),
  (   +('field-content_'),
      'field-vchar'(H2),
      {L = [H1,H2]}
  ;   {L = [H1]}
  ).

'field-content_' --> 'SP'.
'field-content_' --> 'HTAB'.
*/



%! 'field-name'(?Name:atom)// .
%
% ```abnf
% field-name = token
% ```
%
% Ensured to be in lower case when parsing, in order to match DCG
% rules that implement each header's grammar.

'field-name'(LowerName) -->
  parsing, !,
  token(Name),
  {downcase_atom(Name, LowerName)}.
'field-name'(Name) -->
  token(Name).



%! 'field-value'(+Name:atom, ?Value:term)// .
%
% ```abnf
% field-value = *( field-content | 'obs-fold' )
% ```

'field-value'(Name, Value) -->
  'field-content'(Name, Value).
/*
'field-value'(Content) -->
  'field-content'(Content).
'field-value'([]) -->
  'obs-fold'.
*/



%! 'field-vchar'(?Code:code)// .
%
% ```abnf
% field-vchar = VCHAR | obs-text
% ```

'field-vchar'(Code) --> 'VCHAR'(Code).
'field-vchar'(Code) --> 'obs-text'(Code).



%! 'header-field'(?Header:pair(atom,term))// .
%
% ```abnf
% header-field = field-name ":" OWS field-value OWS
% ```

'header-field'(Name-Value) -->
  'field-name'(Name),
  ":", 'OWS',
  'field-value'(Name, Value).



%! host(-Authority:compound)// .
%
% ```abnf
% Host = uri-host [ ":" port ] ; Section 2.7.1
% ```

http:http_header(host).
host(auth(_User,_Password,Host,Port)) -->
  'uri-host'(Host),
  (":" -> port(Port) ; "").



%! 'HTTP-message'(?Message:compound)// .
%
% Message has the following form:
%
% ```prolog
% http(StartLine:compound,Headers:list(pair(atom,term)),Body:list(code))
% ```
%
% ```abnf
% HTTP-message = start-line *( header-field CRLF ) CRLF [ message-body ]
% ```

'HTTP-message'(http(StartLine,Headers,Body)) -->
  'start-line'(StartLine),
  *('header-field_eol', Headers),
  'CRLF',
  dcg_default('message-body', Body, []),
  'CRLF'.



%! 'HTTP-name'// .
%
% "HTTP", case-sensitive
%
% ```abnf
% HTTP-name = %x48.54.54.50
% ```

'HTTP-name' --> [0x48,0x54,0x54,0x50].



%! 'http-URI'(-Uri:compound)// .
%
% ```abnf
% http-URI = "http:" "//" authority path-abempty [ "?" query ] [ "#" fragment ]
% ```

'http-URI'(uri(http,Authority,Segments,Query,Fragment)) -->
  "http://",
  authority(http, Authority),
  'path-abempty'(Segments),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Fragment) ; "").



%! 'HTTP-version'(-Version:pair(nonneg))// .
%
% ```abnf
% HTTP-version = HTTP-name "/" DIGIT "." DIGIT
% ```

'HTTP-version'(Major-Minor) -->
  'HTTP-name',
  http_sep(0'/),
  'DIGIT'(Major),
  http_sep(0'.),
  'DIGIT'(Minor).



%! 'https-URI'(-Uri:compound)// .
%
% ```abnf
% https-URI = "https:" "//" authority path-abempty [ "?" query ] [ "#" fragment ]
% ```

'https-URI'(uri(https,Authority,Segments,Query,Fragment)) -->
  atom_ci('https://'),
  authority(https, Authority),
  'path-abempty'(Segments),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Fragment) ; "").



%! 'last-chunk'(-Extensions:list)// .
%
% ```abnf
% last-chunk = 1*("0") [ chunk-ext ] CRLF
% ```
%
% @bug It's a mistake to make chunk-ext optional when its also Kleene star.

'last-chunk'(Extensions) -->
  +("0"), !,
  'chunk-ext'(Extensions),
  'CRLF'.



%! 'message-body'(-Body:list(code))// .
%
% ```abnf
% message-body = *OCTET
% ```

'message-body'(Codes) -->
  *('OCTET', Codes), !.



%! method(-Method:atom)// .
%
% ```abnf
% method = token
% ```
%
% The following methods are defined by HTTP 1.1:
%
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



%! 'obs-fold'// .
%
% Obsolete line folding.
%
% ```abnf
% obs-fold = CRLF 1*( SP | HTAB )
% ```

'obs-fold' -->
  'CRLF',
  +(sp_or_htab).



%! 'obs-text'(-Code:code)// .
%
% ```abnf
% obs-text = %x80-FF
% ```

'obs-text'(Code) -->
  dcg_between(0x80, 0xFF, Code).



%! 'origin-form'(-Uri:compound)// .
%
% ```abnf
% origin-form = absolute-path [ "?" query ]
% ```

'origin-form'(uri(_,_,Segments,Query,_)) -->
  'absolute-path'(Segments),
  ("?" -> query(Query) ; {Query = []}).



%! 'OWS'// .
%
% Optional whitespace.
%
% ```abnf
% OWS = *( SP | HTAB )
% ```

'OWS' -->
  *(sp_or_htab).



%! 'partial-URI'(-Uri:atom)// .
%
% ```abnf
% partial-URI = relative-part [ "?" query ]
% ```

'partial-URI'(Uri) -->
  'relative-part'(_, Authority, Segments),
  ("?" -> query(Query) ; ""),
  {uri_comps(Uri, uri(_,Authority,Segments,Query,_))}.



%! protocol(-Protocol:or([atom,pair(atom)]))// .
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



%! 'protocol-name'(?Name:atom)// .
%
% ```abnf
% protocol-name = token
% ```

'protocol-name'(Name) -->
  token(Name).



%! 'protocol-version'(?Version:atom)// .
%
% ```abnf
% protocol-version = token
% ```

'protocol-version'(Version) -->
  token(Version).



%! pseudonym(?Pseudonym:atom)// .
%
% ```abnf
% pseudonym = token
% ```

pseudonym(Pseudonym) -->
  token(Pseudonym).



%! qdtext(?Code:code)// .
%
% ```abnf
% qdtext = HTAB | SP | %x21 | %x23-5B | %x5D-7E | obs-text
% ```

qdtext(Code) --> 'HTAB'(Code).
qdtext(Code) --> 'SP'(Code).
qdtext(0x21) --> [0x21].
qdtext(Code) --> dcg_between(0x23, 0x5B, Code).
qdtext(Code) --> dcg_between(0x5D, 0x7E, Code).
qdtext(Code) --> 'obs-text'(Code).



%! 'quoted-pair'(?Code:code)// .
%
% ```abnf
% quoted-pair = "\" ( HTAB | SP | VCHAR | obs-text )
% ```

'quoted-pair'(Code) -->
  "\\",
  (   'HTAB'(Code)
  ;   'SP'(Code)
  ;   'VCHAR'(Code)
  ;   'obs-text'(Code)
  ).



%! 'quoted-string'(?String:atom)// .
%
% ```abnf
% quoted-string = DQUOTE *( qdtext | quoted-pair ) DQUOTE
% ```

'quoted-string'(String) -->
  'DQUOTE',
  dcg_atom(*(quoted_string_), String),
  'DQUOTE'.

quoted_string_(Code) -->
  qdtext(Code).
quoted_string_(Code) -->
  'quoted-pair'(Code).



%! rank(-Rank:between(0.0,1.0))// .
%
% ```abnf
% rank = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ```

rank(N)   -->
  "0",
  (   "."
  ->  'm*n'(0, 3, 'DIGIT', Weights),
      {
        fractional_weights(N0, Weights),
        N is float(N0)
      }
  ;   {N = 0}
  ).
rank(1.0) -->
  "1",
  ("." -> 'm*n'(0, 3, "0") ; "").



%! 'reason-phrase'(?Reason:string)// .
%
% ```abnf
% reason-phrase = *( HTAB | SP | VCHAR | obs-text )
% ```

'reason-phrase'(Reason) -->
  dcg_string(*(reason_phrase_), Reason).

reason_phrase_(Code) --> 'HTAB'(Code).
reason_phrase_(Code) --> 'SP'(Code).
reason_phrase_(Code) --> 'VCHAR'(Code).
reason_phrase_(Code) --> 'obs-text'(Code).



%! 'received-by'(-Receiver:compound)// .
%
% ```abnf
% received-by = ( uri-host [ ":" port ] ) | pseudonym
% ```

'received-by'(auth(_User,_Password,Host,Port)) -->
  'uri-host'(Host), !,
  (":" -> port(Port) ; {Port = 80}).
'received-by'(Pseudonym) -->
  pseudonym(Pseudonym).



%! 'received-protocol'(-Protocol:or([atom,pair(atom)]))// .
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



%! 'request-line'(-RequestLine:compound)// .
%
% RequestLine has the following form:
%
% ```prolog
% request_line(Method:atom,Uri:atom,Version:pair(nonneg))
% ```
%
% ```abnf
% request-line = method SP request-target SP HTTP-version CRLF
% ```

'request-line'(request_line(Method,Uri,Version)) -->
  method(Method),
  'SP',
  'request-target'(Uri),
  'SP',
  'HTTP-version'(Version),
  'CRLF'.



%! 'request-target'(-RequestTarget:compound)// .
%
% ```abnf
% request-target = origin-form
%                | absolute-form
%                | authority-form
%                | asterisk-form
% ```

'request-target'(Uri) --> 'origin-form'(Uri).
'request-target'(Uri) --> 'absolute-form'(Uri).
'request-target'(Uri) --> 'authority-form'(Uri).
'request-target'('*') --> 'asterisk-form'.



%! 'RWS'// .
%
% Required whitespace.
%
% ```abnf
% RWS = 1*( SP | HTAB )
% ```

'RWS' -->
  +(sp_or_htab).



%! 'start-line'(?StartLine:compound)// .
%
% ```abnf
% start-line = request-line | status-line
% ```

'start-line'(RequestLine) -->
  'request-line'(RequestLine).
'start-line'(StatusLine) -->
  'status-line'(StatusLine).



%! 'status-line'(?StatusLine:compound)// .
%
% StatusLine has the following form:
%
% ```prolog
% status_line(Version:pair(nonneg),Status:between(100,599),Reason:string)
% ```
%
% ```abnf
% status-line = HTTP-version SP status-code SP reason-phrase CRLF
% ```

'status-line'(status_line(Version,Status,Reason)) -->
  'HTTP-version'(Version),
  'SP',
  'status-code'(Status),
  {http_status_reason(Status, Reason)},
  'SP',
  'reason-phrase'(Reason),
  'CRLF'.



%! 'status-code'(?Status:between(100,599))// .
%
% ```abnf
% status-code = 3DIGIT
% ```

'status-code'(Status) -->
  dcg_integer(#(3, 'DIGIT'), Status).



%! 't-codings'(-TransferCoding:pair(between(0.0,1.0),term))// .
%
% ```abnf
% t-codings = "trailers" | ( transfer-coding [ t-ranking ] )
% ```

't-codings'(1.0-trailers) -->
  atom_ci(trailers).
't-codings'(Rank-TransferCoding) -->
  'transfer-coding'(TransferCoding),
  ('t-ranking'(Rank) -> "" ; {Rank = 1.0}).



%! 't-ranking'(-Rank:float)// .
%
% ```abnf
% t-ranking = OWS ";" OWS "q=" rank
% ```

't-ranking'(Rank) -->
  http_sep(0';),
  atom_ci('q='),
  rank(Rank).



%! tchar(?Code:code)// .
%
% Any VCHAR, except delimiter.
%
% ```abnf
% tchar = "!" | "#" | "$" | "%" | "&" | "'" | "*"
%       | "+" | "-" | "." | "^" | "_" | "`" | "|" | "~"
%       | DIGIT | ALPHA
% ```

tchar(Code) --> 'ALPHA'(Code).
tchar(Code) --> 'DIGIT'(_, Code).
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



%! te(-TCodings:list)// .
%
% ```abnf
% TE = #t-codings
% ```
%
% Examples:
%
%   - `TE: deflate`
%   - `TE:`
%   - `TE: trailers, deflate;q=0.5`

http:http_header(te).
http:http_separable(te).
te(L) -->
  *##('t-codings', L).



%! token(?Token)// .
%
% ```abnf
% token = 1*tchar
% ```

token(Token) -->
  dcg_once(dcg_atom(+(tchar), Token)).



%! trailer(-Fields:list(atom))// .
%
% ```abnf
% Trailer = 1#field-name
% ```

http:http_header(trailer).
http:http_separable(trailer).
trailer(L) -->
  +##('field-name', L), !.



%! 'trailer-part'(-Headers:list(pair(string)))// .
% ```abnf
% trailer-part = *( header-field CRLF )
% ```

'trailer-part'(Headers) -->
  *('header-field_eol', Headers).



%! 'transfer-coding'(?TransferCoding:term)// .
%
% ```abnf
% transfer-coding = "chunked"
%                 | "compress"
%                 | "deflate"
%                 | "gzip"
%                 | transfer-extension
% ```

'transfer-coding'(chunked) -->
  atom_ci(chunked).
'transfer-coding'(compress) -->
  atom_ci(compress).
'transfer-coding'(deflate) -->
  atom_ci(deflate).
'transfer-coding'(gzip) -->
  atom_ci(gzip).
'transfer-coding'(Name-Parameters) -->
  'transfer-extension'(Name, Parameters).



%! 'transfer-encoding'(?TransferCodings:list(dict))// .
%
% ```abnf
% Transfer-Encoding = 1#transfer-coding
% ```
%
% Example: `Transfer-Encoding: gzip, chunked`

http:http_header('transfer-encoding').
http:http_separable('transfer-encoding').
'transfer-encoding'(L) -->
  +##('transfer-coding', L).



%! 'transfer-extension'(?Name:atom, ?Parameters:list(pair(atom)))// .
%
% ```abnf
% transfer-extension = token *( OWS ";" OWS transfer-parameter )
% ```

'transfer-extension'(Name, Parameters) -->
  token(Name),
  *('sep_transfer-parameter', Parameters).

'sep_transfer-parameter'(Parameter) -->
  http_sep(0';),
  'transfer-parameter'(Parameter).



%! 'transfer-parameter'(?Parameter:pair(atom))// .
%
% ```abnf
% transfer-parameter = token BWS "=" BWS ( token | quoted-string )
% ```

'transfer-parameter'(Key-Value) -->
  token(Key),
  'BWS',
  "=",
  'BWS',
  (token(Value) ; 'quoted-string'(Value)).



%! upgrade(-Protocols:list(or([atom,pair(atom)])))// is det.
%
% ```abnf
% upgrade = 1#protocol
% ```

http:http_header(upgrade).
http:http_separable(upgrade).
upgrade(L) -->
  +##(protocol, L).



%! via(-L:list(compound))// .
%
% ```abnf
% Via = 1#( received-protocol RWS received-by [ RWS comment ] )
% ```

http:http_header(via).
http:http_separable(via).
via(L) -->
  +##(via_component, L), !.

via_component(via(ReceivedProtocol,ReceivedBy,Comment)) -->
  'received-protocol'(ReceivedProtocol),
  'RWS',
  'received-by'(ReceivedBy),
  ('RWS' -> comment(Comment) ; "").





% HELPERS %

%! 'header-field_eol'(?Header:compound)// is det.

'header-field_eol'(Header) -->
  'header-field'(Header),
  'CRLF'.



%! http_sep// is det.
%! http_sep(?Code:code)// is det.

http_sep -->
  http_sep(0',).


http_sep(Code) -->
  'OWS',
  [Code],
  'OWS'.



%! sp_or_htab// is det.

sp_or_htab --> 'SP'.
sp_or_htab --> 'HTAB'.
