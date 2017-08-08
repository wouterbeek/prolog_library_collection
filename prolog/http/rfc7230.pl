:- module(
  rfc7230,
  [
    '##'//1,               % :Dcg_0
    '##'//2,               % :Dcg_1, ?Args
    '+##'//1,              % :Dcg_0
    '+##'//2,              % :Dcg_1, ?Args
    '*##'//1,              % :Dcg_0
    '*##'//2,              % :Dcg_1, ?Args
    '##n'//2,              % ?High, :Dcg_0
    '##n'//3,              % ?High, :Dcg_1, ?Args
    'm##'//2,              % ?Low, :Dcg_0
    'm##'//3,              % ?Low, :Dcg_1, ?Args
    'm##n'//3,             % ?Low, ?High, :Dcg_0
    'm##n'//4,             % ?Low, ?High, :Dcg_1, ?Args
    'BWS'//0,
    comment//1,            % -Comment:string
    'field-name'//1,       % -Name:atom
    'HTTP-message'//1,     % -Message:compound
    http_sep//0,
    http_sep//1,           % +Code
    method//1,             % -Method:atom
    'obs-text'//1,         % -Code:code
    'OWS'//0,
    'partial-URI'//1,      % -Uri:atom
    pseudonym//1,          % -Pseudonym:atom
    'quoted-string'//1,    % -String:atom
    'RWS'//0,
    token//1,              % -Token:atom
    'transfer-encoding'//1 % -TransferCodings:list(dict)
  ]
).
:- reexport(library(dcg/rfc5234)).
:- reexport(library(uri/rfc3986), [
     'absolute-URI'//1,
     authority//1,
     fragment//1,
     host//1 as 'uri-host',
     'path-abempty'//1,
     port//1,
     query//1,
     'relative-part'//2,
     scheme//1,
     segment//1,
     'URI-reference'//1
   ]).

/** <module> RFC 7230 - HTTP/1.1: Message Syntax and Routing

@author Wouter Beek
@compat RFC 7230
@see https://tools.ietf.org/html/rfc7230
@version 2017/05-2017/06, 2017/08
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
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
    '##'(//, ?, ?),
    '##'(3, ?, ?, ?),
    '+##'(//, ?, ?),
    '+##'(3, ?, ?, ?),
    '*##'(//, ?, ?),
    '*##'(3, ?, ?, ?),
    '##n'(?, //, ?, ?),
    '##n'(?, 3, ?, ?, ?),
    'm##'(?, //, ?, ?),
    'm##'(?, 3, ?, ?, ?),
    'm##n'(?, ?, //, ?, ?),
    'm##n'(?, ?, 3, ?, ?, ?),
    'field-content'(3, -, ?, ?),
    'field-value'(?, 3, -).

:- multifile
    http:http_header/1,
    http:http_separable/1.





%! '##'(:Dcg_0)// .
%! '##'(:Dcg_1, ?Args:list)// .

'##'(Dcg_0) -->
  'm##n'(_, _, Dcg_0).


'##'(Dcg_1, Args) -->
  'm##n'(_, _, Dcg_1, Args).



%! '+##'(:Dcg_0)// .
%! '+##'(:Dcg_1, ?Args:list)// .

'+##'(Dcg_0) -->
  'm##n'(1, _, Dcg_0).


'+##'(Dcg_1, Args) -->
  'm##n'(1, _, Dcg_1, Args).



%! '*##'(:Dcg_0)// .
%! '*##'(:Dcg_1, ?Args:list)// .

'*##'(Dcg_0) -->
  'm##n'(0, _, Dcg_0).


'*##'(Dcg_1, Args) -->
  'm##n'(0, _, Dcg_1, Args).



%! '##n'(?High:nonneg, :Dcg_0)// .
%! '##n'(?High:nonneg, :Dcg_1, ?Args:list)// .

'##n'(High, Dcg_0) -->
  'm##n'(_, High, Dcg_0).


'##n'(High, Dcg_1, Args) -->
  'm##n'(_, High, Dcg_1, Args).



%! 'm##'(?Low:nonneg, :Dcg_0)// .
%! 'm##'(?Low:nonneg, :Dcg_1, ?Args:list)// .

'm##'(Low, Dcg_0) -->
  'm##n'(Low, _, Dcg_0).


'm##'(Low, Dcg_1, Args) -->
  'm##n'(Low, _, Dcg_1, Args).



%! 'm##n'(?Low:nonneg, ?High:nonneg, :Dcg_0)// .
%! 'm##n'(?Low:nonneg, ?High:nonneg, :Dcg_1, ?Args:list)// .
%
% ```abnf
% <n>#<m>element => element <n-1>*<m-1>( OWS "," OWS element )
% ```
%
% Empty elements do not contribute to the count of elements present.

'm##n'(Low, High, Dcg_0) -->
  'm*&n'(Low, High, Dcg_0, http_sep).


'm##n'(Low, High, Dcg_1, Args) -->
  'm*&n'(Low, High, Dcg_1, http_sep, Args).



%! 'absolute-form'(-Uri:compound)// .
%
% ```abnf
% absolute-form = absolute-URI
% ```

'absolute-form'(Uri) -->
  'absolute-URI'(Uri).



%! 'absolute-path'(-Segments:list(atom))// .
%
% ```abnf
% absolute-path = 1*( "/" segment )
% ```

'absolute-path'(Segments) --> +(sep_segment0, Segments).

sep_segment0(Segment) -->
  "/",
  segment(Segment).



%! 'asterisk-form'// .
%
% ```abnf
% asterisk-form = "*"
% ```

'asterisk-form' -->
  "*".



%! 'authority-form'(-Auth:compound)// .
%
% ```abnf
% authority-form = authority
% ```

'authority-form'(Auth) -->
  authority(Auth).



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
  'chunk-ext'(Extensions), 'CRLF',
  'chunk-data'(Codes), 'CRLF'.



%! 'chunk-data'(-Codes:list(code))// .
%
% A sequence of chunk-size octets
%
% ```abnf
% chunk-data = 1*OCTET
% ```

'chunk-data'(Codes) -->
  +('OCTET', Codes), !.



%! 'chunk-ext'(-Extensions:list(or([atom,pair(atom)])))// .
%
% ```abnf
% chunk-ext = *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
% ```

'chunk-ext'(Extensions) -->
  *(sep_chunk_ext0, Extensions), !.

sep_chunk_ext0(Extension) -->
  ";",
  'chunk-ext-name'(Key),
  (   "="
  ->  'chunk-ext-val'(Val),
      {Extension = Key-Val}
  ;   {Extension = Key}
  ).



%! 'chunk-ext-name'(-Name:atom)// .
%
% ```abnf
% chunk-ext-name = token
% ```

'chunk-ext-name'(Name) -->
  token(Name).



%! 'chunk-ext-val'(-Val:atom)// .
%
% ```abnf
% chunk-ext-val = token / quoted-string
% ```

'chunk-ext-val'(Val) -->
  token(Val), !.
'chunk-ext-val'(Val) -->
  'quoted-string'(Val).



%! 'chunk-size'(-Size:nonneg)// .
%
% ```abnf
% chunk-size = 1*HEXDIG
% ```

'chunk-size'(Size) -->
  +('HEXDIG', Weights), !,
  {integer_weights(Size, 16, Weights)}.



%! 'chunked-body'(-ChunkedBody:)// .
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
  dcg_string(comment_codes1, Comment).

comment_codes1([0'(|T]) -->
  "(", comment_codes2(T0), ")",
  {append(T0, [0')], T)}.

comment_codes2([H|T]) -->
  ctext(H), !,
  comment_codes2(T).
comment_codes2([H|T]) -->
  'quoted-pair'(H), !,
  comment_codes2(T).
comment_codes2(L) -->
  comment_codes1(L), !.
comment_codes2([]) --> "".



%! connection(-Opts:list(atom))// .
%
% ```abnf
% 'Connection'(S) --> 1#(connection-option)
% ```

http:http_header(connection).
http:http_separable(connection).
connection(Opts) -->
  +##('connection-option', Opts).



%! 'connection-option'(-Opt)// .
%
% ```abnf
% connection-option = token
% ```

'connection-option'(Opt) -->
  token(Opt).



%! 'content-length'(-Length:nonneg)// .
%
% ```abnf
% Content-Length = 1*DIGIT
% ```
%
% Example: `Content-Length: 3495`

http:http_header('content-length').
'content-length'(Length) -->
  +('DIGIT', Weights), !,
  {integer_weights(Length, Weights)}.



%! ctext(-Code:code)// .
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



%! 'field-content'(:Key_3, -Value:term)// .
%
% ```abnf
% field-content = field-vchar [ 1*( SP | HTAB ) field-vchar ]
% ```

'field-content'(Mod:Key_3, Value) -->
  (   {current_predicate(Key_3/3)}
  ->  (   % (1/4) valid value
          dcg_call(Mod:Key_3, Value),
          'OWS',
          % @note This should fail in case only _part_ of the HTTP
          %       header is parsed.
          eos
      ->  ""
      ;   % (2/4) empty value
          phrase('obs-fold')
      ->  {
            Value = empty_http_value(Key_3),
            print_message(warning, Value)
          }
      ;    % (3/4) buggy value
          rest_as_string(Raw),
          {
            Value = buggy_http_header(Key_3,Raw),
            print_message(warning, Value)
          }
      )
  ;   % (4/4) unknown key
      rest_as_string(Raw),
      {
        (   http:http_header(Key_3)
        ->  Value = undefined_http_header(Key_3,Raw)
        ;   Value = unknown_http_header(Key_3,Raw)
        ),
        print_message(warning, Value)
      }
  ).



%! 'field-name'(-Name:atom)// .
%
% ```abnf
% field-name = token
% ```

'field-name'(LowerA) -->
  token(A),
  {downcase_atom(A, LowerA)}.



%! 'field-value'(+Codes:list(code), -Pair:pair(atom,term))// .
%
% ```abnf
% field-value = *( field-content | 'obs-fold' )
% ```

'field-value'(Codes, Key, Value) :-
  phrase('field-content'(Key, Value), Codes).



%! 'field-vchar'(-Code:code)// .
%
% ```abnf
% field-vchar = VCHAR | obs-text
% ```

'field-vchar'(C) --> 'VCHAR'(C).
'field-vchar'(C) --> 'obs-text'(C).



%! 'header-field'(-Header:pair(atom,compound))// .
%
% ```abnf
% header-field = field-name ":" OWS field-value OWS
% ```

'header-field'(Key-Value) -->
  'field-name'(Key),
  ":", 'OWS',
  rest(Codes),
  {'field-value'(Codes, Key, Value)}.



%! host(-Auth:compound)// .
%
% ```abnf
% Host = uri-host [ ":" port ] ; Section 2.7.1
% ```

http:http_header(host).
host(auth(_,Host,Port)) -->
  'uri-host'(Host),
  (":" -> port(Port) ; "").



%! 'HTTP-message'(-Message:compound)// .
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
  *(header_field_eol, Headers),
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



%! http_sep// is det.
%! http_sep(+Code:code)// is det.

http_sep -->
  http_sep(0',).


http_sep(Code) -->
  'OWS',
  [Code],
  'OWS'.



%! 'http-URI'(-Uri:compound)// .
%
% ```abnf
% http-URI = "http:" "//" authority path-abempty [ "?" query ] [ "#" fragment ]
% ```

'http-URI'(uri(http,Auth,Segments,QueryComps,Frag)) -->
  "http://",
  authority(Auth),
  'path-abempty'(Segments),
  ("?" -> query(QueryComps) ; ""),
  ("#" -> fragment(Frag) ; "").



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

'https-URI'(uri(https,Auth,Segments,QueryComps,Frag)) -->
  atom_ci('https://'),
  authority(Auth),
  'path-abempty'(Segments),
  ("?" -> query(QueryComps) ; ""),
  ("#" -> fragment(Frag) ; "").



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
  +(sp_or_htab), !.



%! 'obs-text'(-Code:code)// .
%
% ```abnf
% obs-text = %x80-FF
% ```

'obs-text'(C) -->
  [C],
  {between(0x80, 0xFF, C)}.



%! 'origin-form'(-Uri:compound)// .
%
% ```abnf
% origin-form = absolute-path [ "?" query ]
% ```

'origin-form'(uri(_,_,Segments,QueryComps,_)) -->
  'absolute-path'(Segments),
  ("?" -> query(QueryComps) ; {QueryComps = []}).



%! 'OWS'// .
%
% Optional whitespace.
%
% ```abnf
% OWS = *( SP | HTAB )
% ```

'OWS' -->
  *(sp_or_htab), !.



%! 'partial-URI'(-Uri:atom)// .
%
% ```abnf
% partial-URI = relative-part [ "?" query ]
% ```

'partial-URI'(Uri) -->
  'relative-part'(Auth, Segments),
  ("?" -> query(Query) ; ""),
  {uri_comps(Uri, uri(_,Auth,Segments,Query,_))}.



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



%! 'protocol-name'(-Name:atom)// .
%
% ```abnf
% protocol-name = token
% ```

'protocol-name'(Name) -->
  token(Name).



%! 'protocol-version'(-Version:atom)// .
%
% ```abnf
% protocol-version = token
% ```

'protocol-version'(Version) -->
  token(Version).



%! pseudonym(-Pseudonym:atom)// .
%
% ```abnf
% pseudonym = token
% ```

pseudonym(Pseudonym) -->
  token(Pseudonym).



%! qdtext(-Code:code)// .
%
% ```abnf
% qdtext = HTAB | SP | %x21 | %x23-5B | %x5D-7E | obs-text
% ```

qdtext(C)    --> 'HTAB'(C).
qdtext(C)    --> 'SP'(C).
qdtext(0x21) --> [0x21].
qdtext(C)    --> [C], {(between(0x23, 0x5B, C), ! ; between(0x5D, 0x7E, C))}.
qdtext(C)    --> 'obs-text'(C).



%! 'quoted-pair'(-Code:code)// .
%
% ```abnf
% quoted-pair = "\" ( HTAB | SP | VCHAR | obs-text )
% ```

'quoted-pair'(C) -->
  "\\",
  ('HTAB'(C) ; 'SP'(C) ; 'VCHAR'(C) ; 'obs-text'(C)).



%! 'quoted-string'(-String:atom)// .
%
% ```abnf
% quoted-string = DQUOTE *( qdtext | quoted-pair ) DQUOTE
% ```

'quoted-string'(String) -->
  'DQUOTE',
  *(quoted_string_code0, Codes), !,
  'DQUOTE',
  {atom_codes(String, Codes)}.

quoted_string_code0(C) --> qdtext(C).
quoted_string_code0(C) --> 'quoted-pair'(C).



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



%! 'reason-phrase'(-Reason:string)// .
%
% ```abnf
% reason-phrase = *( HTAB | SP | VCHAR | obs-text )
% ```

'reason-phrase'(Reason) -->
  *(reason_phrase_code, Codes),
  {string_codes(Reason, Codes)}.

reason_phrase_code(C) --> 'HTAB'(C).
reason_phrase_code(C) --> 'SP'(C).
reason_phrase_code(C) --> 'VCHAR'(C).
reason_phrase_code(C) --> 'obs-text'(C).



%! 'received-by'(-Receiver:compound)// .
%
% ```abnf
% received-by = ( uri-host [ ":" port ] ) | pseudonym
% ```

'received-by'(auth(_,Host,Port)) -->
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
  +(sp_or_htab), !.



%! 'start-line'(-StartLine:compound)// .
%
% ```abnf
% start-line = request-line | status-line
% ```

'start-line'(RequestLine) -->
  'request-line'(RequestLine), !.
'start-line'(StatusLine) -->
  'status-line'(StatusLine).



%! 'status-line'(-StatusLine:compound)// .
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
  'HTTP-version'(Version), 'SP',
  'status-code'(Status), 'SP',
  'reason-phrase'(Reason), 'CRLF'.



%! 'status-code'(-Status:between(100,599))// .
%
% ```abnf
% status-code = 3DIGIT
% ```

'status-code'(Status) -->
  #(3, 'DIGIT', Weights),
  {integer_weights(Status, Weights)}.



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



%! tchar(?C)// .
%
% Any VCHAR, except delimiter.
%
% ```abnf
% tchar = "!" | "#" | "$" | "%" | "&" | "'" | "*"
%       | "+" | "-" | "." | "^" | "_" | "`" | "|" | "~"
%       | DIGIT | ALPHA
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
  *(header_field_eol, Headers).



%! 'transfer-coding'(-TransferCoding)// .
%
% ```abnf
% transfer-coding = "chunked"
%                 | "compress"
%                 | "deflate"
%                 | "gzip"
%                 | transfer-extension
% ```

'transfer-coding'(chunked)    --> atom_ci(chunked), !.
'transfer-coding'(compress)   --> atom_ci(compress), !.
'transfer-coding'(deflate)    --> atom_ci(deflate), !.
'transfer-coding'(gzip)       --> atom_ci(gzip), !.
'transfer-coding'(Name-Pairs) --> 'transfer-extension'(Name, Pairs).



%! 'transfer-encoding'(-TransferCodings:list(dict))// .
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



%! 'transfer-extension'(-Name:atom, -Pairs:list(pair(atom)))// .
%
% ```abnf
% transfer-extension = token *( OWS ";" OWS transfer-parameter )
% ```

'transfer-extension'(Name, Pairs) -->
  token(Name),
  *(sep_transfer_parameter0, Pairs).

sep_transfer_parameter0(Pair) -->
  http_sep(0';),
  'transfer-parameter'(Pair).



%! 'transfer-parameter'(-Param:pair(atom))// .
%
% ```abnf
% transfer-parameter = token BWS "=" BWS ( token | quoted-string )
% ```

'transfer-parameter'(Key-Value) -->
  token(Key),
  'BWS',
  "=",
  'BWS',
  (token(Value), ! ; 'quoted-string'(Value)).



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

%! header_field_eol(Header:compound)// is det.

header_field_eol(Header) -->
  'header-field'(Header),
  'CRLF'.



%! sp_or_htab// is det.

sp_or_htab --> 'SP'.
sp_or_htab --> 'HTAB'.
