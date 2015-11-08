:- module(
  rfc7230,
  [
    'BWS'//0,
    'Connection'//1, % ?Options:list
    'Content-Length'//1, % ?ContentLength:nonneg
    'HTTP-message'//3, % ?StartLine
                       % ?HeaderFields:list(compound)
                       % ?Body:list(code)
    'HTTP-name'//0,
    'HTTP-version'//2, % ?Major:between(0,9)
                       % ?Minor:between(0,9)
    'Host'//2, % ?Host
               % ?Port
    'obs-text'//1, % ?Code:code
    'OWS'//0,
    qdtext//1, % ?Code:code
    'quoted-pair'//1, % ?Code:code
    'quoted-string'//1, % ?String:string
    'RWS'//0,
    tchar//1, % ?TokenCharacter:code
    token//1 % ?String:string
  ]
).

/** <module> RFC 7230: Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing

@author Wouter Beek
@compat RFC 7230
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234)).





%! 'BWS'// .
% ```abnf
% BWS = OWS
% ```

'BWS' --> 'OWS'.



%! 'Connection'(?Options:list)// .
% ```abnf
% Connection = *( "," OWS )
%              connection-option
%              *( OWS "," [ OWS connection-option ] )
% ```

'Connection'([H|T]) -->
  ows1,
  'connection-option'(H),
  connection_options(T).
ows1 --> ",", 'OWS', ows1.
ows1 --> "".
connection_options([H|T]) -->
  'OWS',
  ",",
  ('OWS', 'connection-option'(H) ; ""),
  connection_options(T).
connection_options([]).



%! 'Content-Length'(?ContentLength:nonneg)// .
% ```abnf
% Content-Length = 1*DIGIT
% ```

'Content-Length'(N) --> +('DIGIT', N, [convert(1-positional)]).



%! 'HTTP-message(?X, ?Y:list(compound), ?Z:list(code))// .
% ```abnf
% HTTP-message = start-line *( header-field CRLF ) CRLF [ message-body ]
% ```

'HTTP-message'(X, Y, Z) -->
  'start-line'(X),
  header_fields(Y),
  'CRLF',
  ('message-body'(Z) ; {Z = []}).
header_fields([H|T]) --> 'header-field'(H), !, 'CRLF', header_fields(T).
header_fields([]) --> "".



%! 'HTTP-name'// .
% ```abnf
% HTTP-name = %x48.54.54.50 ; HTTP
% ```

'HTTP-name' --> "HTTP".



%! 'HTTP-version'(?Major:between(0,9), ?Minor:between(0,9))// .
% ```abnf
% HTTP-version = HTTP-name "/" DIGIT "." DIGIT
% ```

'HTTP-version'(X, Y) -->
  'HTTP-name',
  "/",
  'DIGIT'(X),
  ".",
  'DIGIT'(Y).



%! 'Host'(?Host, ?Port)// .
% ```abnf
% Host = uri-host [ ":" port ]
% ```

'Host'(Host, Port) --> 'uri-host'(Host), (":", port(Port) ; "").



%! 'obs-text'(?Code:code)// .
% ```abnf
% obs-text = %x80-FF
% ```

'obs-text'(C) --> between_code_radix(hex('80'), hex('FF'), C).



%! 'OWS'// .
% ```abnf
% OWS = *( SP / HTAB )
% ```

'OWS' --> 'SP', !, 'OWS'.
'OWS' --> 'HTAB', !, 'OWS'.
'OWS' --> "".



%! qdtext(?Code:code)// .
% ```abnf
% qdtext = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
% ```

qdtext(C) --> 'HTAB'(C).
qdtext(C) --> 'SP'(C).
qdtext(C) --> code_radix(hex(21), C).
qdtext(C) --> between_code_radix(hex(23), hex('5B'), C).
qdtext(C) --> between_code_radix(hex('5D'), hex('7E'), C).
qdtext(C) --> 'obs-text'(C).



%! 'quoted-pair(?Code:code)// .
% ```abnf
% quoted-pair = "\" ( HTAB / SP / VCHAR / obs-text )
% ```

'quoted-pair'(C) --> "\\", ('HTAB'(C) ; 'SP'(C) ; 'VCHAR'(C) ; 'obs-text'(C)).



%! 'quoted-string'(?String:string)// .
% ```abnf
% quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE
% ```

'quoted-string'(S) --> dcg_string(quoted_string, S).
quoted_string(Cs) --> 'DQUOTE', quoted_string_codes(Cs), 'DQUOTE'.
quoted_string_codes([H|T]) --> qdtext(H), !, quoted_string_codes(T).
quoted_string_codes([H|T]) --> 'quoted-pair'(H), !, quoted_string_codes(T).
quoted_string_codes([]) --> "".



%! 'RWS'// .
% ```abnf
% RWS = 1*( SP / HTAB )
% ```

'RWS' --> ('SP' ; 'HTAB'), 'OWS'.



%! tchar(?TokenCharacter:code)// .
% ```abnf
% tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*"
%       / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
%       / DIGIT / ALPHA
%       ; any VCHAR, except delimiters
% ```

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
tchar(C) --> 'DIGIT'(_, C).
tchar(C) --> 'ALPHA'(C).



%! token(?Token:string)// .
% ```abnf
% token = 1*tchar
% ```
%
% @compat RFC 7230

token(S) --> dcg_string(tchars, S).
tchars([H1,H2|T]) --> tchar(H1), !, tchars([H2|T]).
tchars([H]) --> tchar(H).
