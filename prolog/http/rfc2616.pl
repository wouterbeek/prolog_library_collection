:- module(
  rfc2616,
  [
    '"'//1, % ?Code:code
    'CHAR'//1, % ?Code:code
    comment//0,
    ctext//1, % ?Code:code
    'HEX'//1, % ?Weight:between(0,15)
    'HEX'//2, % ?Weight:between(0,15)
              % ?Code:code
    http_URL//1, % ?Url:atom
    'HTTP-Version'//2, % ?Major:nonneg
                       % ?Minor:nonneg
    'LOALPHA'//1, % ?Code:code
    'LWS'//0,
    qdtext//1, % ?Code:code
    'quoted-pair'//1, % ?Code:code
    'quoted-string'//1, % ?String:string
    separators//1, % ?Code:code
    'TEXT'//1, % ?Code:code
    token//1, % ?Token:string
    'UPALPHA'//1, % ?Code:code
    value//1 % ?Value:string
  ]
).
:- reexport(
  library(dcg/rfc2234),
  [
    'ALPHA'//1, % ?Code:code
    'CR'//1, % ?Code:code
    'CRLF'//0,
    'CTL'//1, % ?Code:code
    'DIGIT'//1, % ?Weight:between(0,9)
    'DIGIT'//2, % ?Weight:between(0,9)
                % ?Code:code
    'DQUOTE'//1 as '"', % ?Code:code
    'HTAB'//0 as 'HT', % ?Code:code
    'HTAB'//1 as 'HT', % ?Code:code
    'LF'//1, % ?Code:code
    'OCTET'//1, % ?Code:code
    'SP'//1 % ?Code:code
  ]
).    

/** <module> RFC 2616: Hypertext Transfer Protocol -- HTTP/1.1

@author Wouter Beek
@compat RFC 2616
@deprecated Use module `library(http/rfcXXX)` instead.
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(uri)).





%! '"'(?Code:code)// .
% Double quote.
%
% The ABNF RFCs define this in a different but compatible way
% under the name 'DQUOTE'//[0,1].
%
% ```abnf
% <"> = <US-ASCII double-quote mark (34)>
% ```



%! 'ALPHA'(?Code:code)// .
% ## Alternative definition
%
% The ABNF RFCs define this in a different but compatible way.
%
% ```abnf
% ALPHA = UPALPHA | LOALPHA
% ```



%! 'CHAR'(?Code:code)// .
% The ABNF RFCs define this without the NULL character.
%
% ```abnf
% CHAR = <any US-ASCII character (octets 0 - 127)>
% ```

'CHAR'(C) --> between_code(0, 127, C).



%! comment//0 .
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment -->
  "(", comment0, ")".
comment0 --> ctext(_), comment0.
comment0 --> 'quoted-pair'(_), comment0.
comment0 --> comment, comment0.
comment0 --> "".



%! ctext(?Code:code)// .
% ```abnf
% ctext = <any TEXT excluding "(" and ")">
% ```

ctext(C) --> 'TEXT'(C), {C \= 0'(, C \= 0')}.



%! 'CR'(?Code:code)// .
% THe ABNF RFCs define this is in a different but compatible way.
%
% ```abnf
% CR = <US-ASCII CR, carriage return (13)>
% ```



%! 'CTL'(?Code:code)// .
% The ABNF RFCs define this in an alternative but compatible way.
%
% ```abnf
% CTL = <any US-ASCII control character (octets 0 - 31) and DEL (127)>
% ```



%! 'DIGIT'(?Weight:between(0,9))// .
%! 'DIGIT'(?Weight:between(0,9), ?Code:code)// .
% The ABNF RFCs define this in an alternative but compatible way.
%
% ```abnf
% DIGIT = <any US-ASCII digit "0".."9">
% ```



%! 'HEX'(?Weight:between(0,15))// .
%! 'HEX'(?Weight:between(0,15), ?Code:code)// .
% Case-sensitive notation for hexadecimal digits.
%
% ```abnf
% HEX = "A" | "B" | "C" | "D" | "E" | "F"
%     | "a" | "b" | "c" | "d" | "e" | "f"
%     | DIGIT
% ```

'HEX'(W) --> 'HEX'(W, _).
'HEX'(10, 0'A) --> "A".
'HEX'(11, 0'B) --> "B".
'HEX'(12, 0'C) --> "C".
'HEX'(13, 0'D) --> "D".
'HEX'(14, 0'E) --> "E".
'HEX'(15, 0'F) --> "F".
'HEX'(10, 0'a) --> "a".
'HEX'(11, 0'b) --> "b".
'HEX'(12, 0'c) --> "c".
'HEX'(13, 0'd) --> "d".
'HEX'(14, 0'e) --> "e".
'HEX'(15, 0'f) --> "f".
'HEX'(W, C) --> 'DIGIT'(W, C).



%! 'HT'// .
%! 'HT'(?Code:code)// .
% The ABNF RFCs define this in a different by compatible way
% under the name 'HTAB'//[0,1].
%
% ```abnf
% HT = <US-ASCII HT, horizontal-tab (9)>
% ```



%! http_URL(?Url:atom)// .
% Port defaults to 80.
% Path defaults to `/`.
%
% ```abnf
% http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
% ```

http_URL(Url) -->
  "http://",
  host(Host),
  (":", port(Port) ; {Port = 80}),
  (abs_path(Path), ("?", query(Query) ; "") ; {Path = '/'}),
  {
    uri_authority_components(Auth, uri_components(_, _, Host, Port)),
    uri_components(Url, uri_components(http, Auth, Path, Query, _))
  }.



%! 'HTTP-Version'(?Major:nonneg, ?Minor:nonneg)// .
% ```abnf
% HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ```

'HTTP-Version'(X, Y) -->
  "HTTP/",
  +('DIGIT', X, [convert(1-positional)]),
  ".",
  +('DIGIT', Y, [convert(1-positional)]).



%! 'LF'(?Code:code)// .
% Line feed.
%
% The ABNF RFCs define this in an alternative but compatible way.
% 
% ```abnf
% LF = <US-ASCII LF, linefeed (10)>
% ```



%! 'LOALPHA'(?Code:code)// .
% ```abnf
% LOALPHA = <any US-ASCII lowercase letter "a".."z">
% ```

'LOALPHA'(0'a) --> "a".
'LOALPHA'(0'b) --> "b".
'LOALPHA'(0'c) --> "c".
'LOALPHA'(0'd) --> "d".
'LOALPHA'(0'e) --> "e".
'LOALPHA'(0'f) --> "f".
'LOALPHA'(0'g) --> "g".
'LOALPHA'(0'h) --> "h".
'LOALPHA'(0'i) --> "i".
'LOALPHA'(0'j) --> "j".
'LOALPHA'(0'k) --> "k".
'LOALPHA'(0'l) --> "l".
'LOALPHA'(0'm) --> "m".
'LOALPHA'(0'n) --> "n".
'LOALPHA'(0'o) --> "o".
'LOALPHA'(0'p) --> "p".
'LOALPHA'(0'q) --> "q".
'LOALPHA'(0'r) --> "r".
'LOALPHA'(0's) --> "s".
'LOALPHA'(0't) --> "t".
'LOALPHA'(0'u) --> "u".
'LOALPHA'(0'v) --> "v".
'LOALPHA'(0'w) --> "w".
'LOALPHA'(0'x) --> "x".
'LOALPHA'(0'y) --> "y".
'LOALPHA'(0'z) --> "z".



%! 'LWS'// .
% Linear white space.
%
% ```abnf
% LWS = [CRLF] 1*(SP|HT)
% ```

'LWS' --> ?('CRLF'), +(lws, []).
lws --> 'SP'.
lws --> 'HT'.



%! 'OCTET'(?Code:code)// .
% The ABNF RFCs defines this in an alternative but compatible way.
%
% ```abnf
% OCTET = <any 8-bit sequence of data>
% ```



%! qdtext(?Code:code)// .
% ```abnf
% qdtext = <any TEXT except <">>
% ```

qdtext(C) --> 'TEXT'(C), {\+ char_code('"', C)}.



%! 'quoted-pair'(?Code:code)// .
% ```abnf
% quoted-pair = "\" CHAR
% ```

'quoted-pair'(C) --> "\\", 'CHAR'(C).



%! 'quoted-string'(?Code:code)// .
% ```abnf
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ```

'quoted-string'(S) --> "\"", dcg_string(quoted_string, S), "\"".
quoted_string([H|T]) --> qdtext(H), !, quoted_string(T).
quoted_string([H|T]) --> 'quoted-pair'(H), !, quoted_string(T).
quoted_string([]) --> "".



%! 'SP'(?Code:code)// .
% The ABNF RFCs define this in an alternative but compatible way.
%
% ```abnf
% SP = <US-ASCII SP, space (32)>
% ```



%! separators(?Code:code)// .
% ```abnf
% separators = "(" | ")" | "<" | ">" | "@"
%            | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "="
%            | "{" | "}" | SP | HT
% ```

separators(0'() --> "(".
separators(0')) --> ")".
separators(0'<) --> "<".
separators(0'>) --> ">".
separators(0'@) --> "@".
separators(0',) --> ",".
separators(0';) --> ";".
separators(0':) --> ":".
separators(0'\\) --> "\\".
separators(0'") --> "\"". %"
separators(0'/) --> "/".
separators(0'[) --> "[".
separators(0']) --> "]".
separators(0'?) --> "?".
separators(0'=) --> "=".
separators(0'{) --> "{".
separators(0'}) --> "}".
separators(C) --> 'SP'(C).
separators(C) --> 'HT'(C).



%! 'TEXT'(?Code:code)// .
% Used for descriptive field contents and values that are
% not intended to be interpreted by the message parser.
% Words of `*TEXT` MAY contain characters from character sets other than
% ISO-8859-1 only when encoded according to the rules of RFC 2047.
%
% ```abnf
% TEXT = <any OCTET except CTLs, but including LWS>
% ```

'TEXT'(C) --> 'OCTET'(C), {\+ 'CTL'(C, _, _)}.



%! token(?Token:string)// .
% ```abnf
% token = 1*<any CHAR except CTLs or separators>
% ```

token(S) --> dcg_string(token_char, S).
token_char(C) --> 'CHAR'(C), {\+ 'CTL'(C, _, _), \+ separators(C, _, _)}.



%! 'UPALPHA'(?Code:code)// .
% ```abnf
% UPALPHA = <any US-ASCII uppercase letter "A".."Z">
% ```

'UPALPHA'(0'A) --> "A".
'UPALPHA'(0'B) --> "B".
'UPALPHA'(0'C) --> "C".
'UPALPHA'(0'D) --> "D".
'UPALPHA'(0'E) --> "E".
'UPALPHA'(0'F) --> "F".
'UPALPHA'(0'G) --> "G".
'UPALPHA'(0'H) --> "H".
'UPALPHA'(0'I) --> "I".
'UPALPHA'(0'J) --> "J".
'UPALPHA'(0'K) --> "K".
'UPALPHA'(0'L) --> "L".
'UPALPHA'(0'M) --> "M".
'UPALPHA'(0'N) --> "N".
'UPALPHA'(0'O) --> "O".
'UPALPHA'(0'P) --> "P".
'UPALPHA'(0'Q) --> "Q".
'UPALPHA'(0'R) --> "R".
'UPALPHA'(0'S) --> "S".
'UPALPHA'(0'T) --> "T".
'UPALPHA'(0'U) --> "U".
'UPALPHA'(0'V) --> "V".
'UPALPHA'(0'W) --> "W".
'UPALPHA'(0'X) --> "X".
'UPALPHA'(0'Y) --> "Y".
'UPALPHA'(0'Z) --> "Z".



%! value(?Value:string)// .
% ```abnf
% value = token | quoted-string
% ```

value(S) --> token(S).
value(S) --> 'quoted-string'(S).
