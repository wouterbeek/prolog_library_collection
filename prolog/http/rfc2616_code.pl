:- module(
  rfc2616_code,
  [
    'CHAR'//1, % ?Code:code
    ctext//1, % ?Code:code
    'LWS'//0,
    qdtext//1, % ?Code:code
    'quoted-pair'//1, % ?Code:code
    separators//1, % ?Code:code
    'TEXT'//1 % ?Code:code
  ]
).
:- reexport(
  library(url/rfc1738_code),
  [
    lowalpha//1 as 'LOALPHA', % ?Code:code
    hialpha//1 as 'UPALPHA' % ?Code:code
  ]
).
:- reexport(
  library(dcg/rfc2234),
  [
    'ALPHA'//1, % ?Code:code
    'CR'//0,
    'CRLF'//0,
    'CTL'//0,
    'CTL'//1, % ?Code:code
    'DIGIT'//1, % ?Weight:nonneg
    'DIGIT'//2, % ?Weight:nonneg
                % ?Code:code
    'DQUOTE'//0 as '"',
    'HEXDIG'//1 as 'HEX', % ?Weight:nonneg
    'HEXDIG'//2 as 'HEX', % ?Weight:nonneg
                     % ?Code:code
    'HTAB'//0 as 'HT',
    'HTAB'//1 as 'HT', % ?Code:code
    'LF'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1 % ?Code:code
  ]
).

/** <module> RFC 2616: Codes

@author Wouter Beek
@compat RFC 2616
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).





%! '"'// .
% Double quote.
%
% RFC 2234 (ABNF) defines this in a different but compatible way
% under the name 'DQUOTE'//[0,1].
%
% ```abnf
% <"> = <US-ASCII double-quote mark (34)>
% ```



%! 'ALPHA'(?Code:code)// .
% RFC 2234 (ABNF) defines this in a different but compatible way.
%
% ```abnf
% ALPHA = UPALPHA | LOALPHA
% ```



%! 'CHAR'(?Code:code)// .
% RFC 2234 (ABNF) defined this without the NULL character.
%
% ```abnf
% CHAR = <any US-ASCII character (octets 0 - 127)>
% ```

'CHAR'(C) --> between_code(0, 127, C).



%! ctext(?Code:code)// .
% ```abnf
% ctext = <any TEXT excluding "(" and ")">
% ```

ctext(C) --> 'TEXT'(C), {C \= 0'(, C \= 0')}.



%! 'CR'// .
% RFC 2234 (ABNF) defines this is in a different but compatible way.
%
% ```abnf
% CR = <US-ASCII CR, carriage return (13)>
% ```



%! 'CTL'// .
% RFC 2234 (ABNF) defines this in an alternative but compatible way.
%
% ```abnf
% CTL = <any US-ASCII control character (octets 0 - 31) and DEL (127)>
% ```



%! 'DIGIT'(?Weight:between(0,9))// .
%! 'DIGIT'(?Weight:between(0,9), ?Code:code)// .
% RFC 2234 (ABNF) defines this in an alternative but compatible way.
%
% ```abnf
% DIGIT = <any US-ASCII digit "0".."9">
% ```



%! 'HEX'(?Weight:between(0,15))// .
%! 'HEX'(?Weight:between(0,15), ?Code:code)// .
% Hexadecimal digit.
%
% RFC 1738 (URL) defines this is a different but compatible way
% under the name hex//2.
%
% ```abnf
% HEX = "A" | "B" | "C" | "D" | "E" | "F"
%     | "a" | "b" | "c" | "d" | "e" | "f"
%     | DIGIT
% ```



%! 'HT'// .
% RFC 2234 (ABNF) defines this in a different by compatible way
% under the name 'HTAB'//[0,1].
%
% ```abnf
% HT = <US-ASCII HT, horizontal-tab (9)>
% ```



%! 'LF'// .
% Line feed.
%
% RFC 2234 (ABNF) defines this in an alternative but compatible way.
% 
% ```abnf
% LF = <US-ASCII LF, linefeed (10)>
% ```



%! 'LOALPHA'(?Code:code)// .
% RFC 1738 (URL) defines this is a different but compatible way
% under the name lowalpha//1.
%
% ```abnf
% LOALPHA = <any US-ASCII lowercase letter "a".."z">
% ```



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
% RFC 2234 (ABNF) defines this in an alternative but compatible way.
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



%! 'SP'// .
% RFC 2234 (ABNF) defines this in an alternative but compatible way.
%
% ```abnf
% SP = <US-ASCII SP, space (32)>
% ```



%! separators(?Code:code)// .
% ```abnf
% separators = "(" | ")" | "<" | ">" | "@" | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "=" | "{" | "}" | SP | HT
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
