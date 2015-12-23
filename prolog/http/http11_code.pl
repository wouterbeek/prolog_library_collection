:- module(
  http11_code,
  [
    'CHAR'//1, % ?Code:code
    'LWS'//0,
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

/** <module> HTTP 1.1: Codes

@author Wouter Beek
@compat RFC 2616
@deprecated
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).





%! 'CHAR'(?Code:code)// .
% RFC 2234 (ABNF) defined this without the NULL character.
%
% ```abnf
% CHAR = <any US-ASCII character (octets 0 - 127)>
% ```

'CHAR'(C) --> [C], {between(0, 127, C)}.



%! 'LWS'// .
% Linear white space.
%
% ```abnf
% LWS = [CRLF] 1*(SP|HT)
% ```

'LWS' --> ?('CRLF'), +(lws).
lws --> 'SP'.
lws --> 'HT'.



%! separators(?Code:code)// .
% ```abnf
% separators = "(" | ")" | "<" | ">" | "@" | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "=" | "{" | "}" | SP | HT
% ```

separators(0'()  --> "(".
separators(0'))  --> ")".
separators(0'<)  --> "<".
separators(0'>)  --> ">".
separators(0'@)  --> "@".
separators(0',)  --> ",".
separators(0';)  --> ";".
separators(0':)  --> ":".
separators(0'\\) --> "\\".
separators(0'")  --> "\"". %"
separators(0'/)  --> "/".
separators(0'[)  --> "[".
separators(0'])  --> "]".
separators(0'?)  --> "?".
separators(0'=)  --> "=".
separators(0'{)  --> "{".
separators(0'})  --> "}".
separators(C)    --> 'SP'(C).
separators(C)    --> 'HT'(C).



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
