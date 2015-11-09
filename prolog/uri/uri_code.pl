:- module(
  uri_code,
  [
    'gen-delims'//1, % ?Code:code
    pchar//2, % ?Iri:boolean
              % ?Code:code
    'pct-encoded'//1, % ?Code:between(0,255)
    private//1, % ?Code:code
    reserved//1, % ?Code:code
    'sub-delims'//1, % ?Code:code
    ucschar//1, % ?Code:code
    unreserved//2 % ?Iri:boolean
                  % ?Code:code
  ]
).

/** <module> URI Syntax: Characters

Grammar for URI characters.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(uri/rfc3986)).





%! 'gen-delims'(?Code:code)// .
% ```abnf
% gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
% ```

'gen-delims'(0':) --> ":".
'gen-delims'(0'/) --> "/".
'gen-delims'(0'?) --> "?".
'gen-delims'(0'#) --> "#".
'gen-delims'(0'[) --> "[".
'gen-delims'(0']) --> "]".
'gen-delims'(0'@) --> "@".



%! pchar(?Iri:boolean, ?Code:code)// .
% ```abnf
%  pchar =  unreserved / pct-encoded / sub-delims / ":" / "@"
% ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
% ```

pchar(I, C  ) --> unreserved(I, C).
pchar(_, C  ) --> 'pct-encoded'(C).
pchar(_, C  ) --> 'sub-delims'(C).
pchar(_, 0':) --> ":".
pchar(_, 0'@) --> "@".



%! private(?Code:code)// .
% ```abnf
% iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
% ```

private(C) --> between_code_radix(hex('E000'),   hex('F8FF'),   C).
private(C) --> between_code_radix(hex('F0000'),  hex('FFFFD'),  C).
private(C) --> between_code_radix(hex('100000'), hex('10FFFD'), C).



%! reserved(?Code:code)// .
% ```abnf
% reserved = gen-delims / sub-delims
% ```

reserved(C) --> 'gen-delims'(C).
reserved(C) --> 'sub-delims'(C).



%! 'sub-delims'(?Code:code)// .
% ```abnf
% sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
% ```

'sub-delims'(0'!) --> "!".
'sub-delims'(0'$) --> "$".
'sub-delims'(0'&) --> "&".
'sub-delims'(0'') --> "'".
'sub-delims'(0'() --> "(".
'sub-delims'(0')) --> ")".
'sub-delims'(0'*) --> "*".
'sub-delims'(0'+) --> "+".
'sub-delims'(0',) --> ",".
'sub-delims'(0';) --> ";".
'sub-delims'(0'=) --> "=".



%! ucschar(?Code:code)// .
% ```abnf
% ucschar = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
%         / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
%         / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
%         / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
%         / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
%         / %xD0000-DFFFD / %xE1000-EFFFD
% ```

ucschar(C) --> between_code_radix(hex('A0'),    hex('D7FF'),  C).
ucschar(C) --> between_code_radix(hex('F900'),  hex('FDCF'),  C).
ucschar(C) --> between_code_radix(hex('FDF0'),  hex('FFEF'),  C).
ucschar(C) --> between_code_radix(hex('10000'), hex('1FFFD'), C).
ucschar(C) --> between_code_radix(hex('20000'), hex('2FFFD'), C).
ucschar(C) --> between_code_radix(hex('30000'), hex('3FFFD'), C).
ucschar(C) --> between_code_radix(hex('40000'), hex('4FFFD'), C).
ucschar(C) --> between_code_radix(hex('50000'), hex('5FFFD'), C).
ucschar(C) --> between_code_radix(hex('60000'), hex('6FFFD'), C).
ucschar(C) --> between_code_radix(hex('70000'), hex('7FFFD'), C).
ucschar(C) --> between_code_radix(hex('80000'), hex('8FFFD'), C).
ucschar(C) --> between_code_radix(hex('90000'), hex('9FFFD'), C).
ucschar(C) --> between_code_radix(hex('A0000'), hex('AFFFD'), C).
ucschar(C) --> between_code_radix(hex('B0000'), hex('BFFFD'), C).
ucschar(C) --> between_code_radix(hex('C0000'), hex('CFFFD'), C).
ucschar(C) --> between_code_radix(hex('D0000'), hex('DFFFD'), C).
ucschar(C) --> between_code_radix(hex('E1000'), hex('EFFFD'), C).



%! unreserved(?Iri:boolean, ?Code:code)// .
% ```abnf
%  unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
% iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
% ```

unreserved(_,    C  ) --> 'ALPHA'(C).
unreserved(_,    C  ) --> 'DIGIT'(_, C).
unreserved(_,    0'-) --> "-".
unreserved(_,    0'.) --> ".".
unreserved(_,    0'_) --> "_".
unreserved(_,    0'~) --> "~".
unreserved(true, C  ) --> ucschar(C).
