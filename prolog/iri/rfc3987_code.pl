:- module(
  rfc3987_code,
  [
    ipchar//1, % ?Code:code
    iprivate//1, % ?Code:code
    iunreserved//1, % ?Code:code
    ucschar//1 % ?Code:code
  ]
).

/** <module> RFC 3987: Codes

@author Wouter Beek
@compat RFC 3987
@see http://tools.ietf.org/html/rfc3987
@version 2015/08, 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_code)).
:- use_module(library(uri/rfc3986)).





%! ipchar(?Code:code)// .
% ```abnf
% ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
% ```

ipchar(C) --> iunreserved(C).
ipchar(C) --> 'pct-encoded'(C).
ipchar(C) --> 'sub-delims'(C).
ipchar(0':) --> ":".
ipchar(0'@) --> "@".



%! iprivate(?Code:code)// .
% ```abnf
% iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
% ```

iprivate(C) --> between_code_radix(hex('E000'),   hex('F8FF'),   C).
iprivate(C) --> between_code_radix(hex('F0000'),  hex('FFFFD'),  C).
iprivate(C) --> between_code_radix(hex('100000'), hex('10FFFD'), C).



%! iunreserved(?Code:code)// .
% ```abnf
% iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
% ```

iunreserved(C) --> unreserved(C).
iunreserved(C) --> ucschar(C).



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
