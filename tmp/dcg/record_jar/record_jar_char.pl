:- module(
  record_jar_char,
  [
    'ASCCHAR'//1, % ?Code:code
    'character@record-jar'//1, % ?Code:code
    'ESCAPE'//1, % ?Code:code
    'UNICHAR'//1 % ?Code:code
  ]
).

/** <module> Reocrd Jar characters

DCGs for characters that occur in the Record Jar representation format.

@author Wouter Beek
@version 2015/07
*/

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_code)).
:- use_module(plc(dcg/rfc5234)).
:- use_module(plc(math/positional)).





%! 'ASCCHAR'// .
%! 'ASCCHAR'(?Code:code)// .
% ASCII characters except %x26 (&) and %x5C (\).
%
% ```abnf
% ASCCHAR = %x21-25 / %x27-5B / %x5D-7E
% ```

'ASCCHAR'(C) --> between_code_radix(hex('21'), hex('25'), C).
'ASCCHAR'(C) --> between_code_radix(hex('27'), hex('5B'), C).
'ASCCHAR'(C) --> between_code_radix(hex('5D'), hex('7E'), C).



%! 'character@record-jar'(?Code:code)// .
% ```abnf
% character = SP / ASCCHAR / UNICHAR / ESCAPE
% ```
%
% Note that ampersand// and backslash// are explicitly excluded.
%
% ## Inconsistency
%
% I assume the horizontal tab is also allowed in comments, as is space.

'character@record-jar'(C) --> 'SP'(C).
'character@record-jar'(C) --> 'ASCCHAR'(C).
'character@record-jar'(C) --> 'UNICHAR'(C).
'character@record-jar'(C) --> 'ESCAPE'(C).



%! 'ESCAPE'(?Code:code)// .
% ```abnf
% ESCAPE = "\" ("\" / "&" / "r" / "n" / "t" ) / "&#x" 2*6HEXDIG ";"
% ```

'ESCAPE'(C) --> "\\", backslash_escape(C).
'ESCAPE'(C) --> "&#x", {clpfd_positional(C, Hexs)}, 'm*n'(2, 6, 'HEXDIG', Hexs, []).
backslash_escape(0'\) --> "\\".
backslash_escape(0'&) --> "&".
backslash_escape(0'r) --> "r".
backslash_escape(0'n) --> "n".
backslash_escape(0't) --> "t".



%! 'UNICHAR'(?Code:code)// .
% ```abnf
% UNICHAR = %x80-10FFFF
% ```

'UNICHAR'(C) -->
  between_code_radix(hex('80'), hex('10FFFF'), C).
