:- module(
  record_jar_char,
  [
    'character@record-jar'//0,
    'character@record-jar'//1, % ?Code:code
    'ESCAPE'//0,
    'ESCAPE'//1, % ?Code:code
    'UNICHAR'//0,
    'UNICHAR'//1 % ?Code:code
  ]
).

/** <module> Reocrd Jar characters

DCGs for characters that occur in the Record Jar representation format.

@author Wouter Beek
@version 2015/07
*/

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_abnf_rules)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_code)).
:- use_module(plc(math/positional)).





%! 'ASCCHAR'// .
%! 'ASCCHAR'(?Code:code)// .
% ASCII characters except %x26 (&) and %x5C (\).
%
% ```abnf
% ASCCHAR = %x21-25 / %x27-5B / %x5D-7E
% ```

'ASCCHAR' -->
  'ASCCHAR'(_).

'ASCCHAR'(Code) -->
  between_code_radix(hex('21'), hex('25'), Code).
'ASCCHAR'(Code) -->
  between_code_radix(hex('27'), hex('5B'), Code).
'ASCCHAR'(Code) -->
  between_code_radix(hex('5D'), hex('7E'), Code).



%! 'character@record-jar'// .
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

'character@record-jar' -->
  'character@record-jar'(_).

'character@record-jar'(Code) --> 'SP'(Code).
'character@record-jar'(Code) --> 'ASCCHAR'(Code).
'character@record-jar'(Code) --> 'UNICHAR'(Code).
'character@record-jar'(Code) --> 'ESCAPE'(Code).



%! 'ESCAPE'// .
%! 'ESCAPE'(?Code:code)// .
% ```abnf
% ESCAPE = "\" ("\" / "&" / "r" / "n" / "t" ) / "&#x" 2*6HEXDIG ";"
% ```

'ESCAPE' -->
  'ESCAPE'(_).

'ESCAPE'(Code) -->
  "\\",
  (   backslash(Code)
  ;   ampersat(Code)
  ;   r_lowercase(Code)
  ;   n_lowercase(Code)
  ;   t_lowercase(Code)
  ).
'ESCAPE'(Code) -->
  "&#x",
  {clpfd_positional(Code, Hexs)},
  'm*n'(2, 6, 'HEXDIG', Hexs, []).



%! 'UNICHAR'// .
%! 'UNICHAR'(?Code:code)// .
% ```abnf
% UNICHAR = %x80-10FFFF
% ```

'UNICHAR' -->
  'UNICHAR'(_).

'UNICHAR'(Code) -->
  between_code_radix(hex('80'), hex('10FFFF'), Code).
