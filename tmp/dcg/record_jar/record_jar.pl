:- module(
  record_jar,
  [
    'record-jar'//2 % ?Encoding:atom
                    % ?Records:list(list(nvpair(atom,list(atom))))
  ]
).

/** <module> Record Jar

Support for the `record-jar` format for storing multiple records with a
 variable repertoire of fields in a text format.

## Syntax

```abnf
record-jar   = [encodingSig] [separator] *record
record       = 1*field separator
field        = ( field-name field-sep field-body CRLF )
field-name   = 1*character
field-sep    = *SP ":" *SP
field-body   = *(continuation 1*character)
continuation = ["\"] [[*SP CRLF] 1*SP]
separator    = [blank-line] *("%%" [comment] CRLF)
comment      = SP *69(character)
character    = SP / ASCCHAR / UNICHAR / ESCAPE
encodingSig  = "%%encoding" field-sep
                *(ALPHA / DIGIT / "-" / "_") CRLF
blank-line   = WSP CRLF

; ASCII characters except %x26 (&) and %x5C (\)
ASCCHAR      = %x21-25 / %x27-5B / %x5D-7E
; Unicode characters
UNICHAR      = %x80-10FFFF
ESCAPE       = "\" ("\" / "&" / "r" / "n" / "t" )
            / "&#x" 2*6HEXDIG ";"
```

---

@author Wouter Beek
@see Originally described in *The Art of Unix Programming*.
@see Latest description was found at
     http://tools.ietf.org/html/draft-phillips-record-jar-02
@version 2013/07, 2014/05-2014/06, 2014/10-2014/11, 2015/02
*/

:- use_module(library(apply)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_abnf_rules)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_meta)).
:- use_module(plc(dcg/record_jar/record_jar_char)).
:- use_module(plc(generics/atom_ext)). % Meta-option.





%! 'blank-line'// .
% ```abnf
% blank-line = WSP CRLF
% ```

'blank-line' -->
  'WSP',
  'CRLF', !.



%! comment// .
% ```abnf
% comment = SP *69(character)
% ```
%
% Notice that the horizontal tab is not allowed in comments,
%  possibly to control the maximum width of comment lines.

comment -->
  'SP',
  '*n'(69, 'character@record-jar', []).



%! continuation// .
% ```abnf
% continuation = ["\"] [[*SP CRLF] 1*SP]
% ```

continuation -->
  (   "\\"
  ;   ""
  ),
  (   (   '*SP',
          'CRLF'
      ;   ""
      ),
      '+SP'
  ;   ""
  ).



%! encodingSig(?Encoding:atom)// .
% ```abnf
% encodingSig = "%%encoding" field-sep *(ALPHA / DIGIT / "-" / "_") CRLF
% ```

encodingSig(Encoding) -->
  "%%encoding",
  'field-sep',
  dcg_atom('*'(encodingSig_char, []), Encoding),
  'CRLF', !.

encodingSig_char(Code) --> 'ALPHA'(Code).
encodingSig_char(Code) --> 'DIGIT'(_, Code).
encodingSig_char(Code) --> hyphen(Code).
encodingSig_char(Code) --> underscore(Code).



%! field(?Field:nvpair(atom,list(atom)))// .
% ```abnf
% field = ( field-name field-sep field-body CRLF )
% ```

field(Name=Body) -->
  'field-name'(Name),
  'field-sep',
  'field-body'(Body),
  'CRLF', !.



%! 'field-body'(?Body:list(atom))// .
% The field-body contains the data value. Logically, the field-body
% consists of a single line of text using any combination of characters
% from the Universal Character Set followed by a `CRLF` (newline).
%
% ### Escaping
%
% The carriage return, newline, and tab characters, when they occur in the
% data value stored in the field-body, are represented by their common
% backslash escapes (=\r=, =\n=, and =\t= respectively).
%
% ```abnf
% field-body = *(continuation 1*character)
% ```
%
% ### Folding
%
% To accommodate line limits (enforced by another standard or implementation),
% readability, and presentational purposes, the field-body portion of a field
% can be split into a multi-line representation; this is called *folding*.
%
% @tbd It is RECOMMENDED that folding not occur between characters inside a
%      Unicode grapheme cluster (since this will alter the display of
%      characters in the file and might result in unintentional alteration
%      of the file's semantics).
% @see Information on grapheme clusters, UAX29.

'field-body'(Sentence) -->
  '*'('field-body0', Words, []),
  {atomic_list_concat(Words, ' ', Sentence)}.

'field-body0'(Word) -->
  continuation,
  dcg_atom('+'('character@record-jar', []), Word).



%! 'field-name'(?Name:atom)// .
% The field-name is an identifer. Field-names consist of a sequence of
% Unicode characters. Whitespace characters and colon (=:=, =%x3A=) are
% not permitted in a field-name.
%
% ### Case
%
% Field-names are case sensitive.  Upper and lowercase letters are
% often used to visually break up the name, for example using
% CamelCase.  It is a common convention that field names use an initial
% capital letter, although this is not enforced.
%
% ```abnf
% field-name = 1*character
% ```
%
% ### Inconsistency
%
% The ABNF seems to be wrong. The working draft states the following:
% ```
% Whitespace characters and colon (":", %x3A) are not permitted in a
% field-name.
% ```
% We therefore introduce the extra DCG rule 'field-name-character'//1.

'field-name'(Name) -->
  dcg_atom('+'('field-name-character', []), Name).

'field-name-character'(_) --> 'SP', !, {fail}.
'field-name-character'(_) --> ":", !, {fail}.
'field-name-character'(Code) --> 'character@record-jar'(Code).



%! 'field-sep'// .
% The field separator is the colon character (=:=, =%x3A=).
% The separator MAY be surrounded on either side by any amount of
% horizontal whitespace (tab or space characters). The normal
% convention is one space on each side.
%
% ```abnf
% field-sep = *SP ":" *SP
% ```

'field-sep' -->
  '*SP',
  ":",
  '*SP'.



%! 'record-jar'(
%!   ?Encoding:atom,
%!   ?Records:list(list(nvpair(atom,list(atom))))
%! )// .
% ```abnf
% record-jar = [encodingSig] [separator] *record
% ```

'record-jar'(Encoding, Records) -->
  {flag(record_jar, _, 0)},
  (   encodingSig(Encoding)
  ;   {Encoding = 'UTF-8'}
  ),
  % The disjunction with the empty string is not needed here,
  % since the production of the separator can process
  % the empty string as well.
  (   separator
  ;   ""
  ),
  '*'(record, Records, []).



%! record(?Fields:list(nvpair(atom,list(atom))))// .
% ```abnf
% record = 1*field separator
% ```

record(Fields) -->
  '+'(field, Fields, []),
  separator,
  {
    flag(record_jar, X, X + 1),
    maplist(print_field(X), Fields)
  }.

print_field(X, Name=Value):-
  format(user_output, '[~D] ~a=~w\n', [X,Name,Value]).
  % @tbd Does not show in console!
  %%%%debug(record_jar, '[~D] ~a=~w', [X,Name,Body]).



%! separator// .
% ```abnf
% separator = [blank-line] *("%%" [comment] CRLF)
% ```

separator -->
  (   'blank-line'
  ;   ""
  ),
  '*'(separator0, []).

separator0 -->
  "%%",
  (   comment
  ;   ""
  ),
  'CRLF', !.





% HELPERS %

'+SP' -->
  'SP',
  '*SP'.

'*SP' --> "".
'*SP' -->
  'SP',
  '*SP'.
