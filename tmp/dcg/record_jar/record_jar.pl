:- module(
  record_jar,
  [
    'blank-line'//0,
    'record-jar'//2 % ?Encoding:atom
                    % ?Records:list(list(nvpair(atom,list(atom))))
  ]
).

/** <module> Record Jar

Support for the `record-jar` format for storing multiple records with a
variable repertoire of fields in a text format.

@author Wouter Beek
@see Originally described in *The Art of Unix Programming*.
@see Latest description was found at
     http://tools.ietf.org/html/draft-phillips-record-jar-02
@version 2015/11
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/record_jar/record_jar_char)).
:- use_module(library(dcg/rfc2234)).





%! 'blank-line'// .
% ```abnf
% blank-line = WSP CRLF
% ```

'blank-line' --> 'WSP', 'CRLF'.



%! comment// .
% ```abnf
% comment = SP *69(character)
% ```
%
% Notice that the horizontal tab is not allowed in comments,
% possibly to control the maximum width of comment lines.

comment --> 'SP', '*n'(69, character, []).



%! continuation// .
% ```abnf
% continuation = ["\"] [[*SP CRLF] 1*SP]
% ```

continuation --> ("\\" ; ""), ((*('SP', []), 'CRLF' ; ""), +('SP', []) ; "").



%! encodingSig(?Encoding:string)// .
% ```abnf
% encodingSig = "%%encoding" field-sep *(ALPHA / DIGIT / "-" / "_") CRLF
% ```

encodingSig(Enc) -->
  "%%encoding",
  'field-sep',
  dcg_string(encodingSig_codes, Enc),
  'CRLF'.
encodingSig_codes([H|T]) --> 'ALPHA'(H), !, encodingSig_codes(T).
encodingSig_codes([H|T]) --> 'DIGIT'(_, H), !, encodingSig_codes(T).
encodingSig_codes([0'-|T]) --> "-", !, encodingSig_codes(T)
encodingSig_codes([0'_|T]) --> "_", !, encodingSig_codes(T)
encodingSig_codes([]) --> "".



%! field(?Field:nvpair(string,list(string)))// .
% ```abnf
% field = ( field-name field-sep field-body CRLF )
% ```

field(Name=Body) -->
  'field-name'(Name),
  'field-sep',
  'field-body'(Body),
  'CRLF'.



%! 'field-body'(?Body:list(string))// .
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

'field-body'([H|T]) -->
  continuation, !,
  *(character, H, [convert(1-string)]),
  'field-body'(T).
'field-body'([]) --> "".



%! 'field-name'(?Name:string)// .
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
  +(character0, Name, [convert(1-string)]).
character0(_) --> 'SP', !, {fail}.
character0(_) --> ":", !, {fail}.
character0(C) --> character(C).



%! 'field-sep'// .
% The field separator is the colon character (=:=, =%x3A=).
% The separator MAY be surrounded on either side by any amount of
% horizontal whitespace (tab or space characters). The normal
% convention is one space on each side.
%
% ```abnf
% field-sep = *SP ":" *SP
% ```

'field-sep' --> *('SP'), ":", *('SP').



%! 'record-jar'(
%!   ?Encoding:string,
%!   ?Records:list(list(nvpair(string,list(string))))
%! )// .
% ```abnf
% record-jar = [encodingSig] [separator] *record
% ```

'record-jar'(Enc, Records) -->
  {flag(record_jar, _, 0)},
  (encodingSig(Enc) ; {Enc = "UTF-8"}),
  % The disjunction with the empty string is not needed here,
  % since the production of the separator can process
  % the empty string as well.
  ?(separator),
  *(record, Records, []).



%! record(?Fields:list(nvpair(atom,list(atom))))// .
% ```abnf
% record = 1*field separator
% ```

record(Fields) -->
  '+'(field, Fields, []),
  separator,
  {debug(record_jar, "~a=~w~n", [Name,Value])}.



%! separator// .
% ```abnf
% separator = [blank-line] *("%%" [comment] CRLF)
% ```

separator --> ?('blank-line'), *(separator0).
separator0 --> "%%", ?(comment), 'CRLF'.
