:- module(
  record_jar,
  [
    'record-jar'//2 % ?Encoding, ?Records
  ]
).

/** <module> Record Jar

Support for the `record-jar` format for storing multiple records with a
variable repertoire of fields in a text format.

@author Wouter Beek
@see Originally described in *The Art of Unix Programming*.
@see http://tools.ietf.org/html/draft-phillips-record-jar-02
@see http://www.inter-locale.com/ID/draft-phillips-record-jar-01.html
@version 2015-2019
*/

:- use_module(library(dcg)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(math_ext)).





%! 'ASCCHAR'(?Code:code)// .
%
% ASCII characters except %x26 (&) and %x5C (\).
%
% ```abnf
% ASCCHAR = %x21-25 / %x27-5B / %x5D-7E
% ```

'ASCCHAR'(C) --> dcg_between(0x21, 0x25, C), !.
'ASCCHAR'(C) --> dcg_between(0x27, 0x5B, C), !.
'ASCCHAR'(C) --> dcg_between(0x5D, 0x7E, C).



%! 'blank-line'// .
%
% ```abnf
% blank-line = WSP CRLF
% ```

'blank-line' -->
  'WSP',
  eol.



%! character// .
%! character(?Code:code)// .
%
% ```abnf
% character = SP / ASCCHAR / UNICHAR / ESCAPE
% ```
%
% Note that ampersand// and backslash// are explicitly excluded.
%
% ## Inconsistency
%
% I assume the horizontal tab is also allowed in comments, as is space.

character -->
  character(_).


character(C) --> 'SP'(C), !.
character(C) --> 'ASCCHAR'(C), !.
character(C) --> 'UNICHAR'(C), !.
character(C) --> 'ESCAPE'(C).



%! comment// .
%
% ```abnf
% comment = SP *69(character)
% ```
%
% Notice that the horizontal tab is not allowed in comments,
% possibly to control the maximum width of comment lines.

comment -->
  'SP',
  '*n!'(69, character).



%! continuation// .
%
% ```abnf
% continuation = ["\"] [[*SP CRLF] 1*SP]
% ```
%
% @bug IANA does not follow record-jar correctly.  They use LF i.o. CRLF.

continuation -->
  ("\\" -> "" ; ""),
  (continuation_ -> "" ; "").

continuation_ -->
  (continuation__ -> "" ; ""),
  '+!'('SP').

continuation__ -->
  '*!'('SP'),
  eol.



%! encodingSig(?Encoding:string)// .
%
% ```abnf
% encodingSig = "%%encoding" field-sep *(ALPHA / DIGIT / "-" / "_") CRLF
% ```
%
% @bug IANA uses LF i.o. CRLF.

encodingSig(Enc) -->
  "%%encoding",
  'field-sep',
  dcg_atom_from_codes(encodingSig_, Enc),
  eol.

encodingSig_(C) --> alphanum(C).
encodingSig_(0'-) --> "-".
encodingSig_(0'_) --> "_".



%! 'ESCAPE'(?Code:code)// .
%
% ```abnf
% ESCAPE = "\" ("\" / "&" / "r" / "n" / "t" ) / "&#x" 2*6HEXDIG ";"
% ```

'ESCAPE'(C) -->
  "\\",
  escape(C), !.
'ESCAPE'(C) -->
  "&#x",
  'm*n!'(2, 6, 'HEXDIG', Ds),
  {integer_weights(C, 16, Ds)}.

escape(0'\\) --> "\\".
escape(0'&)  --> "&".
escape(0'r)  --> "r".
escape(0'n)  --> "n".
escape(0't)  --> "t".



%! field(?Field:npair(atom,list(string)))// .
%
% ```abnf
% field = ( field-name field-sep field-body CRLF )
% ```

field(Name-Body) -->
  'field-name'(Name),
  'field-sep',
  'field-body'(Body),
  eol.



%! 'field-body'(?Body:or([string,list(string)]))// .
%
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

'field-body'(L) -->
  '*!'(field_body_, L).

field_body_(String) -->
  continuation,
  dcg_string_from_codes(character, String),
  {String \== ""}.



%! 'field-name'(?Name:atom)// .
%
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
%
% ```
% Whitespace characters and colon (":", %x3A) are not permitted in a
% field-name.
% ```
%
% We therefore introduce the extra DCG rule field_name_character//1.

'field-name'(Atom) -->
  dcg_atom_from_codes(field_name_, Atom),
  {Atom \== ''}.

field_name_(_) --> 'SP', !, {fail}.
field_name_(_) --> ":", !, {fail}.
field_name_(C) --> character(C).



%! 'field-sep'// .
%
% The field separator is the colon character (=:=, =%x3A=).  The
% separator MAY be surrounded on either side by any amount of
% horizontal whitespace (tab or space characters). The normal
% convention is one space on each side.
%
% ```abnf
% field-sep = *SP ":" *SP
% ```

'field-sep' -->
  '*!'('SP'),
  ":",
  '*!'('SP').



%! record(?Fields:list(pair(atom,list(string))))// .
%
% ```abnf
% record = 1*field separator
% ```

record(Fields) -->
  '+!'(field, Fields),
  separator.



%! 'record-jar'(?Encoding:string, ?Records:list(list(pair(atom,list(string)))))// .
%
% ```abnf
% record-jar = [encodingSig] [separator] *record
% ```

'record-jar'(Enc, Records) -->
  default(encodingSig(Enc), {Enc = "UTF-8"}),
  (separator -> "" ; ""),
  '*!'(record, Records).



%! separator// .
%
% ```abnf
% separator = [blank-line] *("%%" [comment] CRLF)
% ```

separator -->
  ('blank-line' -> "" ; ""),
  '*!'(separator_).

separator_ -->
  "%%",
  (comment -> "" ; ""),
  eol.



%! 'UNICHAR'(?Code:code)// .
%
% ```abnf
% UNICHAR = %x80-10FFFF
% ```

'UNICHAR'(C) -->
  dcg_between(0x80, 0x10FFFF, C).





% HELPERS %

eol --> 'CRLF', !.
eol --> 'LF'.
