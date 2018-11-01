:- module(
  xml_ext,
  [
    call_on_xml/3,      % +In, +Names, :Goal_1
    'Char'//1,          % +Version
    'Char'//2,          % +Version, ?Code
    load_xml/2,         % +Source, -Dom
    xml_encoding/2,     % +In, -Encoding
    xml_file_encoding/2 % +File, -Encoding
  ]
).

/** <module> XML extensions

@author Wouter Beek
@version 2016-2018
*/

:- use_module(library(apply)).
:- use_module(library(pure_input)).
:- use_module(library(sgml)).
:- use_module(library(yall)).

:- use_module(library(atom_ext)).
:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(stream_ext)).

:- meta_predicate
    call_on_xml(+, +, 1).





%! call_on_xml(+In:stream, +Names:list(atom), :Goal_1) is det.
%
% Call Goal_1 on an XML stream, where the argument supplied to Goal_1
% is a subtree that starts with an element called Name.

call_on_xml(In, Names, Goal_1) :-
  b_setval(xml_stream_record_names, Names),
  b_setval(xml_stream_goal, Goal_1),
  setup_call_cleanup(
    new_sgml_parser(Parser, []),
    (
      maplist(set_sgml_parser(Parser), [dialect(xml),space(remove)]),
      sgml_parse(Parser, [call(begin,on_begin_),source(In)])
    ),
    free_sgml_parser(Parser)
  ).

on_begin_(Name, Attr, Parser) :-
  b_getval(xml_stream_goal, Goal_1),
  b_getval(xml_stream_record_names, Names),
  memberchk(Name, Names), !,
  sgml_parse(Parser, [document(Dom),parse(content)]),
  (   call(Goal_1, [element(Name,Attr,Dom)])
  ->  true
  ;   print_message(warning, xml_error(element(Name,Attr,Dom)))
  ).



%! 'Char'(+Version:compound)// .
%! 'Char'(+Version:compound, ?Code:code)// .
%
% An **XML Character** is an atomic unit of text specified by ISO/IEC
% 10646.
%
% # XML 1.0
%
% ```ebnf
% Char ::= #x9                // Horizontal tab
%        | #xA                // Line feed
%        | #xD                // Carriage return
%        | [#x20-#xD7FF]      // Space, punctuation, numbers, letters
%        | [#xE000-#xFFFD]
%        | [#x10000-#x10FFFF]
% ```
%
% Avoid comapatibility characters [Unicode, section 2.3].  Avoid the
% following characters (control characters, permanently undefined
% Unicode characters):
%
% ```
% [#x7F-#x84] // Delete, ...
% [#x86-#x9F]
% [#xFDD0-#xFDEF],
% [#x1FFFE-#x1FFFF]
% [#x2FFFE-#x2FFFF]
% [#x3FFFE-#x3FFFF]
% [#x4FFFE-#x4FFFF]
% [#x5FFFE-#x5FFFF]
% [#x6FFFE-#x6FFFF]
% [#x7FFFE-#x7FFFF]
% [#x8FFFE-#x8FFFF]
% [#x9FFFE-#x9FFFF]
% [#xAFFFE-#xAFFFF]
% [#xBFFFE-#xBFFFF]
% [#xCFFFE-#xCFFFF]
% [#xDFFFE-#xDFFFF]
% [#xEFFFE-#xEFFFF]
% [#xFFFFE-#xFFFFF]
% [#x10FFFE-#x10FFFF]
% ```
%
% # XML 1.1
%
% ```ebnf
% Char ::= [#x1-#xD7FF]
%        | [#xE000-#xFFFD]
%        | [#x10000-#x10FFFF]
% /* any Unicode character, excluding the surrogate blocks,
%    FFFE, and FFFF. */
% ```
%
% @arg Version is either `version(1,0)' for XML 1.0 or `version(1,1)'
%      for XML 1.1.

'Char'(Version) -->
  'Char'(Version, _).


'Char'(version(1,0), 0x9) --> [0x9].
'Char'(version(1,0), 0xA) --> [0xA].
'Char'(version(1,0), 0xD) --> [0xD].
'Char'(version(1,0), Code) --> dcg_between(0x20, 0xD7FF, Code).
'Char'(version(1,0), Code) --> dcg_between(0xE000, 0xFFFD, Code).
'Char'(version(1,0), Code) --> dcg_between(0x10000, 0x10FFFF, Code).
'Char'(version(1,1), Code) --> dcg_between(0x1, 0xD7FF, Code).
'Char'(version(1,1), Code) --> dcg_between(0xE000, 0xFFFD, Code).
'Char'(version(1,1), Code) --> dcg_between(0x10000, 0x10FFFF, Code).



%! load_xml(+Source, -Dom:list(compound)) is det.

load_xml(Source, Dom) :-
  load_xml(Source, Dom, [space(remove)]).



%! xml_encoding(+In:stream, -Encoding:atom) is semidet.

xml_encoding(In, Encoding) :-
  phrase_from_stream(xml_encoding(Encoding0), In),
  nonvar(Encoding0),
  stream_ext:clean_encoding_(Encoding0, Encoding).

xml_encoding(Encoding) -->
  'XMLDecl'(_,Encoding,_),
  remainder(_).



%! xml_file_encoding(+File:atom, -Encoding:atom) is semidet.

xml_file_encoding(File, Encoding) :-
  read_from_file(File, {Encoding}/[In]>>xml_encoding(In, Encoding)).





% GRAMMAR %

%! 'EncName'(-Encoding:atom)// is det.
%
% ```bnf
% EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
% ```
%
% compat XML 1.0.5 [81]
% compat XML 1.1.2 [81]

'EncName'(Encoding) -->
  alpha(H),
  'enc_name_char*'(T),
  {atom_codes(Encoding, [H|T])}.

'enc_name_char*'([H|T]) -->
  enc_name_char(H), !,
  'enc_name_char*'(T).
'enc_name_char*'([]) --> "".

enc_name_char(Code) --> alphanum(Code).
enc_name_char(0'.) --> ".".
enc_name_char(0'_) --> "_".
enc_name_char(0'-) --> "-".



%! 'EncodingDecl'(-Encoding:atom)// .
%
% ```ebnf
% EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
% ```
%
% @compat XML 1.0.5 [80]
% @compat XML 1.1.2 [80]

'EncodingDecl'(Encoding) -->
  'S+',
  "encoding",
  'Eq',
  (   "\""
  ->  'EncName'(Encoding),
      must_see_code(0'")%"
  ;   "'"
  ->  'EncName'(Encoding),
      must_see_code(0'')
  ).



%! 'Eq'// is det.
%
% ```bnf
% Eq ::= S? '=' S?
% ```
%
% @compat XML 1.0.5 [25].
% @compat XML 1.1.2 [25].

'Eq' -->
  'S*',
  "=",
  'S*'.



%! 'S'// is det.
%
% Greedy white space.
%
% ```ebnf
% S ::= ( #x20 | #x9 | #xD | #xA )+ // Any consecutive number of spaces,
%                                   // carriage returns, line feeds, and
%                                   // horizontal tabs.
% ```
%
% The presence of carriage_return// in the above production is
% maintained purely for backward compatibility with the First Edition.
% All `#xD` characters literally present in an XML document are either
% removed or replaced by line_feed// (i.e., `#xA`) characters before
% any other processing is done.

'S' --> [0x20].
'S' --> [0x9].
'S' --> [0xD].
'S' --> [0xA].

'S+' -->
  'S',
  'S*'.

'S*' -->
  'S', !,
  'S*'.
'S*' --> "".



%! 'SDDecl'(-Standalone:boolean)// is det.
%
% Standalone Declaration
%
% ```ebnf
% SDDecl ::= S 'standalone' Eq
%            (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
% ```
%
% @compat XML 1.0.5 [32].
% @compat XML 1.1.2 [32].
% @tbd [VC: Standalone Document Declaration]

'SDDecl'(Standalone) -->
  'S+',
  "standalone",
  'Eq',
  (   "'"
  ->  yesno(Standalone),
      must_see_code(0'')
  ;   "\""
  ->  yesno(Standalone),
      must_see_code(0'")%"
  ).

yesno(true) -->"yes".
yesno(false) --> "no".



%! 'XMLDecl'(-Version:compound, -Encoding:atom, -Standalone:boolean)// is det.
%
% ```ebnf
% XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
% ```
%
% @compat XML 1.0.5 [23].
% @compat XML 1.1.2 [23].

'XMLDecl'(Version, Encoding, Standalone) -->
  "<?xml",
  'VersionInfo'(Version),
  ('EncodingDecl'(Encoding) -> "" ; ""),
  ('SDDecl'(Standalone) -> "" ; ""),
  'S*',
  "?>".



%! 'VersionInfo'(-Version:compound)// is det.
%
% ```ebnf
% VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
% ```
%
% @compat XML 1.0.5 [24].
% @compat XML 1.1.2 [24].

'VersionInfo'(Version) -->
  'S+',
  "version",
  'Eq',
  (   "'"
  ->  'VersionNum'(Version),
      "'"
  ;   "\""
  ->  'VersionNum'(Version),
      "\""
  ).



%! 'VersionNum'(-Version:compound)// is det.
%
% # XML 1.0
%
% ```bnf
% VersionNum ::= '1.' [0-9]+
% ```
%
% # XML 1.1
%
% ```bnf
% VersionNum ::= '1.1'
% ```
%
% @arg Version is a compound term of the form
%      `version(Major:nonneg,Minor:nonneg)'.
%
% @compat XML 1.0.5 [26].
% @compat XML 1.1.2 [26].

'VersionNum'(version(1,Minor)) -->
  "1.",
  integer(Minor).
'VersionNum'(version(1,1)) -->
  "1.1".





% HELPERS %

%! must_see_code(+Code:code)// is det.

must_see_code(Code) -->
  must_see_code(Code, 'S*').
