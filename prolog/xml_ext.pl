:- module(
  xml_ext,
  [
    call_on_xml/3,      % +In, +Names, :Goal_1
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
      maplist(set_sgml_parser(Parser), [space(remove)]),
      sgml_parse(Parser, [call(begin,on_begin_),source(In)])
    ),
    free_sgml_parser(Parser)
  ).

on_begin_(Name, Attr, Parser) :-
  b_getval(xml_stream_goal, Goal_1),
  b_getval(xml_stream_record_names, Names),
  memberchk(Name, Names), !,
  sgml_parse(Parser, [document(Dom1),parse(content)]),
  convlist(xml_clean_dom, Dom1, Dom2),
  (   call(Goal_1, [element(Name,Attr,Dom2)])
  ->  true
  ;   print_message(warning, xml_error(element(Name,Attr,Dom2)))
  ).

xml_clean_dom(element(Name,Attr,Dom1), element(Name,Attr,Dom2)) :- !,
  convlist(xml_clean_dom, Dom1, Dom2).
% 1. Strip all leading and trailing blanks.
% 2. Remove elements that only contain blanks.
xml_clean_dom(H1, H2) :-
  atom_strip(H1, H2),
  H2 \== ''.



%! load_xml(+Source, -Dom:list(compound)) is det.
%
% @see Wrapper around load_xml/3 with default options.

load_xml(Source, Dom) :-
  load_xml(Source, Dom, []).



%! xml_encoding(+In:stream, -Encoding:atom) is semidet.

xml_encoding(In, Encoding) :-
  phrase_from_stream(xml_encoding(Encoding0), In),
  nonvar(Encoding0),
  clean_encoding(Encoding0, Encoding).

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
