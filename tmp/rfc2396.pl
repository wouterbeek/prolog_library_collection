:- module(
  rfc2396,
  [
    abs_path//1, % -Segments
    host//1,     % -Host
    port//1,     % -Port
    query//1     % -Query
  ]
).

/** <module> RFC 2396: Uniform Resource Identifiers (URI): Generic Syntax

@author Wouter Beek
@compat RFC 2396
@deprecated Use module `rfc3986' instead.
@see https://tools.ietf.org/html/rfc2396
@version 2017/05-2017/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg), except([
     alpha//1,
     alphanum//1,
     digit//1
   ])).
:- use_module(library(dict)).
:- use_module(library(math_ext)).
:- use_module(library(uri)).





%! abs_path(-Segments:list(atom))// is det.
%
% ```abnf
% abs_path = "/"  path_segments
% ```

abs_path(Segments) -->
  "/",
  path_segments(Segments).



%! absoluteURI(-Uri:compound)// is det.
%
% ```abnf
% absoluteURI = scheme ":" ( hier_part | opaque_part )
% ```

absoluteURI(uri(Scheme,Authority,Segments,Query,_)) -->
  scheme(Scheme),
  ":",
  (opaque_part(Segments) -> "" ; hier_part(Authority, Segments, Query)).



%! alpha(?Code:code)// .
%
% ```abnf
% alpha = lowalpha | upalpha
% ```

alpha(Code) -->
  lowalpha(Code).
alpha(Code) -->
  upalpha(Code).



%! alphanum(?Code:code)// .
%
% ```abnf
% alphanum = alpha | digit
% ```

alphanum(Code) -->
  alpha(Code).
alphanum(Code) -->
  digit(Code).



%! authority(-Authority:compound)// is det.
%
% ```abnf
% authority = server | reg_name
% ```

authority(Authority) -->
  reg_name(Authority), !.
authority(Authority) -->
  server(Authority).



%! delims(-Code:code)// .
%
% ```abnf
% delims = "<" | ">" | "#" | "%" | <">
% ```

delims(0'<) --> "<".
delims(0'>) --> ">".
delims(0'#) --> "#".
delims(0'%) --> "%".
delims(0'") --> "\"".



%! digit(-Code:code)// .
%
% ```abnf
% digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
% ```

digit(0'0) --> "0".
digit(0'1) --> "1".
digit(0'2) --> "2".
digit(0'3) --> "3".
digit(0'4) --> "4".
digit(0'5) --> "5".
digit(0'6) --> "6".
digit(0'7) --> "7".
digit(0'8) --> "8".
digit(0'9) --> "9".



%! domainlabel(-DomainLabel:atom)// is det.
%
% ```abnf
% domainlabel = alphanum | alphanum *( alphanum | "-" ) alphanum
% ```

domainlabel(DomainLabel) -->
  dcg_atom(domainlabel_1, DomainLabel).

domainlabel_1([H|T]) -->
  alphanum(H),
  domainlabel_2(T).
domainlabel_2([0'-,H|T]) -->
  "-", !,
  alphanum(H),
  domainlabel_2(T).
domainlabel_2([H|T]) -->
  alphanum(H), !,
  domainlabel_2(T).
domainlabel_2([]) --> "".



%! escaped(-Code:code)// .
%
% ```abnf
% escaped = "%" hex hex
% ```

escaped(Code) -->
  "%",
  hex(Weights1),
  hex(Weights2),
  {Code is Weights1 * 16 + Weights2}.



%! fragment(?Fragment:atom)// is det.
%
% ```abnf
% fragment = *uric
% ```

fragment(Fragment) -->
  dcg_atom(*(uric), Fragment).



%! hex(?Code:code)// .
%
% ```abnf
% hex = digit | "A" | "B" | "C" | "D" | "E" | "F"
%             | "a" | "b" | "c" | "d" | "e" | "f"
% ```

hex(Code) -->
  digit(Code).
hex(0'A) --> "A".
hex(0'B) --> "B".
hex(0'C) --> "C".
hex(0'D) --> "D".
hex(0'E) --> "E".
hex(0'F) --> "F".
hex(0'a) --> "a".
hex(0'b) --> "b".
hex(0'c) --> "c".
hex(0'd) --> "d".
hex(0'e) --> "e".
hex(0'f) --> "f".



%! hier_part(-Authority:compound, -Segments:list(atom),
%!           -Query:list(compound))// is det.
%
% ```abnf
% hier_part = ( net_path | abs_path ) [ "?" query ]
% ```

hier_part(Authority, Segments, Query) -->
  (net_path(Authority, Segments), ! ; abs_path(Segments)),
  ("?" -> query(Query) ; {var(Query)}).



%! host(-Host:atom)// is det.
%
% ```abnf
% host = hostname | IPv4address
% ```

host(Host) -->
  hostname(Host), !.
host(Host) -->
  'IPv4address'(Ns),
  {atomic_list_concat(Ns, ., Host)}.



%! hostname(-Labels:list(atom))// is det.
%
% ```abnf
% hostname = *( domainlabel "." ) toplabel [ "." ]
% ```

hostname(L) -->
  *(domainlabel_sep, L1),
  toplabel(X),
  ?("."),
  {append(L1, [X], L)}.

domainlabel_sep(C) -->
  domainlabel(C),
  ".".



%! hostport(-Host:atom, -Port:nonneg)// is det.
%
% ```abnf
% hostport = host [ ":" port ]
% ```

hostport(Host, Port) -->
  host(Host),
  (":" -> port(Port) ; {Port = 80}).



%! 'IPv4address'(-Address:list(nonneg))// is det.
%
% ```abnf
% IPv4address = 1*digit "." 1*digit "." 1*digit "." 1*digit
% ```

'IPv4address'([N1,N2,N3,N4]) -->
  % TBD: #(4, dcg_integer(+digit), Address)
  dcg_integer(+(digit), N1),
  ".",
  dcg_integer(+(digit), N2),
  ".",
  dcg_integer(+(digit), N3),
  ".",
  dcg_integer(+(digit), N4).



%! lowalpha(?Code:code)// .
%
% ```abnf
% lowalpha = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i"
%          | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r"
%          | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
% ```

lowalpha(0'a) --> "a".
lowalpha(0'b) --> "b".
lowalpha(0'c) --> "c".
lowalpha(0'd) --> "d".
lowalpha(0'e) --> "e".
lowalpha(0'f) --> "f".
lowalpha(0'g) --> "g".
lowalpha(0'h) --> "h".
lowalpha(0'i) --> "i".
lowalpha(0'j) --> "j".
lowalpha(0'k) --> "k".
lowalpha(0'l) --> "l".
lowalpha(0'm) --> "m".
lowalpha(0'n) --> "n".
lowalpha(0'o) --> "o".
lowalpha(0'p) --> "p".
lowalpha(0'q) --> "q".
lowalpha(0'r) --> "r".
lowalpha(0's) --> "s".
lowalpha(0't) --> "t".
lowalpha(0'u) --> "u".
lowalpha(0'v) --> "v".
lowalpha(0'w) --> "w".
lowalpha(0'x) --> "x".
lowalpha(0'y) --> "y".
lowalpha(0'z) --> "z".



%! mark(?Code:code)// .
%
% ```abnf
% mark = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"
% ```

mark(0'-) --> "-".
mark(0'_) --> "_".
mark(0'.) --> ".".
mark(0'!) --> "!".
mark(0'~) --> "~".
mark(0'*) --> "*".
mark(0'') --> "'".
mark(0'() --> "(".
mark(0')) --> ")".



%! net_path(-Authority:compound, -Segments:list(atom))// is det.
%
% ```abnf
% net_path = "//" authority [ abs_path ]
% ```

net_path(Authority, Segments) -->
  "//",
  authority(Authority),
  ?(abs_path(Segments)).



%! opaque_part(-Segments:atom)// is det.
%
% ```abnf
% opaque_part = uric_no_slash *uric
% ```

opaque_part([Segment]) -->
  uric_no_slash(H),
  *(uric, T), !,
  {atom_codes(Segment, [H|T])}.



%! param(-Parameter:atom)// is det.
%
% ```abnf
% param = *pchar
% ```

param(Parameter) -->
  dcg_atom(*(pchar), Parameter).



%! path(-Segments:list(atom))// is det.
%
% ```abnf
% path = [ abs_path | opaque_part ]
% ```

path(Segments) -->
  abs_path(Segments), !.
path(Segments) -->
  opaque_part(Segments), !.
path([]) --> "".



%! path_segments(-Segments:list(atom))// is det.
%
% ```abnf
% path_segments = segment *( "/" segment )
% ```

path_segments([H|T]) -->
  segment(H),
  *(sep_segment, T), !.

sep_segment(X) -->
  "/",
  segment(X).



%! pchar(?Code:code)// .
%
% ```abnf
% pchar = unreserved | escaped | ":" | "@" | "&" | "=" | "+" | "$" | ","
% ```

pchar(Code) -->
  unreserved(Code).
pchar(Code) -->
  escaped(Code).
pchar(0':) --> ":".
pchar(0'@) --> "@".
pchar(0'&) --> "&".
pchar(0'=) --> "=".
pchar(0'+) --> "+".
pchar(0'$) --> "$".
pchar(0',) --> ",".



%! port(-Port:nonneg)// is det.
%
% ````abnf
% port = *digit
% ```

port(Port) -->
  dcg_integer(*(digit), Port).



%! query(?Query:atom)// is det.
%
% ```abnf
% query = *uric
% ```

query(Query) -->
  dcg_atom(*(uric), Query).



%! reg_name(?Name:atom)// is det.
%
% ```abnf
% reg_name = 1*( unreserved | escaped | "$" | ","
%              | ";" | ":" | "@" | "&" | "=" | "+" )
% ```

reg_name(Name) -->
  dcg_atom(+(reg_name_), Name).

reg_name_(Code) -->
  unreserved(Code).
reg_name_(Code) -->
  escaped(Code).
reg_name_(0'$) --> "$".
reg_name_(0',) --> ",".
reg_name_(0';) --> ";".
reg_name_(0':) --> ":".
reg_name_(0'@) --> "@".
reg_name_(0'&) --> "&".
reg_name_(0'=) --> "=".
reg_name_(0'+) --> "+".



%! rel_path(-Authority:atom, -Segments:list(atom))// is det.
%
% ```abnf
% rel_path = rel_segment [ abs_path ]
% ```

rel_path(Authority, Segments) -->
  rel_segment(Authority),
  ?(abs_path(Segments)).



%! rel_segment(?Segment:atom)// is det.
%
% ```abnf
% rel_segment = 1*( unreserved | escaped
%             | ";" | "@" | "&" | "=" | "+" | "$" | "," )
% ```

rel_segment(Segment) -->
  dcg_atom(+(rel_segment_), Segment).

rel_segment_(Code) -->
  unreserved(Code).
rel_segment_(Code) -->
  escaped(Code).
rel_segment_(0';) --> ";".
rel_segment_(0'@) --> "@".
rel_segment_(0'&) --> "&".
rel_segment_(0'=) --> "=".
rel_segment_(0'+) --> "+".
rel_segment_(0'$) --> "$".
rel_segment_(0',) --> ",".



%! relativeURI(-Uri:compound)// is det.
%
% ```abnf
% relativeURI = ( net_path | abs_path | rel_path ) [ "?" query ]
% ```

relativeURI(uri(_,Authority,Segments,Query,_)) -->
  (   net_path(Authority, Segments)
  ->  ""
  ;   abs_path(Segments)
  ->  ""
  ;   rel_path(Authority, Segments)
  ),
  ("?" -> query(Query) ; "").



%! reserved(?Code:code)// .
%
% ```abfn
% reserved = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ","
% ```

reserved(0';) --> ";".
reserved(0'/) --> "/".
reserved(0'?) --> "?".
reserved(0':) --> ":".
reserved(0'@) --> "@".
reserved(0'&) --> "&".
reserved(0'=) --> "=".
reserved(0'+) --> "+".
reserved(0'$) --> "$".
reserved(0',) --> ",".



%! scheme(-Scheme:atom)// is det.
%
% ```abnf
% scheme = alpha *( alpha | digit | "+" | "-" | "." )
% ```

scheme(Scheme) -->
  alpha(H),
  *(scheme_, T),
  {atom_codes(Scheme, [H|T])}.

scheme_(Code) -->
  alpha(Code).
scheme_(Code) -->
  digit(Code).
scheme_(0'+) --> "+".
scheme_(0'-) --> "-".
scheme_(0'.) --> ".".



%! segment(-Segment:atom)// is det.
%
% ```abnf
% segment = *pchar *( ";" param )
% ```

segment(Segment) -->
  dcg_atom(*(pchar), H),
  *(sep_param, T),
  {atomic_list_concat([H|T], ;, Segment)}.

sep_param(Param) -->
  ";",
  param(Param).



%! server(-Authority:compound)// is det.
%
% ```abnf
% server = [ [ userinfo "@" ] hostport ]
% ```

server(auth(User,_Password,Host,Port)) -->
  (userinfo(User) -> "@" ; {var(User)}),
  hostport(Host, Port), !.
% @bug This seems to be a bug in the standard?
server(_) --> "".



%! space(?Code:code)// .
%
% ```abnf
% space = <US-ASCII coded character 20 hexadecimal>
% ```

space(0' ) -->
  [0x20].



%! toplabel(?TopLabel:atom)// is det.
%
% ```abnf
% toplabel = alpha | alpha *( alphanum | "-" ) alphanum
% ```

toplabel(Label) -->
  dcg_atom(toplabel_1, Label).

toplabel_1([H|T]) -->
  alpha(H),
  toplabel_2(T).

toplabel_2([0'-,H|T]) -->
  "-", !,
  alphanum(H),
  toplabel_2(T).
toplabel_2([H|T]) -->
  alphanum(H), !,
  toplabel_2(T).
toplabel_2([]) --> "".



%! upalpha(?Code:code)// .
%
% ```abnf
% upalpha = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
%         | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R"
%         | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
% ```

upalpha(0'A) --> "A".
upalpha(0'B) --> "B".
upalpha(0'C) --> "C".
upalpha(0'D) --> "D".
upalpha(0'E) --> "E".
upalpha(0'F) --> "F".
upalpha(0'G) --> "G".
upalpha(0'H) --> "H".
upalpha(0'I) --> "I".
upalpha(0'J) --> "J".
upalpha(0'K) --> "K".
upalpha(0'L) --> "L".
upalpha(0'M) --> "M".
upalpha(0'N) --> "N".
upalpha(0'O) --> "O".
upalpha(0'P) --> "P".
upalpha(0'Q) --> "Q".
upalpha(0'R) --> "R".
upalpha(0'S) --> "S".
upalpha(0'T) --> "T".
upalpha(0'U) --> "U".
upalpha(0'V) --> "V".
upalpha(0'W) --> "W".
upalpha(0'X) --> "X".
upalpha(0'Y) --> "Y".
upalpha(0'Z) --> "Z".



%! 'URI-reference'(-Uri:compound)// is det.
%
% ```abnf
% URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
% ```

'URI-reference'(uri(Scheme,Authority,Segments,Query,Fragment)) -->
  (   absoluteURI(uri(Scheme,Authority,Segments,Query))
  ->  ""
  ;   relativeURI(uri(Scheme,Authority,Segments,Query))
  ),
  ("#" -> fragment(Fragment) ; {var(Fragment)}).



%! unreserved(?Code:code)// .
%
% ```abnf
% unreserved = alphanum | mark
% ```

unreserved(Code) -->
  alphanum(Code).
unreserved(Code) -->
  mark(Code).



%! unwise(?Code:code)// .
%
% ```abnf
% unwise = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
% ```

unwise(0'{) --> "{".
unwise(0'}) --> "}".
unwise(0'|) --> "|".
unwise(0'\\) --> "\\".
unwise(0'^) --> "^".
unwise(0'[) --> "[".
unwise(0']) --> "]".
unwise(0'`) --> "`".



%! uric(?Code:code)// .
%
% RFC 1738 (URL) defines a reordering of this grammar rule under the
% name xchar//1.
%
% ```abnf
% uric = reserved | unreserved | escaped
% ```

uric(Code) -->
  reserved(Code).
uric(Code) -->
  unreserved(Code).
uric(Code) -->
  escaped(Code).



%! uric_no_slash(?Code:code)// .
%
% ```abnf
% uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@"
%               | "&" | "=" | "+" | "$" | ","
% ```

uric_no_slash(Code) -->
  unreserved(Code).
uric_no_slash(Code) -->
  escaped(Code).
uric_no_slash(0';) --> ";".
uric_no_slash(0'?) --> "?".
uric_no_slash(0':) --> ":".
uric_no_slash(0'@) --> "@".
uric_no_slash(0'&) --> "&".
uric_no_slash(0'=) --> "=".
uric_no_slash(0'+) --> "+".
uric_no_slash(0'$) --> "$".
uric_no_slash(0',) --> ",".



%! userinfo(?User:atom)// is det.
%
% ```abnf
% userinfo = *( unreserved | escaped
%          | ";" | ":" | "&" | "=" | "+" | "$" | "," )
% ```

userinfo(User) -->
  dcg_atom(*(userinfo_), User).

userinfo_(Code) -->
  unreserved(Code).
userinfo_(Code) -->
  escaped(Code).
userinfo_(0';) --> ";".
userinfo_(0':) --> ":".
userinfo_(0'&) --> "&".
userinfo_(0'=) --> "=".
userinfo_(0'+) --> "+".
userinfo_(0'$) --> "$".
userinfo_(0',) --> ",".
