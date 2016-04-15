:- module(
  rfc2396,
  [
    abs_path//1, % -Path:list(string)
    host//1, % -Host
    port//1, % -Port:nonneg
    query//1 % -Query:string
  ]
).

/** <module> RFC 2396: Uniform Resource Identifiers (URI): Generic Syntax

@author Wouter Beek
@compat RFC 2396
@deprecated Use module `rfc3986' instead.
@see https://tools.ietf.org/html/rfc2396
@version 2015/11-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext), [
     '?'//1,
     '+'//2,
     '*'//2,
     dcg_string//2,
     pos_sum/2
   ]).
:- use_module(library(dict_ext)).
:- use_module(library(pair_ext)).





%! abs_path(-Path:list(string))// is det.
% ```abnf
% abs_path = "/"  path_segments
% ```

abs_path(L) --> "/", path_segments(L).



%! absoluteURI(-Uri:dict)// is det.
% ```abnf
% absoluteURI = scheme ":" ( hier_part | opaque_part )
% ```

absoluteURI(D) -->
  scheme(Scheme),
  ":",
  (   opaque_part(Opaque)
  ->  {D = absolute_uri{scheme: Scheme, opaque_part: Opaque}}
  ;   hier_part(D0),
      {
        dict_pairs(D0, hier_part, T),
        dict_pairs(D, absolute_uri, [scheme-Scheme|T])
      }
  ).



%! alpha(-Code:code)// .
% ```abnf
% alpha = lowalpha | upalpha
% ```

alpha(C) --> lowalpha(C).
alpha(C) --> upalpha(C).



%! alphanum(-Code:code)// .
% ```abnf
% alphanum = alpha | digit
% ```

alphanum(C) --> alpha(C).
alphanum(C) --> digit(C).



%! authority(-Authority:compound)// is det.
% ```abnf
% authority = server | reg_name
% ```

authority(authority{registered_name: RegisteredName}) -->
  reg_name(RegisteredName), !.
authority(authority{server: Server}) -->
  server(Server).



%! delims(-Code:code)// .
% ```abnf
% delims = "<" | ">" | "#" | "%" | <">
% ```

delims(0'<) --> "<".
delims(0'>) --> ">".
delims(0'#) --> "#".
delims(0'%) --> "%".
delims(0'") --> "\"".   %"



%! digit(-Code:code)// .
% ```
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



%! domainlabel(DomainLabel:string)// is det.
% ```abnf
% domainlabel = alphanum | alphanum *( alphanum | "-" ) alphanum
% ```

domainlabel(S) --> dcg_string(domainlabel_codes1, S).
domainlabel_codes1([H|T]) --> alphanum(H), domainlabel_codes2(T).
domainlabel_codes2([0'-,H|T]) --> "-", !, alphanum(H), domainlabel_codes2(T).
domainlabel_codes2([H|T]) --> alphanum(H), !, domainlabel_codes2(T).
domainlabel_codes2([]) --> "".



%! escaped(-Code:code)// .
% ```
% escaped = "%" hex hex
% ```

escaped(C) --> "%", hex(D1), hex(D2), {C is D1 * 16 + D2}.



%! fragment(-Fragment:string)// is det.
% ```abnf
% fragment = *uric
% ```

fragment(S) --> *(uric, Cs), {string_codes(S, Cs)}.



%! hex(-Code:code)// .
% ```abnf
% hex = digit | "A" | "B" | "C" | "D" | "E" | "F"
%             | "a" | "b" | "c" | "d" | "e" | "f"
% ```

hex(C)   --> digit(C).
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



%! hier_part(-HierarchicalPart:dict)// is det.
% ```abnf
% hier_part = ( net_path | abs_path ) [ "?" query ]
% ```

hier_part(D) -->
  (net_path(Auth, Path), ! ; abs_path(Path)),
  ("?" -> query(Query) ; ""),
  {
    exclude(pair_has_var_value, [authority-Auth,path-Path,query-Query], L),
    dict_pairs(D, hier_part, L)
  }.



%! host(-Host:dict)// is det.
% ```abnf
% host = hostname | IPv4address
% ```

host(host{hostname: Ls}) --> hostname(Ls), !.
host(host{ipv4address: Ns}) --> 'IPv4address'(Ns).



%! hostname(-Labels:list(string))// is det.
% ```abnf
% hostname = *( domainlabel "." ) toplabel [ "." ]
% ```

hostname(L) -->
  *(domainlabel_sep, L1),
  toplabel(X),
  ?("."),
  {append(L1, [X], L)}.
domainlabel_sep(C) --> domainlabel(C), ".".



%! hostport(-Host:dict, -Port:nonneg)// is det.
% ```abnf
% hostport = host [ ":" port ]
% ```

hostport(Host, Port) --> host(Host), (":" -> port(Port) ; {Port = 80}).



%! 'IPv4address'(-Address:list(nonneg))// .
% ```abnf
% IPv4address = 1*digit "." 1*digit "." 1*digit "." 1*digit
% ```

'IPv4address'([N1,N2,N3,N4]) -->
  +(digit, Ds1), {pos_sum(Ds1, N1)},
  ".",
  +(digit, Ds2), {pos_sum(Ds2, N2)},
  ".",
  +(digit, Ds3), {pos_sum(Ds3, N3)},
  ".",
  +(digit, Ds4), {pos_sum(Ds4, N4)}.



%! lowalpha(-Code:code)// .
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



%! mark(-Code:code)// .
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



%! net_path(-Authority:compound, -Path:list(list(string)))// is det.
% ```abnf
% net_path = "//" authority [ abs_path ]
% ```

net_path(Auth, Path) -->
  "//", authority(Auth), (abs_path(Path) -> "" ; {Path = []}).



%! opaque_part(OpaquePart:string)// is det.
% ```abnf
% opaque_part = uric_no_slash *uric
% ```

opaque_part(S) --> uric_no_slash(H), *(uric, T), {string_codes(S, [H|T])}.



%! param(-Parameter:string)// is det.
% ```abnf
% param = *pchar
% ```

param(S) --> *(pchar, Cs), {string_codes(S, Cs)}.



%! path(-Path:list(string))// is det.
% ```abnf
% path = [ abs_path | opaque_part ]
% ```

path(Path)         --> abs_path(Path), !.
path([OpaquePart]) --> opaque_part(OpaquePart), !.
path([])           --> "".



%! path_segments(-Segments:list(string))// is det.
% ```abnf
% path_segments = segment *( "/" segment )
% ```

path_segments([H|T]) --> segment(H), *(sep_segment, T).
sep_segment(X) --> "/", segment(X).



%! pchar(-Code:code)// .
% ```abnf
% pchar = unreserved | escaped | ":" | "@" | "&" | "=" | "+" | "$" | ","
% ```

pchar(C)   --> unreserved(C).
pchar(C)   --> escaped(C).
pchar(0':) --> ":".
pchar(0'@) --> "@".
pchar(0'&) --> "&".
pchar(0'=) --> "=".
pchar(0'+) --> "+".
pchar(0'$) --> "$".
pchar(0',) --> ",".



%! port(-Port:nonneg)// is det.
% ````abnf
% port = *digit
% ```

port(N) --> *(digit, Ds), {pos_sum(Ds, N)}.



%! query(-Query:string)// is det.
% ```abnf
% query = *uric
% ```

query(S) --> *(uric, Cs), {string_codes(S, Cs)}.



%! reg_name(-Name:string)// is det.
% ```abnf
% reg_name = 1*( unreserved | escaped | "$" | ","
%              | ";" | ":" | "@" | "&" | "=" | "+" )
% ```

reg_name(S) --> +(reg_name_code, Cs), {string_codes(S, Cs)}.
reg_name_code(C)   --> unreserved(C).
reg_name_code(C)   --> escaped(C).
reg_name_code(0'$) --> "$".
reg_name_code(0',) --> ",".
reg_name_code(0';) --> ";".
reg_name_code(0':) --> ":".
reg_name_code(0'@) --> "@".
reg_name_code(0'&) --> "&".
reg_name_code(0'=) --> "=".
reg_name_code(0'+) --> "+".



%! rel_path(-Path:list(string))// is det.
% ```abnf
% rel_path = rel_segment [ abs_path ]
% ```

rel_path([H|T]) --> rel_segment(H), (abs_path(T) -> "" ; {T = []}).



%! rel_segment(-Segment:string)// is det.
% ```abnf
% rel_segment = 1*( unreserved | escaped
%             | ";" | "@" | "&" | "=" | "+" | "$" | "," )
% ```

rel_segment(S) --> +(rel_segment_code, Cs), {string_codes(S, Cs)}.
rel_segment_code(C)   --> unreserved(C).
rel_segment_code(C)   --> escaped(C).
rel_segment_code(0';) --> ";".
rel_segment_code(0'@) --> "@".
rel_segment_code(0'&) --> "&".
rel_segment_code(0'=) --> "=".
rel_segment_code(0'+) --> "+".
rel_segment_code(0'$) --> "$".
rel_segment_code(0',) --> ",".



%! relativeURI(-Uri:dict)// .
% ```abnf
% relativeURI = ( net_path | abs_path | rel_path ) [ "?" query ]
% ```

relativeURI(D) -->
  (net_path(Auth, Path), ! ; abs_path(Path), ! ; rel_path(Path)),
  ("?" -> query(Query) ; ""),
  {
    exclude(pair_has_var_value, [authority-Auth,path-Path,query-Query], L),
    dict_pairs(D, relative_uri, L)
  }.



%! reserved(-Code:code)// .
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



%! scheme(-Scheme:string)// is det.
% ```abnf
% scheme = alpha *( alpha | digit | "+" | "-" | "." )
% ```

scheme(S) --> *(scheme_code, Cs), {string_codes(S, Cs)}.
scheme_code(C)   --> alpha(C).
scheme_code(C)   --> digit(C).
scheme_code(0'+) --> "+".
scheme_code(0'-) --> "-".
scheme_code(0'.) --> ".".



%! space(-Code:code)// .
% ```abnf
% space = <US-ASCII coded character 20 hexadecimal>
% ```

space(0' ) --> [0x20].



%! segment(-Segment:list(string))// .
% ```abnf
% segment = *pchar *( ";" param )
% ```

segment([H|T]) --> *(pchar, Cs), {string_codes(H, Cs)}, *(sep_param, T).
sep_param(S) --> ";", param(S).



%! server(-Server:dict)// is det.
% ```abnf
% server = [ [ userinfo "@" ] hostport ]
% ```

server(D) -->
  (userinfo(UserInfo) -> "@" ; ""),
  hostport(Host, Port), !,
  {
    exclude(pair_has_var_value, [host-Host,port-Port,userinfo-UserInfo], L),
    dict_pairs(D, server, L)
  }.
server(server{}) --> "".



%! toplabel(-TopLabel:string)// is det.
% ```abnf
% toplabel = alpha | alpha *( alphanum | "-" ) alphanum
% ```

toplabel(S) --> dcg_string(toplabel_codes1, S).
toplabel_codes1([H|T]) --> alpha(H), toplabel_codes2(T).
toplabel_codes2([0'-,H|T]) --> "-", !, alphanum(H), toplabel_codes2(T).
toplabel_codes2([H|T]) --> alphanum(H), !, toplabel_codes2(T).
toplabel_codes2([]) --> "".



%! upalpha(-Code:code)// .
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



%! 'URI-reference'(-Uri:dict)// is det.
% ```abnf
% URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
% ```

'URI-reference'(D) -->
  (absoluteURI(D0), ! ; relativeURI(D0)),
  ("#" -> fragment(Frag), {D = D0.put(fragment, Frag)} ; {D = D0}).



%! unreserved(-Code:code)// .
% ```abnf
% unreserved = alphanum | mark
% ```

unreserved(C) --> alphanum(C).
unreserved(C) --> mark(C).



%! unwise(-Code:code)// .
% ```abnf
% unwise = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
% ```

unwise(0'{)  --> "{".
unwise(0'})  --> "}".
unwise(0'|)  --> "|".
unwise(0'\\) --> "\\".
unwise(0'^)  --> "^".
unwise(0'[)  --> "[".
unwise(0'])  --> "]".
unwise(0'`)  --> "`".



%! uric(-Code:code)// .
% RFC 1738 (URL) defines a reordering of this grammar rule
% under the name xchar//1.
%
% ```abnf
% uric = reserved | unreserved | escaped
% ```

uric(C) --> reserved(C).
uric(C) --> unreserved(C).
uric(C) --> escaped(C).



%! uric_no_slash(-Code:code)// .
% ```abnf
% uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@"
%               | "&" | "=" | "+" | "$" | ","
% ```

uric_no_slash(C)   --> unreserved(C).
uric_no_slash(C)   --> escaped(C).
uric_no_slash(0';) --> ";".
uric_no_slash(0'?) --> "?".
uric_no_slash(0':) --> ":".
uric_no_slash(0'@) --> "@".
uric_no_slash(0'&) --> "&".
uric_no_slash(0'=) --> "=".
uric_no_slash(0'+) --> "+".
uric_no_slash(0'$) --> "$".
uric_no_slash(0',) --> ",".



%! userinfo(-Userinfo:string)// is det.
% ```abnf
% userinfo = *( unreserved | escaped
%          | ";" | ":" | "&" | "=" | "+" | "$" | "," )
% ```

userinfo(S) --> *(userinfo_code, Cs), {string_codes(S, Cs)}.
userinfo_code(C)   --> unreserved(C).
userinfo_code(C)   --> escaped(C).
userinfo_code(0';) --> ";".
userinfo_code(0':) --> ":".
userinfo_code(0'&) --> "&".
userinfo_code(0'=) --> "=".
userinfo_code(0'+) --> "+".
userinfo_code(0'$) --> "$".
userinfo_code(0',) --> ",".
