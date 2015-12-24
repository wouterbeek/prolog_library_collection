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
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_rfc)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dict_ext)).





%! abs_path(-Path:list(string))// .
% ```abnf
% abs_path = "/"  path_segments
% ```

abs_path(L) --> "/", path_segments(L).



%! absoluteURI(-Uri:dict)// is det.
% ```abnf
% absoluteURI = scheme ":" ( hier_part | opaque_part )
% ```

absoluteURI(absolute_uri{scheme: Scheme, opaque: Opaque}) -->
  scheme(Scheme),
  ":",
  opaque_part(Opaque), !.
absoluteURI(Uri) -->
  scheme(Scheme),
  ":",
  hier_part(Auth, Path, Query),
  {dict_remove_uninstantiated(
    absolute_uri{scheme: Scheme, authority: Auth, path: Path, query: Query},
    Uri
  )}.



%! authority(-Authority:compound)// .
% ```abnf
% authority = server | reg_name
% ```

authority(authority{registered_name: RegisteredName}) --> reg_name(RegisteredName), !.
authority(authority{server: Server}) --> server(Server).



%! delims(-Code:code)// .
% ```abnf
% delims = "<" | ">" | "#" | "%" | <">
% ```

delims(0'<)  --> "<".
delims(0'>)  --> ">".
delims(0'#)  --> "#".
delims(0'%)  --> "%".
delims(0'\") --> "\"".



%! domainlabel(DomainLabel:string)// .
% ```abnf
% domainlabel = alphanum | alphanum *( alphanum | "-" ) alphanum
% ```

domainlabel(S) --> dcg_string(domainlabel_codes1, S).
domainlabel_codes1([H|T]) --> alphadigit(H), domainlabel_codes2(T).
domainlabel_codes2([0'-,H|T]) --> "-", !, alphadigit(H), domainlabel_codes2(T).
domainlabel_codes2([H|T]) --> alphadigit(H), !, domainlabel_codes2(T).
domainlabel_codes2([]) --> "".



%! fragment(-Fragment:string)// .
% ```abnf
% fragment = *uric
% ```

fragment(S) --> *(uric, Cs), {string_codes(S, Cs)}.



%! hier_part(-Authority:compound, -Path:list(string), -Query:string)// .
% ```abnf
% hier_part = ( net_path | abs_path ) [ "?" query ]
% ```

hier_part(Auth, Path, Query) -->
  (net_path(Auth, Path), ! ; abs_path(Path)),
  ("?" -> query(Query) ; "").



%! host(-Host:or([list(nonneg),list(string)]))// .
% ```abnf
% host = hostname | IPv4address
% ```

host(Ls) --> hostname(Ls), !.
host(Ns) --> 'IPv4address'(Ns).



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



%! hostport(-Host:or([list(nonneg),list(string)]), -Port:nonneg)// .
% ```abnf
% hostport = host [ ":" port ]
% ```

hostport(Host, Port) --> host(Host), (":" -> port(Port) ; {Port = 80}).



%! 'IPv4address'(-Address:list(nonneg))// .
% ```abnf
% IPv4address = 1*digit "." 1*digit "." 1*digit "." 1*digit
% ```

'IPv4address'([N1,N2,N3,N4]) -->
  '+digit'(N1), ".", '+digit'(N2), ".", '+digit'(N3), ".", '+digit'(N4), ".".



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



%! net_path(-Authority:compound, -Path:list(list(string))// .
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



%! relativeURI(-Uri:dict)// .
% ```abnf
% relativeURI = ( net_path | abs_path | rel_path ) [ "?" query ]
% ```

relativeURI(D) -->
  (net_path(Auth, Path), ! ; abs_path(Path), ! ; rel_path(Path)),
  ("?" -> query(Query) ; ""),
  {dict_remove_uninstantiated(
    relative_uri{authority: Auth, path: Path, query: Query},
    D
  )}.



%! segment(-Segment:list(string))// .
% ```abnf
% segment = *pchar *( ";" param )
% ```

segment([H|T]) --> *(pchar, Cs), {string_codes(H, Cs)}, *(sep_param, T).
sep_param(S) --> ";", param(S).



%! server(
%!   -UserInfo:string,
%!   -Host:or([list(nonneg),list(string)]),
%!   -Port:nonneg
%! )// .
% ```abnf
% server = [ [ userinfo "@" ] hostport ]
% ```

server(server{userinfo: UserInfo, host: Host, port: Port}) -->
  (userinfo(UserInfo) -> "@" ; ""), !,
  hostport(Host, Port).
server(_, _, _) --> "".



%! toplabel(-TopLabel:string)// is det.
% ```abnf
% toplabel = alpha | alpha *( alphanum | "-" ) alphanum
% ```

toplabel(S) --> dcg_string(toplabel_codes1, S).
toplabel_codes1([H|T]) --> alpha(H), toplabel_codes2(T).
toplabel_codes2([0'-,H|T]) --> "-", !, alphadigit(H), toplabel_codes2(T).
toplabel_codes2([H|T]) --> alphadigit(H), !, toplabel_codes2(T).
toplabel_codes2([]) --> "".



%! 'URI-reference'(-Uri:dict)// is det.
% ```abnf
% URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
% ```

'URI-reference'(Uri2) -->
  (absoluteURI(Uri1), ! ; relativeURI(Uri1)),
  ("#" -> fragment(Frag), {Uri2 = Uri1.put(fragment, Frag)} ; {Uri2 = Uri1}).



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



%! upalpha(-Code:code)// .
% RFC 1738 (URL) defines this under the name hialpha//1.
%
% ```abnf
% upalpha = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
%         | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R"
%         | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
% ```



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
