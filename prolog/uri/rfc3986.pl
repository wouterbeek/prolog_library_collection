:- module(
  rfc3986,
  [
    'absolute-URI'//1, % -AbsoluteUri:dict
    authority//1, % -Authority:dict
    fragment//1, % -Fragment:string
    host//1, % -Host:dict
    'path-abempty'//1, % -Segments:list(string)
    port//1, % -Port:nonneg
    query//1, % -Query:string
    'relative-part'//1, % -RelativeUri:dict
    segment//1, % -Segment:string
    'URI-reference'//1 % -UriReference:dict
  ]
).

/** <module> RFC 3986

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2015/11-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'DIGIT'//1, % ?Weight:between(0,9)
     'HEXDIG'//1 % ?Weight:between(0,9)
   ]).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).





%! 'absolute-URI'(-AbsoluteUri:dict)// is det.
% ```abnf
% absolute-URI = scheme ":" hier-part [ "?" query ]
% ```

'absolute-URI'(D) -->
  scheme(Scheme),
  ":",
  'hier-part'(D0),
  {dict_pairs(D0, hier_part, T1)},
  ("?" -> query(Query), {T2 = [query-Query|T1]} ; {T2 = T1}),
  {dict_pairs(D, absolute_uri, [scheme-Scheme|T2])}.



%! authority(-Authority:dict)// is det.
% # Syntax
%
% ```abnf
% authority = [ userinfo "@" ] host [ ":" port ]
% ```
%
% If the user info occurs, it is separated from the host with an ampesat.
% If the port occurs, it is separated from the host with a colon.
%
% # Semantics
%
% The authority component determines who has the right to respond
% authoritatively to requests that target the identified resource.

authority(D) -->
  (userinfo(UserInfo) -> "@" ; ""),
  host(Host),
  (":" -> port(Port) ; ""),
  {
    exclude(pair_has_var_value, [host-Host,port-Port,userinfo-UserInfo], L),
    dict_pairs(D, authority, L)
  }.



%! 'dec-octet'(-Octet:between(0,255))// is det.
% ```abnf
% dec-octet = DIGIT               ; 0-9
%           / %x31-39 DIGIT       ; 10-99
%           / "1" 2DIGIT          ; 100-199
%           / "2" %x30-34 DIGIT   ; 200-249
%           / "25" %x30-35        ; 250-255
% ```

'dec-octet'(D) -->
  "25",
  [C], {between(0x30, 0x35, C)}, !,
  {D3 is C - 0x30, pos_sum([2,5,D3], D)}.
'dec-octet'(D) -->
  "2",
  [C], {between(0x30, 0x34, C), !, D2 is C - 0x30},
  'DIGIT'(D3), !,
  {pos_sum([2,D2,D3], D)}.
'dec-octet'(D) -->
  "1",
  #(2, 'DIGIT', [D2,D3]), !,
  {pos_sum([1,D2,D3], D)}.
'dec-octet'(D) -->
  [C], {between(0x31, 0x39, C), D1 is C - 0x30},
  'DIGIT'(D2),
  {D is D1 * 10 + D2}.
'dec-octet'(D) -->
  'DIGIT'(D).



%! fragment(-Fragment:string)// is det.
% ```anbf
% fragment = *( pchar / "/" / "?" )
% ```

fragment(S) --> *(fragment_code, Cs), {string_codes(S, Cs)}.
fragment_code(C)   --> pchar(C).
fragment_code(0'/) --> "/".
fragment_code(0'?) --> "?".



%! 'gen-delims'(?Code:code)// .
% ```abnf
% gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
% ```

'gen-delims'(0':) --> ":".
'gen-delims'(0'/) --> "/".
'gen-delims'(0'?) --> "?".
'gen-delims'(0'#) --> "#".
'gen-delims'(0'[) --> "[".
'gen-delims'(0']) --> "]".
'gen-delims'(0'@) --> "@".



%! h16(-Number:between(0,65535))// is det.
% 16-bit hexadecimal.
%
% ```abnf
% h16 = 1*4HEXDIG
% ```

h16(N) --> 'm*n'(1, 4, 'HEXDIG', Ds), {pos_sum(Ds, N)}.



%! 'hier-part'(-HierarchicalPart:dict)// is det.
% ```abnf
% hier-part = "//" authority path-abempty
%           / path-absolute
%           / path-rootless
%           / path-empty
% ```

'hier-part'(hier_part{authority: Auth, path: L}) -->
  "//", !,
  authority(Auth),
  'path-abempty'(L).
'hier-part'(hier_part{path: L}) -->
  'path-absolute'(L), !.
'hier-part'(hier_part{path: L}) -->
  'path-rootless'(L), !.
'hier-part'(hier_part{path: L}) -->
  'path-empty'(L).



%! host(-Host:dict)// is det.
% A host denotes a physical machine that is connected to the Internet.
%
% If the host identifier is provided as an IP address, the origin
% server is the listener (if any) on the indicated TCP port at that IP
% address.
%
% If host is a registered name, the registered name is an indirect
% identifier for use with a name resolution service, such as DNS,
% to find an address for that origin server.
%
% ```abnf
% host = IP-literal / IPv4address / reg-name
% ```

host(Host) --> 'IP-literal'(Host), !.
host(Host) --> 'IPv4address'(Host), !.
host(Host) --> 'reg-name'(Host).



%! 'IP-literal'(?IpLiteral:compound)// is det.
% ```abnf
% IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
% ```

'IP-literal'(Lit) --> "[", ('IPv6address'(Lit), ! ; 'IPvFuture'(Lit)), "]".



%! 'IPv4address'(?Address:list(between(0,255)))// is det.
% ```abnf
% IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
% ```

'IPv4address'([I1,I2,I3,I4]) -->
  'dec-octet'(I1), ".", 'dec-octet'(I2), ".", 'dec-octet'(I3), ".", 'dec-octet'(I4), ".".



%! 'IPv6address'(-Ipv6Address:list)// is det.
% ```abnf
% IPv6address =                            6( h16 ":" ) ls32
%             /                       "::" 5( h16 ":" ) ls32
%             / [               h16 ] "::" 4( h16 ":" ) ls32
%             / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
%             / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
%             / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
%             / [ *4( h16 ":" ) h16 ] "::"              ls32
%             / [ *5( h16 ":" ) h16 ] "::"    h16
%             / [ *6( h16 ":" ) h16 ] "::"
% ```

'IPv6address'(L) -->                                           #(6, 'h16:', L2), ls32(Z), {append(L2, [Z], L)}.
'IPv6address'(L) -->                                     "::", #(5, 'h16:', L2), ls32(Z), {append(L2, [Z], L)}.
'IPv6address'(L) --> (                     h16(X) ; ""), "::", #(4, 'h16:', L2), ls32(Z), {append([X|L2], [Z], L)}.
'IPv6address'(L) --> ('*n'(1, 'h16:', L1), h16(X) ; ""), "::", #(3, 'h16:', L2), ls32(Z), {append([L1,[X|L2],[Z]], L)}.
'IPv6address'(L) --> ('*n'(2, 'h16:', L1), h16(X) ; ""), "::", #(2, 'h16:', L2), ls32(Z), {append([L1,[X|L2],[Z]], L)}.
'IPv6address'(L) --> ('*n'(3, 'h16:', L1), h16(X) ; ""), "::", h16(Y), ":",      ls32(Z), {append(L1, [X,Y,Z], L)}.
'IPv6address'(L) --> ('*n'(4, 'h16:', L1), h16(X) ; ""), "::",                   ls32(Z), {append(L1, [X,Z], L)}.
'IPv6address'(L) --> ('*n'(5, 'h16:', L1), h16(X) ; ""), "::", h16(Y),                    {append(L1, [X,Y], L)}.
'IPv6address'(L) --> ('*n'(6, 'h16:', L1), h16(X) ; ""), "::",                            {append(L1, [X], L)}.
'h16:'(I) --> h16(I), ":".



%! 'IPvFuture'(-IpLiteral:compound)// is det.
% ```abnf
% IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
% ```

'IPvFuture'(ipvfuture(I,S)) -->
  "v",
  +('HEXDIG', Ds),
  {pos_sum(Ds, 16, I)},
  ".",
  +(ipv_future_code, Cs),
  {string_codes(S, Cs)}.
ipv_future_code(C)   --> unreserved(C).
ipv_future_code(C)   --> 'sub-delims'(C).
ipv_future_code(0':) --> ":".



%! ls32(-Address:compound)// is det.
% ```abnf
% ls32 = ( h16 ":" h16 ) / IPv4address
% ```

ls32(ipa([H16a,H16b])) --> h16(H16a), !, ":", h16(H16b).
ls32(ipv4(Address))    --> 'IPv4address'(Address).



%! path(-Segments:list(string))// is det.
% ```abnf
% path =  path-abempty    ; begins with "/" or is empty
%      /  path-absolute   ; begins with "/" but not "//"
%      /  path-noscheme   ; begins with a non-colon segment
%      /  path-rootless   ; begins with a segment
%      /  path-empty      ; zero characters
% ```

% Begins with "/" or is empty.
path(L) --> 'path-abempty'(L), !.
% Begins with "/" but not "//".
path(L) --> 'path-absolute'(L), !.
% Begins with a non-colon segment
path(L) --> 'path-noscheme'(L), !.
% Begins with a segment
path(L) --> 'path-rootless'(L), !.
% Empty path (i.e., no segments).
path(L) --> 'path-empty'(L).



%! 'path-abempty'(-Segments:list(string))// is det.
% ```abnf
% path-abempty = *( "/" segment )
% ```

'path-abempty'(L) --> *(sep_segment, L).



%! 'path-absolute'(-Segments:list(string))// is det.
% ```abnf
% path-absolute = "/" [ segment-nz *( "/"  segment ) ]
% ```

'path-absolute'([H|T]) -->
  "/", ('segment-nz'(H) -> *(sep_segment, T) ; {T = []}).



%! 'path-empty'(-Segments:list(string))// is det.
% ```abnf
% path-empty = 0<pchar>
% ```

'path-empty'([]) --> "".



%! 'path-noscheme'(-Segments:list(string))// is det.
% ```abnf
% path-noscheme = segment-nz-nc *( "/"  segment )
% ```

'path-noscheme'([H|T]) --> 'segment-nz-nc'(H), *(sep_segment, T).



%! 'path-rootless'(-Segments:list(string))// is det.
% ```abnf
% path-rootless = segment-nz *( "/"  segment )
% ```

'path-rootless'([H|T]) --> 'segment-nz'(H), *(sep_segment, T).



%! pchar(?Code:code)// .
% ```abnf
% pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
% ```

pchar(C)   --> unreserved(C).
pchar(C)   --> 'pct-encoded'(C).
pchar(C)   --> 'sub-delims'(C).
pchar(0':) --> ":".
pchar(0'@) --> "@".



%! 'pct-encoded'(?Code:between(0,255))// .
% Similar to escape//1 in RFC 1738 which also supports
% uppercase hexadecimal letters.
%
% ```abnf
% pct-encoded = "%" HEXDIG HEXDIG
% ```

'pct-encoded'(C) --> "%", 'HEXDIG'(H1), 'HEXDIG'(H2), {C is H1 * 16 + H2}.



%! port(-Port:nonneg)// is det.
% ```abnf
% port = *DIGIT
% ```

port(N) --> *('DIGIT', Ds), {pos_sum(Ds, N)}.



%! query(-Query:string)// is det.
% ```abnf
% query = *( pchar / "/" / "?" )
% ```

query(S) --> *(query_code, Cs), {string_codes(S, Cs)}.
query_code(C)   --> pchar(C).
query_code(0'/) --> "/".
query_code(0'?) --> "?".



%! 'reg-name'(?RegisteredName:string)// is det.
% ```abnf
% reg-name = *( unreserved / pct-encoded / sub-delims )
% ```

'reg-name'(S) --> *(reg_name_code, Cs), {string_codes(S, Cs)}.
reg_name_code(C) --> unreserved(C).
reg_name_code(C) --> 'pct-encoded'(C).
reg_name_code(C) --> 'sub-delims'(C).



%! 'relative-part'(-RelativeUri:dict)// is det.
% ```abnf
% relative-part = "//" authority path-abempty
%               / path-absolute
%               / path-noscheme
%               / path-empty
% ```

'relative-part'(relative_part{authority: Auth, path: L}) -->
  "//", !, authority(Auth), 'path-abempty'(L).
'relative-part'(relative_part{path: L}) --> 'path-absolute'(L), !.
'relative-part'(relative_part{path: L}) --> 'path-noscheme'(L), !.
'relative-part'(relative_part{path: L}) --> 'path-empty'(L).



%! 'relative-ref'(-RelativeUri:dict)// is det.
% Relative URI reference.
%
% ```abnf
% relative-ref = relative-part [ "?" query ] [ "#" fragment ]
% ```

'relative-ref'(D) -->
  'relative-part'(D0),
  {dict_pairs(D0, relative_part, T)},
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Frag) ; ""),
  {
    exclude(pair_has_var_value, [fragment-Frag,query-Query|T], L),
    dict_pairs(D, relative_ref, L)
  }.



%! reserved(?Code:code)// .
% ```abnf
% reserved = gen-delims / sub-delims
% ```

reserved(C) --> 'gen-delims'(C).
reserved(C) --> 'sub-delims'(C).



%! scheme(-Scheme:string)// is det.
% An US-ASCII letter, followed by a sequence consisting of
% US-ASCII letters, digits, plus, dash, and dot characters.
%
% ```abnf
% scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
% ```

scheme(S) --> alpha(H), *(scheme_code, T), {string_codes(S, [H|T])}.
scheme_code(C)   --> alpha(C).
scheme_code(C)   --> digit(_, C).
scheme_code(0'+) --> "+".
scheme_code(0'-) --> "-".
scheme_code(0'.) --> ".".



%! segment(-Segment:string)// is det.
% ```abnf
% segment = *pchar
% ```

segment(S) --> *(pchar, Cs), {string_codes(S, Cs)}.



%! 'segment-nz'(-Segment:string)// is det.
% Non-empty (nz = non-zero?) segment.
%
% ```abnf
% segment-nz = 1*pchar
% ```

'segment-nz'(S) --> +(pchar, Cs), {string_codes(S, Cs)}.



%! 'segment-nz-nc'(-Segment:string)// is det.
% Non-zero-length segment without any colon.
%
% ```abnf
% segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
% ```

'segment-nz-nc'(S) --> +(segment_nz_nc_code, Cs), {string_codes(S, Cs)}.
segment_nz_nc_code(C)   --> unreserved(C).
segment_nz_nc_code(C)   --> 'pct-encoded'(C).
segment_nz_nc_code(C)   --> 'sub-delims'(C).
segment_nz_nc_code(0'@) --> "@".



%! 'sub-delims'(?Code:code)// .
% ```abnf
% sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
% ```

'sub-delims'(0'!) --> "!".
'sub-delims'(0'$) --> "$".
'sub-delims'(0'&) --> "&".
'sub-delims'(0'') --> "'".
'sub-delims'(0'() --> "(".
'sub-delims'(0')) --> ")".
'sub-delims'(0'*) --> "*".
'sub-delims'(0'+) --> "+".
'sub-delims'(0',) --> ",".
'sub-delims'(0';) --> ";".
'sub-delims'(0'=) --> "=".



%! unreserved(?Code:code)// .
% ```abnf
% unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
% ```

unreserved(C)   --> alphadigit(C).
unreserved(0'-) --> "-".
unreserved(0'.) --> ".".
unreserved(0'_) --> "_".
unreserved(0'~) --> "~".



%! 'URI'(-Uri:dict)// is det.
% ```abnf
% URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
% ```

'URI'(D) -->
  scheme(Scheme),
  ":",
  'hier-part'(D0),
  {dict_pairs(D0, hier_part, T)},
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Frag) ; ""),
  {
    exclude(pair_has_var_value, [fragment-Frag,query-Query,scheme-Scheme|T], L),
    dict_pairs(D, uri, L)
  }.



%! 'URI-reference'(-UriReference:dict)// is det.
% ```abnf
% URI-reference = URI / relative-ref
% ```

'URI-reference'(Uri) --> 'URI'(Uri).
'URI-reference'(Uri) --> 'relative-ref'(Uri).



%! userinfo(-UserInfo:string)// is det.
%
% # Syntax
%
% ```abnf
% userinfo = *( unreserved / pct-encoded / sub-delims / ":" )
% ```
%
% # Semantics
%
% This subcomponent may consist of a user name and, optionally,
% scheme-specific information about how to gain authorization to access
% the denoted resource.
%
% # Security
%
% Applications that render a URI for the sake of user feedback, such as
% in graphical hypertext browsing, should render userinfo in a way that
% is distinguished from the rest of a URI, when feasible.  Such
% rendering will assist the user in cases where the userinfo has been
% misleadingly crafted to look like a trusted domain name
%
% # Legacy
%
% Use of the format `user:password` in the userinfo field is deprecated.
% The passing of authentication information in clear text has proven to be
% a security risk.

userinfo(S) --> *(userinfo_code, Cs), {string_codes(S, Cs)}.
userinfo_code(C)   --> unreserved(C).
userinfo_code(C)   --> 'pct-encoded'(C).
userinfo_code(C)   --> 'sub-delims'(C).
userinfo_code(0':) --> ":".





% HELPERS %

sep_segment(S) --> "/", segment(S).
