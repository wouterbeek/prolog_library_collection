:- module(
  rfc3986,
  [
    'absolute-URI'//1, % -Uri:compound
    authority//1, % -Auth:compound
    fragment//1, % -Frag:atom
    host//1, % -Host:atom
    'IP-literal'//2, % -Version:nonneg, -Address:list(nonneg)
    'IPv4address'//1, % -Address:list(between(0,255))
    'path-abempty'//1, % -Segments:list(atom)
    'pct-encoded'//1, % -Code:between(0,255)
    port//1, % -Port:nonneg
    query//1, % -Query:atom
    'relative-part'//2, % -Auth:compound, -Segments:list(atom)
    scheme//1, % -Scheme:atom
    scheme_code//1, % -Code:code
    segment//1, % -Segment:atom
    'segment-nz'//1, % -Segment:atom
    'sub-delims'//1 , % -Code:code
    unreserved//1, % -Code:code
    'URI'//1, % -Uri:compound
    'URI-reference'//1 % -Uri:compound
  ]
).

/** <module> RFC 3986

The following terms are used:

| Auth | auth(User,host,Port)             |
| Host | IP or atom Name                  |
| IP   | ip(Version,Address)              |
| Uri  | uri(Scheme,Auth,Segments,Query,Frag) |

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2015/11-2016/01, 2016/11
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'DIGIT'//1, % ?Weight:between(0,9)
     'HEXDIG'//1 % ?Weight:between(0,9)
   ]).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(uri/uri_ext)).





%! 'absolute-URI'(-Uri:compound)// is det.
%
% ```abnf
% absolute-URI = scheme ":" hier-part [ "?" query ]
% ```

'absolute-URI'(uri(Scheme,Auth,Segments,Query,_)) -->
  scheme(Scheme),
  ":",
  'hier-part'(Auth, Segments),
  ("?" -> query(Query) ; "").



%! authority(-Auth:compound)// is det.
%
% # Syntax
%
% ```abnf
% authority = [ userinfo "@" ] host [ ":" port ]
% ```
%
% If the user info occurs, it is separated from the host with an
% ampesat.  If the port occurs, it is separated from the host with a
% colon.
%
% # Semantics
%
% The authority component determines who has the right to respond
% authoritatively to requests that target the identified resource.

authority(auth(User,Host,Port)) -->
  (userinfo(User) -> "@" ; ""),
  host(Host),
  (":" -> port(Port) ; "").



%! 'dec-octet'(-Octet:between(0,255))// is det.
%
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



%! fragment(-Frag:atom)// is det.
%
% ```anbf
% fragment = *( pchar / "/" / "?" )
% ```

fragment(Frag) -->
  *(fragment_code, Cs), !,
  {atom_codes(Frag, Cs)}.

fragment_code(C)   --> pchar(C).
fragment_code(0'/) --> "/".
fragment_code(0'?) --> "?".



%! 'gen-delims'(-Code:code)// .
%
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
%
% 16-bit hexadecimal.
%
% ```abnf
% h16 = 1*4HEXDIG
% ```

h16(N) -->
  'm*n'(1, 4, 'HEXDIG', Ds),
  {pos_sum(Ds, N)}.



%! 'hier-part'(-Auth:compound, -Segments:list(atom))// is det.
%
% ```abnf
% hier-part = "//" authority path-abempty
%           / path-absolute
%           / path-rootless
%           / path-empty
% ```

'hier-part'(Auth, Segments) -->
  (   "//"
  ->  authority(Auth),
      'path-abempty'(Segments)
  ;   'path-absolute'(Segments)
  ->  ""
  ;   'path-rootless'(Segments)
  ->  ""
  ;   'path-empty'(Segments)
  ->  ""
  ).



%! host(-Host:compound)// is det.
%
% Host is either a compound IP or an atomic Name.
%
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

host(ip(Version,Address)) -->
  'IP-literal'(Version, Address).
host(ip(4,Address)) -->
  'IPv4address'(Address).
host(Name) -->
  'reg-name'(Name).



%! 'IP-literal'(-Version:nonneg, -Address:compound)// is det.
%
% ```abnf
% IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
% ```

'IP-literal'(Version, Address) -->
  "[",
  ('IPv6address'(Address) -> Version = 6 ; 'IPvFuture'(Version, Address)),
  "]".



%! 'IPv4address'(-Address:list(between(0,255)))// is det.
%
% ```abnf
% IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
% ```

'IPv4address'([I1,I2,I3,I4]) -->
  'dec-octet'(I1),
  ".",
  'dec-octet'(I2),
  ".",
  'dec-octet'(I3),
  ".",
  'dec-octet'(I4),
  ".".



%! 'IPv6address'(-Address:list(between(0,65535)))// is det.
%
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



%! 'IPvFuture'(-Version:nonneg, -Address:atom)// is det.
%
% ```abnf
% IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
% ```

'IPvFuture'(ip(Version,Address)) -->
  "v",
  +('HEXDIG', Ds), !,
  {pos_sum(Ds, 16, Version)},
  ".",
  +(ipv_future_code, Cs), !,
  {atom_codes(Address, Cs)}.

ipv_future_code(C)   --> unreserved(C).
ipv_future_code(C)   --> 'sub-delims'(C).
ipv_future_code(0':) --> ":".



%! ls32(-Address:list(between(0,65535)))// is det.
%
% ```abnf
% ls32 = ( h16 ":" h16 ) / IPv4address
% ```

ls32([N1,N2]) -->
  h16(N1), ":", h16(N2), !.
ls32(Ns) -->
  'IPv4address'(Ns).



%! path(-Segments:list(atom))// is det.
%
% ```abnf
% path =  path-abempty    ; begins with "/" or is empty
%      /  path-absolute   ; begins with "/" but not "//"
%      /  path-noscheme   ; begins with a non-colon segment
%      /  path-rootless   ; begins with a segment
%      /  path-empty      ; zero characters
% ```

% Begins with "/" or is empty.
path(Segments) --> 'path-abempty'(Segments).
% Begins with "/" but not "//".
path(Segments) --> 'path-absolute'(Segments).
% Begins with a non-colon segment
path(Segments) --> 'path-noscheme'(Segments).
% Begins with a segment
path(Segments) --> 'path-rootless'(Segments).
% Empty path (i.e., no segments).
path(Segments) --> 'path-empty'(Segments).



%! 'path-abempty'(-Segments:list(atom))// is det.
%
% ```abnf
% path-abempty = *( "/" segment )
% ```

'path-abempty'(L) -->
  *(sep_segment, L).



%! 'path-absolute'(-Segments:list(atom))// is det.
%
% ```abnf
% path-absolute = "/" [ segment-nz *( "/"  segment ) ]
% ```

'path-absolute'([H|T]) -->
  "/",
  ('segment-nz'(H) -> *(sep_segment, T) ; {T = []}).



%! 'path-empty'(-Segments:list(atom))// is det.
%
% ```abnf
% path-empty = 0<pchar>
% ```

'path-empty'([]) --> "".



%! 'path-noscheme'(-Segments:list(atom))// is det.
%
% ```abnf
% path-noscheme = segment-nz-nc *( "/"  segment )
% ```

'path-noscheme'([H|T]) -->
  'segment-nz-nc'(H),
  *(sep_segment, T).



%! 'path-rootless'(-Segments:list(atom))// is det.
%
% ```abnf
% path-rootless = segment-nz *( "/"  segment )
% ```

'path-rootless'([H|T]) -->
  'segment-nz'(H),
  *(sep_segment, T).



%! pchar(-Code:code)// .
%
% ```abnf
% pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
% ```

pchar(C)   --> unreserved(C).
pchar(C)   --> 'pct-encoded'(C).
pchar(C)   --> 'sub-delims'(C).
pchar(0':) --> ":".
pchar(0'@) --> "@".



%! 'pct-encoded'(-Code:between(0,255))// .
%
% Similar to escape//1 in RFC 1738 which also supports uppercase
% hexadecimal letters.
%
% ```abnf
% pct-encoded = "%" HEXDIG HEXDIG
% ```

'pct-encoded'(C) -->
  "%",
  'HEXDIG'(H1),
  'HEXDIG'(H2),
  {C is H1 * 16 + H2}.



%! port(-Port:nonneg)// is det.
%
% ```abnf
% port = *DIGIT
% ```

port(Port) -->
  *('DIGIT', Ds), !,
  {pos_sum(Ds, Port)}.



%! query(-Query:atom)// is det.
%
% ```abnf
% query = *( pchar / "/" / "?" )
% ```

query(Query) -->
  *(query_code, Cs), !,
  {atom_codes(Query, Cs)}.

query_code(C)   --> pchar(C).
query_code(0'/) --> "/".
query_code(0'?) --> "?".



%! 'reg-name'(-Name:atom)// is det.
%
% “Registered name”
%
% ```abnf
% reg-name = *( unreserved / pct-encoded / sub-delims )
% ```

'reg-name'(Name) -->
  *(reg_name_code, Cs), !,
  {atom_codes(Name, Cs)}.

reg_name_code(C) --> unreserved(C).
reg_name_code(C) --> 'pct-encoded'(C).
reg_name_code(C) --> 'sub-delims'(C).



%! 'relative-part'(-Auth:compound, -Segments:list(atom))// is det.
%
% ```abnf
% relative-part = "//" authority path-abempty
%               / path-absolute
%               / path-noscheme
%               / path-empty
% ```

'relative-part'(Auth, Segments) -->
  (   "//"
  ->  authority(Auth),
      'path-abempty'(Segments)
  ;   'path-absolute'(Segments)
  ->  ""
  ;   'path-noscheme'(Segments)
  ->  ""
  ;   'path-empty'(Segments)
  ->  ""
  ).



%! 'relative-ref'(-Uri:compound)// .
%
% Relative URI reference.
%
% ```abnf
% relative-ref = relative-part [ "?" query ] [ "#" fragment ]
% ```

'relative-ref'(uri(_,Auth,Segments,Query,Frag)) -->
  'relative-part'(Auth, Segments),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Frag) ; "").



%! reserved(-Code:code)// .
%
% ```abnf
% reserved = gen-delims / sub-delims
% ```

reserved(C) --> 'gen-delims'(C).
reserved(C) --> 'sub-delims'(C).



%! scheme(-Scheme:atom)// is det.
%
% An US-ASCII letter, followed by a sequence consisting of US-ASCII
% letters, digits, plus, dash, and dot characters.
%
% ```abnf
% scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
% ```

scheme(Scheme) -->
  alpha(H),
  *(scheme_code, T), !,
  {atom_codes(Scheme, [H|T])}.

scheme_code(C)   --> alpha(C).
scheme_code(C)   --> digit(_, C).
scheme_code(0'+) --> "+".
scheme_code(0'-) --> "-".
scheme_code(0'.) --> ".".



%! segment(-Segment:atom)// is det.
%
% ```abnf
% segment = *pchar
% ```

segment(Segment) -->
  *(pchar, Cs), !,
  {atom_codes(Segment, Cs)}.



%! 'segment-nz'(-Segment:atom)// is det.
%
% Non-empty (nz = non-zero?) segment.
%
% ```abnf
% segment-nz = 1*pchar
% ```

'segment-nz'(Segment) -->
  +(pchar, Cs), !,
  {atom_codes(Segment, Cs)}.



%! 'segment-nz-nc'(-Segment:atom)// is det.
%
% Non-zero-length segment without any colon.
%
% ```abnf
% segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
% ```

'segment-nz-nc'(Segment) -->
  +(segment_nz_nc_code, Cs), !,
  {atom_codes(Segment, Cs)}.

segment_nz_nc_code(C)   --> unreserved(C).
segment_nz_nc_code(C)   --> 'pct-encoded'(C).
segment_nz_nc_code(C)   --> 'sub-delims'(C).
segment_nz_nc_code(0'@) --> "@".



%! 'sub-delims'(-Code:code)// .
%
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



%! unreserved(-Code:code)// .
%
% ```abnf
% unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
% ```

unreserved(C)   --> alphadigit(C).
unreserved(0'-) --> "-".
unreserved(0'.) --> ".".
unreserved(0'_) --> "_".
unreserved(0'~) --> "~".



%! 'URI'(-Uri:compound)// is det.
%
% ```abnf
% URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
% ```

'URI'(uri_components(Scheme,Auth,Segments,Query,Frag)) -->
  scheme(Scheme),
  ":",
  'hier-part'(Auth, Segments),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Frag) ; "").



%! 'URI-reference'(-Uri:compound)// is det.
%
% ```abnf
% URI-reference = URI / relative-ref
% ```

'URI-reference'(Uri) --> 'URI'(Uri), !.
'URI-reference'(Uri) --> 'relative-ref'(Uri).



%! userinfo(-User:atom)// is det.
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
% scheme-specific information about how to gain authorization to
% access the denoted resource.
%
% # Security
%
% Applications that render a URI for the sake of user feedback, such
% as in graphical hypertext browsing, should render userinfo in a way
% that is distinguished from the rest of a URI, when feasible.  Such
% rendering will assist the user in cases where the userinfo has been
% misleadingly crafted to look like a trusted domain name
%
% # Legacy
%
% Use of the format `user:password` in the userinfo field is
% deprecated.  The passing of authentication information in clear text
% has proven to be a security risk.

userinfo(User) -->
  *(userinfo_code, Cs), !,
  {atom_codes(User, Cs)}.

userinfo_code(C)   --> unreserved(C).
userinfo_code(C)   --> 'pct-encoded'(C).
userinfo_code(C)   --> 'sub-delims'(C).
userinfo_code(0':) --> ":".





% HELPERS %

sep_segment(Segment) -->
  "/",
  segment(Segment).
