:- module(
  rfc3986,
  [
    'absolute-URI'//1,  % -Uri
    authority//1,       % -Authority
    fragment//1,        % -Fragment
    host//1,            % -Host
    'IP-literal'//1,    % -Ip
    'IPv4address'//1,   % -Address
    'path-abempty'//1,  % -Segments
    pchar//1,           % -Code
    'pct-encoded'//1,   % -Code
    port//1,            % -Port
    query//1,           % -Query
    'relative-part'//2, % -Authority, -Segments
    scheme//1,          % -Scheme
    scheme_code//1,     % -Code
    segment//1,         % -Segment
    'segment-nz'//1,    % -Segment
    'sub-delims'//1 ,   % -Code
    unreserved//1,      % -Code
    'URI'//1,           % -Uri
    'URI-reference'//1, % ?Uri
    'URI-reference'//2  % ?BaseUri, ?Uri
  ]
).

/** <module> RFC 3986 - Uniform Resource Identifier (URI): Generic Syntax

The following terms are used:

| Authority | auth(User,host,Port)                               |
| Host      | IP or atom Name                                    |
| IP        | ip(Version,Address)                                |
| Uri       | uri(Scheme,Authority,Segments,QueryComps,Fragment) |

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2017/04-2017/08
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(math_ext)).
:- use_module(library(uri/uri_ext)).





%! 'absolute-URI'(-Uri:compound)// is det.
%
% ```abnf
% absolute-URI = scheme ":" hier-part [ "?" query ]
% ```

'absolute-URI'(uri(Scheme,Authority,Segments,Query,_)) -->
  scheme(Scheme),
  ":",
  'hier-part'(Authority, Segments),
  ("?" -> query(Query) ; "").



%! authority(-Authority:compound)// is det.
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
  (userinfo(User), "@" -> "" ; ""),
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
  {D3 is C - 0x30, integer_weights(D, [2,5,D3])}.
'dec-octet'(D) -->
  "2",
  [C], {between(0x30, 0x34, C), !, D2 is C - 0x30},
  digit_weight(D3), !,
  {integer_weights(D, [2,D2,D3])}.
'dec-octet'(D) -->
  "1",
  #(2, digit_weight, [D2,D3]), !,
  {integer_weights(D, [1,D2,D3])}.
'dec-octet'(D) -->
  [C], {between(0x31, 0x39, C), D1 is C - 0x30},
  digit_weight(D2),
  {D is D1 * 10 + D2}.
'dec-octet'(D) -->
  digit_weight(D).



%! fragment(-Fragment:atom)// is det.
%
% ```anbf
% fragment = *( pchar / "/" / "?" )
% ```

fragment(Fragment) -->
  *(fragment_code, Cs), !,
  {atom_codes(Fragment, Cs)}.

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
  'm*n'(1, 4, xdigit, Weights),
  {integer_weights(N, Weights)}.



%! 'hier-part'(-Authority:compound, -Segments:list(atom))// is det.
%
% ```abnf
% hier-part = "//" authority path-abempty
%           / path-absolute
%           / path-rootless
%           / path-empty
% ```

'hier-part'(Authority, Segments) -->
  (   "//"
  ->  authority(Authority),
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

host(Ip) -->
  'IP-literal'(Ip).
host(ip(4,Address)) -->
  'IPv4address'(Address).
host(Name) -->
  'reg-name'(Name).



%! 'IP-literal'(-Ip:compound)// is det.
%
% ```abnf
% IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
% ```

'IP-literal'(Ip) -->
  "[",
  ('IPv6address'(Address) -> {Ip = ip(6,Address)} ; 'IPvFuture'(Ip)),
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



%! 'IPvFuture'(-Ip:compound)// is det.
%
% ```abnf
% IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
% ```

'IPvFuture'(ip(Version,Address)) -->
  "v",
  +(xdigit, Weights), !,
  {integer_weights(Version, 16, Weights)},
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
  h16(N1),
  ":",
  h16(N2), !.
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
path(Segments) --> 'path-abempty'(Segments), !.
% Begins with "/" but not "//".
path(Segments) --> 'path-absolute'(Segments), !.
% Begins with a non-colon segment
path(Segments) --> 'path-noscheme'(Segments), !.
% Begins with a segment
path(Segments) --> 'path-rootless'(Segments), !.
% Empty path (i.e., no segments).
path(Segments) --> 'path-empty'(Segments).



%! 'path-abempty'(-Segments:list(atom))// is det.
%
% ```abnf
% path-abempty = *( "/" segment )
% ```

'path-abempty'(L) -->
  *(sep_segment, L), !.



%! 'path-absolute'(-Segments:list(atom))// is det.
%
% ```abnf
% path-absolute = "/" [ segment-nz *( "/"  segment ) ]
% ```

'path-absolute'([H|T]) -->
  "/",
  ('segment-nz'(H) -> *(sep_segment, T), ! ; {T = []}).



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
  *(sep_segment, T), !.



%! 'path-rootless'(-Segments:list(atom))// is det.
%
% ```abnf
% path-rootless = segment-nz *( "/"  segment )
% ```

'path-rootless'([H|T]) -->
  'segment-nz'(H),
  *(sep_segment, T), !.



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
  xdigit(H1),
  xdigit(H2),
  {C is H1 * 16 + H2}.



%! port(-Port:nonneg)// is det.
%
% ```abnf
% port = *DIGIT
% ```

port(Port) -->
  *(digit_weight, Weights), !,
  {integer_weights(Port, Weights)}.



%! query(-Query:list(compound))// is det.
%
% ```abnf
% query = *( pchar / "/" / "?" )
% ```

query(Query) -->
  *(query_code, Cs), !,
  {
    atom_codes(Query, Cs),
    uri_query_components(Query, Query)
  }.

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



%! 'relative-part'(-Authority:compound, -Segments:list(atom))// is det.
%
% ```abnf
% relative-part = "//" authority path-abempty
%               / path-absolute
%               / path-noscheme
%               / path-empty
% ```

'relative-part'(Authority, Segments) -->
  (   "//"
  ->  authority(Authority),
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

'relative-ref'(uri(_,Authority,Segments,Query,Fragment)) -->
  'relative-part'(Authority, Segments),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Fragment) ; "").



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

scheme_code(C)   --> alphanum(C).
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

unreserved(C)   --> alphanum(C).
unreserved(0'-) --> "-".
unreserved(0'.) --> ".".
unreserved(0'_) --> "_".
unreserved(0'~) --> "~".



%! 'URI'(-Uri:compound)// is det.
%
% ```abnf
% URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
% ```

'URI'(uri(Scheme,Authority,Segments,Query,Fragment)) -->
  scheme(Scheme),
  ":",
  'hier-part'(Authority, Segments),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Fragment) ; "").



%! 'URI-reference'(?Uri:atom)// is det.
%! 'URI-reference'(?BaseUri:atom, ?Uri:atom)// is det.
%
% ```abnf
% URI-reference = URI / relative-ref
% ```

'URI-reference'(Uri) -->
  'URI-reference'(_, Uri).


'URI-reference'(BaseUri, Uri) -->
  (   'URI'(UriComps)
  ->  {uri_comps(Uri, UriComps)}
  ;   'relative-ref'(RelUriComps),
      {
        uri_comps(RelUri, RelUriComps),
        uri_resolve(RelUri, BaseUri, Uri)
      }
  ).



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
