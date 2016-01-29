:- module(
  rfc3986,
  [
    'absolute-URI'//1, % -AbsoluteUri:dict
    authority//1, % -Authority:dict
    fragment//1, % -Fragment:string
    host//1, % -Host:dict
    'IP-literal'//1, % ?IpLiteral:dict
    'IPv4address'//1, % ?Address:list(between(0,255))
    'path-abempty'//1, % -Segments:list(string)
    'pct-encoded'//1, % -Code:between(0,255)
    port//1, % -Port:nonneg
    query//1, % -Query:string
    'relative-part'//1, % -RelativePart:dict
    scheme//1, % -Scheme:string
    scheme_code//1, % -Code:code
    segment//1, % -Segment:string
    'segment-nz'//1, % -Segment:string
    'sub-delims'//1 , % ?Code:code
    unreserved//1, % ?Code:code
    'URI'//1, % -Uri:dict
    'URI-reference'//1 % -UriReference:dict
  ]
).

/** <module> RFC 3986

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2015/11-2016/01
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

'absolute-URI'(D2) -->
  scheme(Scheme),
  ":",
  'hier-part'(HierPart),
  {D1 = absolute_uri{'@type': 'uri:absolute-URI', 'uri:hierarchical-part': HierPart, 'uri:scheme': Scheme}},
  ("?" -> query(Query), {D2 = D1.put({'uri:query', Query})} ; {D2 = D1}).



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

authority(D4) -->
  {D1 = authority{'@type': 'uri:authority'}},
  (userinfo(Userinfo), "@" -> {D2 = D1.put({'uri:userinfo', Userinfo})} ; {D2 = D1}),
  host(Host),
  {D3 = D2.put({'uri:host', Host})},
  (":" -> port(Port), {D4 = D3.put({'uri:port', Port})} ; {D4 = D3}).



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

fragment(S) -->
  *(fragment_code, Cs),
  {string_codes(S, Cs)}.

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

h16(N) -->
  'm*n'(1, 4, 'HEXDIG', Ds),
  {pos_sum(Ds, N)}.



%! 'hier-part'(-HierarchicalPart:dict)// is det.
% ```abnf
% hier-part = "//" authority path-abempty
%           / path-absolute
%           / path-rootless
%           / path-empty
% ```

'hier-part'(D2) -->
  {D1 = hier_part{'@type': 'uri:hier-part'}},
  (   "//"
  ->  authority(Authority),
      'path-abempty'(PathAbempty),
      {D2 = D1.put({'uri:authority': Authority, 'uri:path-abempty', PathAbempty})}
  ;   'path-absolute'(PathAbsolute)
  ->  {D2 = D1.put({'uri:path-absolute': PathAbsolute})}
  ;   'path-rootless'(PathRootless)
  ->  {D2 = D1.put({'uri:path-rootless': PathRootless})}
  ;   'path-empty'(PathEmpty)
  ->  {D2 = D1.put({'uri:path-empty': PathEmpty})}
  ).



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

host(D2) -->
  {D1 = host{'@type': 'uri:host'}},
  (   'IP-literal'(IpLiteral)
  ->  {D2 = D1.put({'uri:IP-literal', IpLiteral})}
  ;   'IPv4address'(Ipv4Address)
  ->  {D2 = D1.put({'uri:IPv4address', Ipv4Address})}
  ;   'reg-name'(RegName)
  ->  {D2 = D1.put({'uri:reg-name', RegName})}
  ).



%! 'IP-literal'(?IpLiteral:dict)// is det.
% ```abnf
% IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
% ```

'IP-literal'(D2) -->
  {D1 = ip_literal{'@type': 'uri:IP-literal'}},
  "[",
  (   'IPv6address'(Ipv6Address)
  ->  {D2 = D1.put({'uri:IPv6address', Ipv6Address})}
  ;   'IPvFuture'(IpvFuture)
  ->  {D2 = D1.put({'uri:IPvFuture': IpvFuture})}
  ),
  "]".



%! 'IPv4address'(?Address:list(between(0,255)))// is det.
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



%! 'IPv6address'(-Ipv6Address:list(dict))// is det.
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



%! 'IPvFuture'(-IpvFuture:dict)// is det.
% ```abnf
% IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
% ```

'IPvFuture'(ipvfuture{'@type': 'uri:IPvFuture', 'uri:major': I, 'uri:minor': S}) -->
  "v",
  +('HEXDIG', Ds),
  {pos_sum(Ds, 16, I)},
  ".",
  +(ipv_future_code, Cs),
  {string_codes(S, Cs)}.
ipv_future_code(C)   --> unreserved(C).
ipv_future_code(C)   --> 'sub-delims'(C).
ipv_future_code(0':) --> ":".



%! ls32(-Ls32:dict)// is det.
% ```abnf
% ls32 = ( h16 ":" h16 ) / IPv4address
% ```

ls32(_{'@type': 'llo:h16', 'rdf:value': [H16a,H16b]}) -->
  h16(H16a), ":", h16(H16b), !.
ls32(_{'@type': 'llo:IPv4address', 'rdf:value': Ipv4Address}) -->
  'IPv4address'(Ipv4Address).



%! path(-Segments:list(string))// is det.
% ```abnf
% path =  path-abempty    ; begins with "/" or is empty
%      /  path-absolute   ; begins with "/" but not "//"
%      /  path-noscheme   ; begins with a non-colon segment
%      /  path-rootless   ; begins with a segment
%      /  path-empty      ; zero characters
% ```

path(D2) -->
  {D1 = path{'@type': 'uri:path'}},
  (   % Begins with "/" or is empty.
      'path-abempty'(PathAbempty)
  ->  {D2 = D1.put({'uri:path-abempty', PathAbempty})}
  ;   % Begins with "/" but not "//".
      'path-absolute'(PathAbsolute)
  ->  {D2 = D1.put({'uri:path-absolute': PathAbsolute})}
  ;   % Begins with a non-colon segment
      'path-noscheme'(PathNoscheme)
  ->  {D2 = D1.put({'uri:path-noscheme': PathNoscheme})}
  ;   % Begins with a segment
      'path-rootless'(PathRootless)
  ->  {D2 = D1.put({'uri:path-rootless': PathRootless})}
  ;   % Empty path (i.e., no segments).
      'path-empty'(PathEmpty)
  ->  {D2 = D1.put({'uri:path-empty': PathEmpty})}
  ).



%! 'path-abempty'(-Segments:list(string))// is det.
% ```abnf
% path-abempty = *( "/" segment )
% ```

'path-abempty'(L) -->
  *(sep_segment, L).



%! 'path-absolute'(-Segments:list(string))// is det.
% ```abnf
% path-absolute = "/" [ segment-nz *( "/"  segment ) ]
% ```

'path-absolute'([H|T]) -->
  "/",
  ('segment-nz'(H) -> *(sep_segment, T) ; {T = []}).



%! 'path-empty'(-Segments:list(string))// is det.
% ```abnf
% path-empty = 0<pchar>
% ```

'path-empty'([]) --> "".



%! 'path-noscheme'(-Segments:list(string))// is det.
% ```abnf
% path-noscheme = segment-nz-nc *( "/"  segment )
% ```

'path-noscheme'([H|T]) -->
  'segment-nz-nc'(H),
  *(sep_segment, T).



%! 'path-rootless'(-Segments:list(string))// is det.
% ```abnf
% path-rootless = segment-nz *( "/"  segment )
% ```

'path-rootless'([H|T]) -->
  'segment-nz'(H),
  *(sep_segment, T).



%! pchar(-Code:code)// .
% ```abnf
% pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
% ```

pchar(C)   --> unreserved(C).
pchar(C)   --> 'pct-encoded'(C).
pchar(C)   --> 'sub-delims'(C).
pchar(0':) --> ":".
pchar(0'@) --> "@".



%! 'pct-encoded'(-Code:between(0,255))// .
% Similar to escape//1 in RFC 1738 which also supports
% uppercase hexadecimal letters.
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
% ```abnf
% port = *DIGIT
% ```

port(N) -->
  *('DIGIT', Ds),
  {pos_sum(Ds, N)}.



%! query(-Query:string)// is det.
% ```abnf
% query = *( pchar / "/" / "?" )
% ```

query(S) --> *(query_code, Cs), {string_codes(S, Cs)}.
query_code(C)   --> pchar(C).
query_code(0'/) --> "/".
query_code(0'?) --> "?".



%! 'reg-name'(?RegName:string)// is det.
% “Registered name”
%
% ```abnf
% reg-name = *( unreserved / pct-encoded / sub-delims )
% ```

'reg-name'(S) -->
  *(reg_name_code, Cs),
  {string_codes(S, Cs)}.

reg_name_code(C) --> unreserved(C).
reg_name_code(C) --> 'pct-encoded'(C).
reg_name_code(C) --> 'sub-delims'(C).



%! 'relative-part'(-RelativePart:dict)// is det.
% ```abnf
% relative-part = "//" authority path-abempty
%               / path-absolute
%               / path-noscheme
%               / path-empty
% ```

'relative-part'(D2) -->
  {D1 = relative_part{'@type': 'uri:relative-part'}},
  (   "//"
  ->  authority(Authority),
      'path-abempty'(PathAbempty),
      {D2 = D1.put({'uri:authority': Authority, 'uri:path-abempty', PathAbempty})}
  ;   'path-absolute'(PathAbsolute)
  ->  {D2 = D1.put({'uri:path-absolute': PathAbsolute})}
  ;   'path-noscheme'(PathNoscheme)
  ->  {D2 = D1.put({'uri:path-noscheme': PathNoscheme})}
  ;   'path-empty'(PathEmpty)
  ->  {D2 = D1.put({'uri:path-empty': PathEmpty})}
  ).



%! 'relative-ref'(-RelativeRef:dict)// is det.
% Relative URI reference.
%
% ```abnf
% relative-ref = relative-part [ "?" query ] [ "#" fragment ]
% ```

'relative-ref'(D3) -->
  'relative-part'(RelativePart),
  {D1 = relative_ref{'@type': 'uri:relative-ref', 'uri:relative-part': RelativePart}},
  ("?" -> query(Query), {D2 = D1.put({'uri:query', Query})} ; {D2 = D1}),
  ("#" -> fragment(Fragment), {D3 = D2.put({'uri:fragment', Fragment})} ; {D3 = D2}).



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

scheme(S) -->
  alpha(H), !,
  *(scheme_code, T),
  {string_codes(S, [H|T])}.

scheme_code(C)   --> alpha(C).
scheme_code(C)   --> digit(_, C).
scheme_code(0'+) --> "+".
scheme_code(0'-) --> "-".
scheme_code(0'.) --> ".".



%! segment(-Segment:string)// is det.
% ```abnf
% segment = *pchar
% ```

segment(S) -->
  *(pchar, Cs),
  {string_codes(S, Cs)}.



%! 'segment-nz'(-Segment:string)// is det.
% Non-empty (nz = non-zero?) segment.
%
% ```abnf
% segment-nz = 1*pchar
% ```

'segment-nz'(S) -->
  +(pchar, Cs),
  {string_codes(S, Cs)}.



%! 'segment-nz-nc'(-Segment:string)// is det.
% Non-zero-length segment without any colon.
%
% ```abnf
% segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
% ```

'segment-nz-nc'(S) -->
  +(segment_nz_nc_code, Cs),
  {string_codes(S, Cs)}.

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

'URI'(D3) -->
  scheme(Scheme),
  ":",
  'hier-part'(HierPart),
  {D1 = uri{'@type': 'uri:URI', 'uri:scheme': Scheme, 'uri:hier-part': HierPart}},
  ("?" -> query(Query), {D2 = D1.put({'uri:query': Query})} ; {D2 = D1}),
  ("#" -> fragment(Fragment), {D3 = D2.put({'uri:fragment': Fragment})} ; {D3 = D2}).



%! 'URI-reference'(-UriReference:dict)// is det.
% ```abnf
% URI-reference = URI / relative-ref
% ```

'URI-reference'(D2) -->
  {D1 = _{'@type': 'uri:URI-reference'}},
  (   'URI'(Uri)
  ->  {D2 = D1.put({'uri:URI': Uri})}
  ;   'relative-ref'(RelativeRef)
  ->  {D2 = D1.put({'uri:relative-ref': RelativeRef})}
  ).



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

userinfo(S) -->
  *(userinfo_code, Cs),
  {string_codes(S, Cs)}.

userinfo_code(C)   --> unreserved(C).
userinfo_code(C)   --> 'pct-encoded'(C).
userinfo_code(C)   --> 'sub-delims'(C).
userinfo_code(0':) --> ":".





% HELPERS %

sep_segment(S) --> "/", segment(S).
