:- module(
  rfc3986_token,
  [
    'dec-octet'//1, % ?Octet:between(0,255)
    h16//1, % ?Number:between(0,65535)
    'IP-literal'//1, % ?IpLiteral:compound
    'IPv4address'//1, % ?Address:list(between(0,255))
    'IPv6address'//1, % ?Address:compound
    'IPvFuture'//1, % ?IpLiteral:compound
    ls32//1, % ?Address:compound
    'path-abempty'//1, % ?Segments:list(string)
    'path-absolute'//1, % ?Segments:list(string)
    'path-empty'//1, % ?Segments:list(string)
    'path-noscheme'//1, % ?Segments:list(string)
    'path-rootless'//1, % ?Segments:list(string)
    'reg-name'//1, % ?RegisteredName:string
    segment//1, % ?Segment:string
    'segment-nz'//1, % ?Segment:string
    'segment-nz-nc'//1 % ?Segment:string
  ]
).

/** <module> RFC 3986: Tokens

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_rfc)).
:- use_module(library(lists)).
:- use_module(library(uri/rfc3986_code)).





%! 'dec-octet'(?Octet:between(0,255))// .
% ```abnf
% dec-octet = DIGIT               ; 0-9
%           / %x31-39 DIGIT       ; 10-99
%           / "1" 2DIGIT          ; 100-199
%           / "2" %x30-34 DIGIT   ; 200-249
%           / "25" %x30-35        ; 250-255
% ```

'dec-octet'(I) --> '+digit'(I), {between(0, 255, I)}.



%! h16(?Number:between(0,65535))// .
% 16-bit hexadecimal.
%
% ```abnf
% h16 = 1*4HEXDIG
% ```

h16(I) --> 'm*nHEXDIG'(1, 4, I).



%! 'IP-literal'(?IpLiteral:compound)// .
% ```abnf
% IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
% ```

'IP-literal'(Lit) --> "[", ('IPv6address'(Lit), ! ; 'IPvFuture'(Lit)), "]".



%! 'IPv4address'(?Address:list(between(0,255)))// .
% ```abnf
% IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
% ```

'IPv4address'([I1,I2,I3,I4]) -->
  'dec-octet'(I1), ".", 'dec-octet'(I2), ".", 'dec-octet'(I3), ".", 'dec-octet'(I4), ".".



%! 'IPv6address'(?Ipv6Address:compound)// .
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



%! 'IPvFuture'(?IpLiteral:compound)// .
% ```abnf
% IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
% ```

'IPvFuture'(ipvfuture(I,S)) -->
  "v", '+HEXDIG'(I), ".", +(ipv_future_code, Cs), {string_codes(S, Cs)}.
ipv_future_code(C)   --> unreserved(C).
ipv_future_code(C)   --> 'sub-delims'(C).
ipv_future_code(0':) --> ":".



%! ls32(?Address:compound)// .
% ```abnf
% ls32 = ( h16 ":" h16 ) / IPv4address
% ```

ls32(ipa([H16A,H16B])) --> h16(H16A), !, ":", h16(H16B).
ls32(ipv4(Address))    --> 'IPv4address'(Address).



%! 'path-abempty'(?Segments:list(string))// .
% ```abnf
% path-abempty = *( "/" segment )
% ```

'path-abempty'(L) --> *(sep_segment, L).



%! 'path-absolute'(?Segments:list(string))// .
% ```abnf
% path-absolute = "/" [ segment-nz *( "/"  segment ) ]
% ```

'path-absolute'([H|T]) -->
  "/", ('segment-nz'(H) -> *(sep_segment, T) ; {T = []}).



%! 'path-empty'(?Segments:list(string))// .
% ```abnf
% path-empty = 0<pchar>
% ```

'path-empty'([]) --> "".



%! 'path-noscheme'(?Segments:list(string))// .
% ```abnf
% path-noscheme = segment-nz-nc *( "/"  segment )
% ```

'path-noscheme'([H|T]) --> 'segment-nz-nc'(H), *(sep_segment, T).



%! 'path-rootless'(?Segments:list(string))// .
% ```abnf
% path-rootless = segment-nz *( "/"  segment )
% ```

'path-rootless'([H|T]) --> 'segment-nz'(H), *(sep_segment, T).



%! 'reg-name'(?RegisteredName:compound)// .
% ```abnf
% reg-name = *( unreserved / pct-encoded / sub-delims )
% ```

'reg-name'(S) --> *(reg_name_code, Cs), {string_codes(S, Cs)}.
reg_name_code(C) --> unreserved(C).
reg_name_code(C) --> 'pct-encoded'(C).
reg_name_code(C) --> 'sub-delims'(C).



%! segment(?Segment:string)// .
% ```abnf
% segment = *pchar
% ```

segment(S) --> *(pchar, Cs), {string_codes(S, Cs)}.



%! 'segment-nz'(?Segment:string)// .
% Non-empty (nz = non-zero?) segment.
%
% ```abnf
% segment-nz = 1*pchar
% ```

'segment-nz'(S) --> +(pchar, Cs), {string_codes(S, Cs)}.



%! 'segment-nz-nc'(?Segment:string)// .
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





% HELPERS %

sep_segment(S) --> "/", segment(S).
