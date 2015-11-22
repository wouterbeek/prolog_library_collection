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

:- use_module(library(dcg/dcg_re)).
:- use_module(library(dcg/rfc2234_re)).
:- use_module(library(uri/rfc3986_code)).





%! 'dec-octet'(?Octet:between(0,255))// .
% ```abnf
% dec-octet = DIGIT               ; 0-9
%           / %x31-39 DIGIT       ; 10-99
%           / "1" 2DIGIT          ; 100-199
%           / "2" %x30-34 DIGIT   ; 200-249
%           / "25" %x30-35        ; 250-255
% ```

'dec-octet'(I) --> '+DIGIT'(I), {between(0, 255, I)}.



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
  'dec-octet'(I1), ".",
  'dec-octet'(I2), ".",
  'dec-octet'(I3), ".",
  'dec-octet'(I4), ".".



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

'IPv6address' -->       #(6, 'h16:', L), ls32(X).
'IPv6address' --> "::", #(5, 'h16:', L), ls32(X).
'IPv6address' --> ?(h16, L1), "::", #(4, 'h16:', L2), ls32(X).
'IPv6address' -->
  ('m*n'(_, 1, 'h16:', L1), ?(h16, L2) ; L1 = [], L2 = []),
  "::", #(4, 'h16:', L3), ls32(X).

'h16:'(X) --> h16(X), ":".



'IPv6address'(ipv6(FormerH16s,LatterH16s,Ls32s)) -->
  {ipv6_pattern(C1, DoubleColon, C2, Colon, C3)},
  'm*n'(0, 7, h16, FormerH16s, [count(C1),separator(colon)]),
  (   "::",
      {DoubleColon = true}
  ;   {DoubleColon = false}
  ),
  'm*n'(0, 6, h16, LatterH16s, [count(C2),separator(colon)]),
  (   ":",
      {Colon = true}
  ;   {Colon = false}
  ),
  'm*n'(0, 1, ls32, Ls32s, [count(C3)]).



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

'path-abempty'(L) --> segments(L).



%! 'path-absolute'(?Segments:list(string))// .
% ```abnf
% path-absolute = "/" [ segment-nz *( "/"  segment ) ]
% ```

'path-absolute'([H|T]) --> "/", 'segment-nz'(H), !, segments(T).
'path-absolute'([])    --> "/".



%! 'path-empty'(?Segments:list(string))// .
% ```abnf
% path-empty = 0<pchar>
% ```

'path-empty'([]) --> "".



%! 'path-noscheme'(?Segments:list(string))// .
% ```abnf
% path-noscheme = segment-nz-nc *( "/"  segment )
% ```

'path-noscheme'([H|T]) --> 'segment-nz-nc'(H), segments(T).



%! 'path-rootless'(?Segments:list(string))// .
% ```abnf
% path-rootless = segment-nz *( "/"  segment )
% ```

'path-rootless'([H|T]) --> 'segment-nz'(H), segments(T).



%! 'reg-name'(?RegisteredName:compound)// .
% ```abnf
% reg-name = *( unreserved / pct-encoded / sub-delims )
% ```

'reg-name'(S) --> dcg_string(reg_name_codes, S).
reg_name_codes([H|T]) --> unreserved(H),    !, reg_name_codes(T).
reg_name_codes([H|T]) --> 'pct-encoded'(H), !, reg_name_codes(T).
reg_name_codes([H|T]) --> 'sub-delims'(H),  !, reg_name_codes(T).
reg_name_codes([])    --> "".



%! segment(?Segment:string)// .
% ```abnf
% segment = *pchar
% ```

segment(S) --> dcg_string('*pchar', S).



%! 'segment-nz'(?Segment:string)// .
% Non-empty (nz = non-zero?) segment.
%
% ```abnf
% segment-nz = 1*pchar
% ```

'segment-nz'(S) --> dcg_string('+pchar', S).



%! 'segment-nz-nc'(?Segment:string)// .
% Non-zero-length segment without any colon.
%
% ```abnf
% segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
% ```

'segment-nz-nc'(S) --> dcg_string(segment_nz_nc_codes, S).
segment_nz_nc_codes([H|T])   --> unreserved(H),    !, segment_nz_nc_codes(T).
segment_nz_nc_codes([H|T])   --> 'pct-encoded'(H), !, segment_nz_nc_codes(T).
segment_nz_nc_codes([H|T])   --> 'sub-delims'(H),  !, segment_nz_nc_codes(T).
segment_nz_nc_codes([0'@|T]) --> "@",              !, segment_nz_nc_codes(T).
segment_nz_nc_codes([])      --> "".





% HELPERS %

'+pchar'(L) --> +(pchar, L).
'*pchar'(L) --> *(pchar, L).

segments([H|T]) --> "/", !, segment(H), segments(T).
segments([])    --> "".
