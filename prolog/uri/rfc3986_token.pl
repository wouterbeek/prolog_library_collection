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

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(math/positional)).
:- use_module(library(string_ext)).
:- use_module(library(uri/rfc3986_code)).





%! 'dec-octet'(?Octet:between(0,255))// .
% ```abnf
% dec-octet = DIGIT               ; 0-9
%           / %x31-39 DIGIT       ; 10-99
%           / "1" 2DIGIT          ; 100-199
%           / "2" %x30-34 DIGIT   ; 200-249
%           / "25" %x30-35        ; 250-255
% ```

'dec-octet'(N) --> between_radix(dec(0), dec(255), dec(N)).



%! h16(?Number:between(0,65535))// .
% 16-bit hexadecimal.
%
% ```abnf
% h16 = 1*4HEXDIG
% ```

h16(N) -->
  'm*n'(1, 4, 'HEXDIG', N, [convert1(positional)]).



%! 'IP-literal'(?IpLiteral:compound)// .
% ```abnf
% IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
% ```

'IP-literal'(Literal) --> "[", 'IPv6address'(Literal), !, "]".
'IP-literal'(Literal) --> "[", 'IPvFuture'(Literal), "]".



%! 'IPv4address'(?Address:list(between(0,255)))// .
% ```abnf
% IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
% ```

'IPv4address'([N1,N2,N3,N4]) -->
  '#'(4, 'dec-octet', [N1,N2,N3,N4], [separator(dot)]).



%! 'IPv6address'(?Ipv6Address:compound)// .
% ```abnf
% IPv6address =                              6( h16 ":" ) ls32
%             /                       "::" 5( h16 ":" ) ls32
%             / [               h16 ] "::" 4( h16 ":" ) ls32
%             / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
%             / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
%             / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
%             / [ *4( h16 ":" ) h16 ] "::"              ls32
%             / [ *5( h16 ":" ) h16 ] "::"    h16
%             / [ *6( h16 ":" ) h16 ] "::"
% ```

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

'IPvFuture'(ipvfuture(N,S)) -->
  "v",
  +('HEXDIG', N, [convert1(positional)]),
  ".",
  +(ipv_future_code, S, [convert1(codes_string)]).
ipv_future_code(C) --> unreserved(C).
ipv_future_code(C) --> 'sub-delims'(C).
ipv_future_code(0':) --> ":".



%! ls32(?Address:compound)// .
% ```abnf
% ls32 = ( h16 ":" h16 ) / IPv4address
% ```

ls32(ipa([H16A,H16B])) --> h16(H16A), ":", h16(H16B).
ls32(ipv4(Address)) --> 'IPv4address'(Address).



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
'path-absolute'([]) --> "/".



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

'reg-name'(S) --> *(reg_name_code, S, [convert1(codes_string)]).
reg_name_code(C) --> unreserved(C).
reg_name_code(C) --> 'pct-encoded'(C).
reg_name_code(C) --> 'sub-delims'(C).



%! segment(?Segment:string)// .
% ```abnf
% segment = *pchar
% ```

segment(S) --> *(pchar, S, [convert1(codes_string)]).



%! 'segment-nz'(?Segment:string)// .
% Non-empty (nz = non-zero?) segment.
%
% ```abnf
% segment-nz = 1* pchar
% ```

'segment-nz'(S) --> +(pchar, S, [convert1(codes_string)]).



%! 'segment-nz-nc'(?Segment:string)// .
% Non-zero-length segment without any colon.
%
% ```abnf
% segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
% ```

'segment-nz-nc'(S) --> +(segment_nz_nc_code, S, [convert1(codes_string)]).
segment_nz_nc_code(C) --> unreserved(C).
segment_nz_nc_code(C) --> 'pct-encoded'(C).
segment_nz_nc_code(C) --> 'sub-delims'(C).
segment_nz_nc_code(0'@) --> "@".





% HELPERS %

%! ipv6_pattern(
%!   ?NumberOfFormerH16s:between(0,7),
%!   ?HasDoubleColon:boolean,
%!   ?NumberOfLatterH16s:between(0,6),
%!   ?HasColon:boolean,
%!   ?NumberOfLs32s:between(0,1)
%! ).

ipv6_pattern(0, false, 6, true,  1).
ipv6_pattern(0, true,  5, true,  1).
ipv6_pattern(1, true,  4, true,  1).
ipv6_pattern(2, true,  3, true,  1).
ipv6_pattern(3, true,  3, true,  1).
ipv6_pattern(4, true,  1, true,  1).
ipv6_pattern(5, true,  0, false, 1).
ipv6_pattern(6, true,  1, false, 0).
ipv6_pattern(7, true,  0, false, 0).



%! segments(?Segments:list(string))// .

segments([H|T]) --> "/", !, segment(H), segments(T).
segments([]) --> "".
