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

:- use_module(library(uri/rfc3986_code)).





%! 'dec-octet'(?Octet:between(0,255))// .
% ```abnf
% dec-octet = DIGIT               ; 0-9
%           / %x31-39 DIGIT       ; 10-99
%           / "1" 2DIGIT          ; 100-199
%           / "2" %x30-34 DIGIT   ; 200-249
%           / "25" %x30-35        ; 250-255
% ```

'dec-octet'(N) --> '+DIGIT'(N), {between(0, 255, N)}.



%! h16(?Number:between(0,65535))// .
% 16-bit hexadecimal.
%
% ```abnf
% h16 = 1*4HEXDIG
% ```

h16(N) -->
  'm*n'(1, 4, 'HEXDIG', L),
  {positional(L, 16, N)}.

positional(L, Base, N):-
  positional(L, Base, 0, N).

positional([], _, N, N):- !.
positional([H|T], Base, N1, N):-
  N2 is N1 * Base + H,
  positional(T, Base, N2, N).



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

'IPv6address' --> '#h16'(6, L), ls32(X).


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
  "v", '+HEXDIG'(N), ".",
  dcg_string(ipv_future_codes, S).
ipv_future_codes([H|T])   --> unreserved(H),   !, ipv_future_codes(T).
ipv_future_codes([H|T])   --> 'sub-delims'(H), !, ipv_future_codes(T).
ipv_future_codes([0':|T]) --> ":",             !, ipv_future_codes(T).
ipv_future_codes([])      --> "".



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

'#h16'(M, L) --> '#h16'(M, 0, L).
'#h16'(M,  M, [])    --> "".
'#h16'(M1, N, [H|T]) --> h16(HT), !, {M2 is M1 + 1}, '#h16'(M2, N, T).



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



'+pchar'([H|T]) --> pchar(H), !, '*pchar'(T).
'*pchar'([H|T]) --> pchar(H), !, '*pchar'(T).
'*pchar'([])    --> "".



%! segments(?Segments:list(string))// .

segments([H|T]) --> "/", !, segment(H), segments(T).
segments([])    --> "".
