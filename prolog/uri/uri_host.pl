:- module(
  uri_host,
  [
    host//2 % ?Iri:boolean
            % ?Host:compound
  ]
).

/** <module> RFC 3986 & RFC 3987: Host component

@author Wouter Beek
@compat RFC 3986
@compat RFC 3987
@see http://tools.ietf.org/html/rfc3987
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_cardinal)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(math/positional)).
:- use_module(library(uri/uri_code)).





%! 'dec-octet'(?Number:between(0,255))// .
% ```abnf
% dec-octet = DIGIT               ; 0-9
%           / %x31-39 DIGIT       ; 10-99
%           / "1" 2DIGIT          ; 100-199
%           / "2" %x30-34 DIGIT   ; 200-249
%           / "25" %x30-35        ; 250-255
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'dec-octet'(N) -->
  between_radix(dec(0), dec(255), dec(N)).



%! h16(?Number:between(0,65535))// .
% 16-bit hexadecimal.
%
% ```abnf
% h16 = 1*4HEXDIG
% ```
%
% @compat RFC 3986
% @compat RFC 3987

h16(N) -->
  'm*n'(1, 4, 'HEXDIG', N, [convert1(positional)]).



%! host(?Iri:boolean, ?Host:compound)// .
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
%  host = IP-literal / IPv4address /  reg-name
% ihost = IP-literal / IPv4address / ireg-name
% ```
%
% @compat RFC 3986
% @compat RFC 3987

host(_, Host) --> 'IP-literal'(Host).
host(_, Host) --> 'IPv4address'(Host).
host(I, Host) --> 'reg-name'(I, Host).



%! 'IP-literal'(?IpLiteral:compound)// .
% ```abnf
% IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'IP-literal'(IpLiteral) -->
  bracketed(square, ('IPv6address'(IpLiteral) ; 'IPvFuture'(IpLiteral))).



%! 'IPv4address'(?Ipv4Address:compound)// .
% ```abnf
% IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'IPv4address'(ipv4(N1,N2,N3,N4)) -->
  '#'(4, 'dec-octet', [N1,N2,N3,N4], [separator(dot)]).



%! 'IPv6address'(?Ipv6Address:compound)// .
% ```abnf
% IPv6address =                              6( h16 ":" ) ls32
%               /                       "::" 5( h16 ":" ) ls32
%               / [               h16 ] "::" 4( h16 ":" ) ls32
%               / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
%               / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
%               / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
%               / [ *4( h16 ":" ) h16 ] "::"              ls32
%               / [ *5( h16 ":" ) h16 ] "::"    h16
%               / [ *6( h16 ":" ) h16 ] "::"
% ```
%
% @compat RFC 3986
% @compat RFC 3987

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



%! ipv6_pattern(
%!   ?FormerH16s:between(0,7),
%!   ?DoubleColon:boolean,
%!   ?LatterH16s:between(0,6),
%!   ?Colon:boolean,
%!   ?Ls32s:between(0,1)
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



%! 'IPvFuture'(?IpLiteral:compound)// .
% ```abnf
% IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'IPvFuture'(ipvfuture(N,Cs)) -->
  "v",
  '+'('HEXDIG', Ws, []),
  {positional(N, Ws)},
  ".",
  '+'('IPvFuture_code', Cs, []).

% Both RFC 3986 and RFC 3987 make use of
% the 'non-IRI' definition of unreserved//2.
'IPvFuture_code'(C) --> unreserved(false, C).
'IPvFuture_code'(C) --> 'sub-delims'(C).
'IPvFuture_code'(C) --> colon(C).



%! 'reg-name'(?Iri:boolean, ?RegisteredName:compound)// .
% ```abnf
%  reg-name = *(  unreserved / pct-encoded / sub-delims )
% ireg-name = *( iunreserved / pct-encoded / sub-delims )
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'reg-name'(I, registered_name(RegisteredName)) -->
  dcg_string('reg-name_codes'(I), RegisteredName).

'reg-name_codes'(I, [H|T]) -->
  (   unreserved(I, H)
  ;   'pct-encoded'(H)
  ;   'sub-delims'(H)
  ), !,
  'reg-name_codes'(I, T).
'reg-name_codes'(_, []) --> "".



%! ls32(?IpAddress:compound)// .
% ```abnf
% ls32 = ( h16 ":" h16 ) / IPv4address
% ```
%
% @compat RFC 3986
% @compat RFC 3987

ls32(ipa(H16A,H16B)) -->
  h16(H16A),
  ":",
  h16(H16B).
ls32(IpAddress) -->
  'IPv4address'(IpAddress).
