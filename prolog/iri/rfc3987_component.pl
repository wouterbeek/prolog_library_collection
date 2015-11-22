:- module(
  rfc3987_component,
  [
    iauthority//2, % +Scheme:string
                   % ?Authority:compound
    ifragment//1, % ?Code:code
    ihost//1, % ?Host:compound
    ipath//1, % ?Segments:list(string)
    iquery//1, % ?Code:code
    'isegment-nz-nc'//1, % ?Segment:string
    iuserinfo//1 % ?UserInfo:string
  ]
).

/** <module> RFC 3987: Component

@author Wouter Beek
@compat RFC 3987
@see tools.ietf.org/html/rfc3987
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(string_ext)).
:- use_module(library(iri/rfc3987_code)).
:- use_module(library(iri/rfc3987_token)).





%! iauthority(+Scheme:string, ?Authority:compound)// .
% ```abnf
% iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
% ```

iauthority(Scheme, authority(UserInfo,Host,Port)) -->
  (iuserinfo(UserInfo) -> "@" ; ""),
  ihost(Host),
  % If the port subcomponent is empty or not given,
  % TCP port 80 (the reserved port for WWW services) is the default.
  (":" -> port(Port) ; {default_port(Scheme, Port)}).



%! ihost(?Host:compound)// .
% ```abnf
% ihost = IP-literal / IPv4address / ireg-name
% ```

ihost(Host) --> 'IP-literal'(Host).
ihost(Host) --> 'IPv4address'(Host).
ihost(Host) --> 'ireg-name'(Host).



%! ifragment(?Code:code)// .
% ```abnf
% ifragment = *( ipchar / "/" / "?" )
% ```

ifragment(S) --> *(ifragment_code, S, [convert1(codes_string)]).
ifragment_code(C) --> ipchar(C).
ifragment_code(0'/) --> "/".
ifragment_code(0'?) --> "?".



%! ipath(?Segments:list(string))// .
% ```abnf
% ipath = ipath-abempty    ; begins with "/" or is empty
%       / ipath-absolute   ; begins with "/" but not "//"
%       / ipath-noscheme   ; begins with a non-colon segment
%       / ipath-rootless   ; begins with a segment
%       / ipath-empty      ; zero characters
% ```

% Begins with "/" or is empty.
ipath(L) --> 'ipath-abempty'(L).
% Begins with "/" but not "//".
ipath(L) --> 'ipath-absolute'(L).
% Begins with a non-colon segment
ipath(L) --> 'ipath-noscheme'(L).
% Begins with a segment
ipath(L) --> 'ipath-rootless'(L).
% Empty path (i.e., no segments).
ipath(L) --> 'ipath-empty'(L).



%! iquery(?Query:string)// .
% ``abnf
% iquery = *( ipchar / iprivate / "/" / "?" )
% ```

iquery(S) --> *(iquery_code, S, [convert1(codes_string)]).
iquery_code(C) --> ipchar(C).
iquery_code(C) --> iprivate(C).
iquery_code(0'/) --> "/".
iquery_code(0'?) --> "?".



%! iuserinfo(?UserInfo:string)// .
% ```abnf
% iuserinfo = *( iunreserved / pct-encoded / sub-delims / ":" )
% ```

iuserinfo(S) --> *(iuserinfo_code, S, [convert1(codes_string)]).
iuserinfo_code(C) --> iunreserved(C).
iuserinfo_code(C) --> 'pct-encoded'(C).
iuserinfo_code(C) --> 'sub-delims'(C).
iuserinfo_code(0':) --> ":".





% HELPERS %

%! default_port(+Scheme:string, -Port:oneof([443,80])) is det.
% The default port for the given scheme.

default_port("http", 80).
default_port("https", 443).
