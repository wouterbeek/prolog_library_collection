:- module(
  rfc3986_component,
  [
    authority//2, % ?Scheme:string
                  % ?Authority:compound
    fragment//1, % ?Fragment:string
    host//1, % ?Host:compound
    path//1, % ?Segments:list(string)
    port//1, % ?Port:nonneg
    query//1, % ?Query:string
    scheme//1, % ?Scheme:string
    userinfo//1 % ?UserInfo:string
  ]
).

/** <module> RFC 3986: Component

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2015/11
*/

:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234_re)).
:- use_module(library(string_ext)).
:- use_module(library(uri/rfc3986_code)).
:- use_module(library(uri/rfc3986_token)).





%! authority(?Scheme:string, ?Authority:compound)// .
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

authority(Scheme, authority(UserInfo,Host,Port)) -->
  (userinfo(UserInfo) -> "@" ; ""),
  host(Host),
  % If the port subcomponent is empty or not given,
  % TCP port 80 (the reserved port for WWW services) is the default.
  (":" -> port(Port) ; {default_port(Scheme, Port)}).



%! fragment(?Fragment:string)// .
% ```anbf
% fragment = *( pchar / "/" / "?" )
% ```

fragment(S) --> dcg_string(fragment_codes, S).
fragment_codes([H|T])   --> pchar(H), !, fragment_codes(T).
fragment_codes([0'/|T]) --> "/",      !, fragment_codes(T).
fragment_codes([0'?|T]) --> "?",      !, fragment_codes(T).
fragment_codes([])      --> "".



%! host(?Host:compound)// .
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

host(Host) --> 'IP-literal'(Host).
host(Host) --> 'IPv4address'(Host).
host(Host) --> 'reg-name'(Host).



%! path(?Segments:list(string))// .
% ```abnf
%  path =  path-abempty    ; begins with "/" or is empty
%       /  path-absolute   ; begins with "/" but not "//"
%       /  path-noscheme   ; begins with a non-colon segment
%       /  path-rootless   ; begins with a segment
%       /  path-empty      ; zero characters
% ```

% Begins with "/" or is empty.
path(L) --> 'path-abempty'(L).
% Begins with "/" but not "//".
path(L) --> 'path-absolute'(L).
% Begins with a non-colon segment
path(L) --> 'path-noscheme'(L).
% Begins with a segment
path(L) --> 'path-rootless'(L).
% Empty path (i.e., no segments).
path(L) --> 'path-empty'(L).



%! port(?Port:nonneg)// .
% ```abnf
% port = *DIGIT
% ```

port(N) --> '*DIGIT'(N).



%! query(?Query:string)// .
% ```abnf
% query = *( pchar / "/" / "?" )
% ```

query(S) --> dcg_string(query_codes, S).
query_codes([H|T])   --> pchar(H), !, query_codes(T).
query_codes([0'/|T]) --> "/",      !, query_codes(T).
query_codes([0'?|T]) --> "?",      !, query_codes(T).
query_codes([])      --> "".



%! scheme(?Scheme:string)// .
% An US-ASCII letter, followed by a sequence consisting of
% US-ASCII letters, digits, plus, dash, and dot characters.
%
% ```abnf
% scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
% ```

scheme(S) --> dcg_string(scheme_codes1, S).
scheme_codes1([H|T])   --> 'ALPHA'(H),    !, scheme_codes2(T).
scheme_codes2([H|T])   --> 'ALPHA'(H),    !, scheme_codes2(T).
scheme_codes2([H|T])   --> 'DIGIT'(_, H), !, scheme_codes2(T).
scheme_codes2([0'+|T]) --> "+",           !, scheme_codes2(T).
scheme_codes2([0'-|T]) --> "-",           !, scheme_codes2(T).
scheme_codes2([0'.|T]) --> ".",           !, scheme_codes2(T).
scheme_codes2([])      --> "".



%! userinfo(?UserInfo:string)// .
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

userinfo(S) --> dcg_string(userinfo_codes, S).
userinfo_codes([H|T])   --> unreserved(H),    !, userinfo_codes(T).
userinfo_codes([H|T])   --> 'pct-encoded'(H), !, userinfo_codes(T).
userinfo_codes([H|T])   --> 'sub-delims'(H),  !, userinfo_codes(T).
userinfo_codes([0':|T]) --> ":",              !, userinfo_codes(T).
userinfo_codes([])      --> "".





% HELPERS %

%! default_port(+Scheme:string, -Port:oneof([443,80])) is det.
% The default port for the given scheme.

default_port("http",  80 ).
default_port("https", 443).
