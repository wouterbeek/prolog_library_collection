:- module(
  rfc6454,
  [
    has_same_origin/2,        % +Origin1, +Origin2
    'obs-fold'//0,
    origin/2,                 % +Uri, -Origin
    origin//1,                % -Origins
    'origin-list-or-null'//1, % -Origins
    'origin-list'//1,         % -Origins
    'serialized-origin'//1    % -Origin
  ]
).

/** <module> RFC 6454: Web Origin

@author Wouter Beek
@compat RFC 6454
@see http://tools.ietf.org/html/rfc6454
@version 2015/11-2016/01, 2016/12-2017/01, 2017/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code
     'CHAR'//1, % ?Code
     'CR'//0,
     'CRLF'//0,
     'CTL'//1, % ?Code
     'DIGIT'//1, % ?Weight
     'DIGIT'//2, % ?Weight, ?Code
     'DQUOTE'//0,
     'HEXDIG'//1, % ?Weight
     'HEXDIG'//2, % ?Weight, ?Code
     'HTAB'//0,
     'LF'//0,
     'OCTET'//1, % ?Code
     'SP'//0,
     'VCHAR'//1, % ?Code
     'WSP'//0
   ]).
:- use_module(library(default)).
:- use_module(library(http/http_client2)).
:- use_module(library(http/rfc4790)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986), [
     host//1,    % -Host
     port//1,    % -Port
     scheme//1,  % -Scheme
     uri_port//2 % ?Scheme, ?Port
   ]).
:- use_module(library(uuid)).

:- multifile
    uri:default_port/2.





%! has_same_origin(+Origin1, +Origin2) is semidet.
%
% Two origins are "the same" if, and only if, they are identical.  In
% particular:
%
%   o  If the two origins are scheme/host/port triples, the two origins
%      are the same if, and only if, they have identical schemes, hosts,
%      and ports.
%
%   o  An origin that is a globally unique identifier cannot be the same
%      as an origin that is a scheme/host/port triple.
%
%   Two URIs are same-origin if their origins are the same.
%
%      NOTE: A URI is not necessarily same-origin with itself.  For
%      example, a data URI [RFC2397] is not same-origin with itself
%      because data URIs do not use a server-based naming authority and
%      therefore have globally unique identifiers as origins.

has_same_origin(origin(Scheme1,Host1,Port1), origin(Scheme2,Host2,Port2)) :-
  maplist(==, [Scheme1,Host1,Port1], [Scheme2,Host2,Port2]), !.
has_same_origin(X1, Y1) :-
  maplist(is_uri, [X1,Y1]), !,
  maplist(origin, [X1,Y1], [X2,Y2]),
  has_same_origin(X2, Y2).



%! 'obs-fold'// is det.
%
% ```abnf
% obs-fold = CRLF ( SP / HTAB )   ; obsolete line folding
% ```

'obs-fold' -->
  'CRLF',
  ('SP' -> "" ; 'HTAB').



%! origin(+Uri, -Origin) is det.
%
% The origin of a URI is the value computed by the following
% algorithm:
%
%   1. If the URI does not use a hierarchical element as a naming
%   authority (see [RFC3986], Section 3.2) or if the URI is not an
%   absolute URI, then generate a fresh globally unique identifier and
%   return that value.
%
%   NOTE: Running this algorithm multiple times for the same URI can
%   produce different values each time.  Typically, user agents
%   compute the origin of, for example, an HTML document once and use
%   that origin for subsequent security checks rather than recomputing
%   the origin for each security check.
%
%   2. Let uri-scheme be the scheme component of the URI, converted
%   to lowercase.
%
%   3. If the implementation doesn't support the protocol given by
%   uri- scheme, then generate a fresh globally unique identifier and
%   return that value.
%
%   4. If uri-scheme is "file", the implementation MAY return an
%   implementation-defined value.
%
%   NOTE: Historically, user agents have granted content from the file
%   scheme a tremendous amount of privilege.  However, granting all
%   local files such wide privileges can lead to privilege escalation
%   attacks.  Some user agents have had success granting local files
%   directory-based privileges, but this approach has not been widely
%   adopted.  Other user agents use globally unique identifiers for
%   each file URI, which is the most secure option.
%
%   5. Let uri-host be the host component of the URI, converted to
%   lower case (using the i;ascii-casemap collation defined in
%   [RFC4790]).
%
%   NOTE: This document assumes that the user agent performs
%   Internationalizing Domain Names in Applications (IDNA) processing
%   and validation when constructing the URI.  In particular, this
%   document assumes the uri-host will contain only LDH labels because
%   the user agent will have already converted any non-ASCII labels to
%   their corresponding A-labels (see [RFC5890]).  For this reason,
%   origin-based security policies are sensitive to the IDNA algorithm
%   employed by the user agent.  See Section 8.4 for further
%   discussion.
%
%   6. If there is no port component of the URI:
%
%      1.  Let uri-port be the default port for the protocol given by
%      uri-scheme.  Otherwise:
%
%      2. Let uri-port be the port component of the URI.
%
%   7. Return the triple (uri-scheme, uri-host, uri-port).

origin(Uri, origin(Scheme,Host,Port)) :-
  is_uri(Uri), !,
  uri_components(Uri, uri_components(Scheme0,Auth,_,_,_)),
  downcase_atom(Scheme0, Scheme),
  uri_authority_components(Auth, uri_authority(_,_,Host0,Port0)),
  ascii_casemap(Host0, Host),
  (var(Port0) -> uri:default_port(Scheme, Port) ; Port = Port0).
origin(_, Id) :-
  uuid(Id).
  


%! origin(-Origins)// is det.
%
% ```abnf
% origin = "Origin:" OWS origin-list-or-null OWS
% ```

origin(Origins) -->
  'OWS',
  'origin-list-or-null'(Origins),
  'OWS'.



%! 'origin-list'(-Origins)// is det.
%
% ```abnf
% origin-list = serialized-origin *( SP serialized-origin )
% ```

'origin-list'([H|T]) -->
  'serialized-origin'(H), !,
  *(sep_serialized_origin, T), !.

sep_serialized_origin(Origin) -->
  'SP',
  'serialized-origin'(Origin).



%! 'origin-list-or-null'(-Origins)// is det.
%
% ```abnf
% origin-list-or-null = %x6E %x75 %x6C %x6C / origin-list
% ```

'origin-list-or-null'([]) -->
  atom_ci(null), !.
'origin-list-or-null'(Origins) -->
  'origin-list'(Origins).



%! 'OWS'// is det.
%
% ```abnf
% OWS = *( SP / HTAB / obs-fold )   ; "optional" whitespace
% ```

'OWS' -->
  *(ows), !.

ows --> 'SP'.
ows --> 'HTAB'.
ows --> 'obs-fold'.



%! 'serialized-origin'(-Origin)// is det.
%
% ```abnf
% serialized-origin = scheme "://" host [ ":" port ]
%                   ; <scheme>, <host>, <port> from RFC 3986
% ```

'serialized-origin'(origin(Scheme,Host,Port)) -->
  scheme(Scheme),
  "://",
  host(Host),
  uri_port(Scheme, Port).
