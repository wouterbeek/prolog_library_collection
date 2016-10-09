:- module(
  rfc6454,
  [
    has_same_origin/2,        % +Uri1, +Uri2
    'obs-fold'//0,
    origin/2,                 % +Uri, -Origin:dict
    origin//1,                % -Origins:list(dict)
    'origin-list-or-null'//1, % -Origins:list(dict)
    'origin-list'//1,         % -Origins:list(dict)
    'OWS'//0,
    'serialized-origin'//1    % -Origin:dict
  ]
).

/** <module> RFC 6454: Web Origin

@author Wouter Beek
@compat RFC 6454
@license MIT License
@see http://tools.ietf.org/html/rfc6454
@version 2015/11-2016/01
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
     'DIGIT'//2, % ?Weight:between(0,9), ?Code
     'DQUOTE'//0,
     'HEXDIG'//1, % ?Weight
     'HEXDIG'//2, % ?Weight:between(0,9), ?Code
     'HTAB'//0,
     'LF'//0,
     'OCTET'//1, % ?Code
     'SP'//0,
     'VCHAR'//1, % ?Code
     'WSP'//0
   ]).
:- use_module(library(default)).
:- use_module(library(http/rfc4790)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986), [
     host//1, % -Host:dict
     port//1, % -Port:nonneg
     scheme//1 % -Scheme:string
   ]).
:- use_module(library(uuid)).





%! has_same_origin(+Uri1, +Uri2) is semidet.
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

has_same_origin(
  _{'uri:scheme': Scheme1, 'uri:host': Host1, 'uri:port': Port1},
  _{'uri:scheme': Scheme2, 'uri:host': Host2, 'uri:port': Port2}
) :-
  maplist(==, [Scheme1,Host1,Port1], [Scheme2,Host2,Port2]), !.
has_same_origin(X1, Y1) :-
  maplist(uri_is_global, [X1,Y1]), !,
  maplist(origin, [X1,Y1], [X2,Y2]),
  has_same_origin(X2, Y2).



%! 'obs-fold'// .
% ```abnf
% obs-fold = CRLF ( SP / HTAB )   ; obsolete line folding
% ```

'obs-fold' --> 'CRLF', ('SP' ; 'HTAB').



%! origin(+Uri, -Origin:dict) is det.
% The origin of a URI is the value computed by the following algorithm:
%   1.  If the URI does not use a hierarchical element as a naming
%       authority (see [RFC3986], Section 3.2) or if the URI is not an
%       absolute URI, then generate a fresh globally unique identifier
%       and return that value.
%
%          NOTE: Running this algorithm multiple times for the same URI
%          can produce different values each time.  Typically, user
%          agents compute the origin of, for example, an HTML document
%          once and use that origin for subsequent security checks rather
%          than recomputing the origin for each security check.
%
%   2.  Let uri-scheme be the scheme component of the URI, converted to
%       lowercase.
%
%   3.  If the implementation doesn't support the protocol given by uri-
%       scheme, then generate a fresh globally unique identifier and
%       return that value.
%
%   4.  If uri-scheme is "file", the implementation MAY return an
%       implementation-defined value.
%
%          NOTE: Historically, user agents have granted content from the
%          file scheme a tremendous amount of privilege.  However,
%          granting all local files such wide privileges can lead to
%          privilege escalation attacks.  Some user agents have had
%          success granting local files directory-based privileges, but
%          this approach has not been widely adopted.  Other user agents
%          use globally unique identifiers for each file URI, which is
%          the most secure option.
%
%   5.  Let uri-host be the host component of the URI, converted to lower
%       case (using the i;ascii-casemap collation defined in [RFC4790]).
%
%          NOTE: This document assumes that the user agent performs
%          Internationalizing Domain Names in Applications (IDNA)
%          processing and validation when constructing the URI.  In
%          particular, this document assumes the uri-host will contain
%          only LDH labels because the user agent will have already
%          converted any non-ASCII labels to their corresponding A-labels
%          (see [RFC5890]).  For this reason, origin-based security
%          policies are sensitive to the IDNA algorithm employed by the
%          user agent.  See Section 8.4 for further discussion.
%
%   6.  If there is no port component of the URI:
%
%       1.  Let uri-port be the default port for the protocol given by
%           uri-scheme.
%
%       Otherwise:
%
%       2.  Let uri-port be the port component of the URI.
%
%   7.  Return the triple (uri-scheme, uri-host, uri-port).

origin(Uri, D2) :-
  D1 = _{'@type': 'llo:origin'},
  (   uri_is_global(Uri)
  ->  uri_components(Uri, uri_components(Scheme1,Auth,_,_,_)),
      downcase_atom(Scheme1, Scheme2),
      uri_authority_components(Auth, uri_authority(_,_,Host1,Port)),
      ascii_casemap(Host1, Host2),
      defval(Port, 80),
      maplist(atom_string, [Scheme3,Host3], [Scheme2,Host2]),
      D2 = D1.put(_{'uri:scheme': Scheme3, 'uri:host': Host3, 'uri:port': Port})
  ;   uuid(Uid1),
      atom_string(Uid1, Uid2),
      D2 = D1.put(_{'llo:uuid': Uid2})
  ).
  


%! origin(-Origins:list(dict))// is det.
% ```abnf
% origin = "Origin:" OWS origin-list-or-null OWS
% ```

origin(L) --> "Origin:", 'OWS', 'origin-list-or-null'(L), 'OWS'.



%! 'origin-list'(-Origins:list(dict))// is det.
% ```abnf
% origin-list = serialized-origin *( SP serialized-origin )
% ```

'origin-list'([H|T]) --> 'serialized-origin'(H), !, *(serialized_origin, T).

serialized_origin(X) --> 'SP', 'serialized-origin'(X).



%! 'origin-list-or-null'(-Origins:list(dict))// is det.
% ```abnf
% origin-list-or-null = %x6E %x75 %x6C %x6C / origin-list
% ```

'origin-list-or-null'([]) --> atom_ci(null), !.
'origin-list-or-null'(L)  --> 'origin-list'(L).



%! 'OWS'// is det.
% ```abnf
% OWS = *( SP / HTAB / obs-fold )   ; "optional" whitespace
% ```

'OWS' --> *(ows).

ows --> 'SP'.
ows --> 'HTAB'.
ows --> 'obs-fold'.



%! 'serialized-origin'(-Origin:dict)// is det.
% ```abnf
% serialized-origin = scheme "://" host [ ":" port ]
%                   ; <scheme>, <host>, <port> from RFC 3986
% ```

'serialized-origin'(_{'@type': 'llo:origin', 'uri:scheme': Scheme, 'uri:host': Host, 'uri:port': Port}) -->
  scheme(Scheme),
  "://",
  host(Host),
  (":", port(Port) ; {Port = 80}).
