:- module(
  rfc6454,
  [
    has_same_origin/2, % +X:or([atom,dict])
                       % +Y:or([atom,dict])
    origin/2, % +Uri:atom
              % -Origin:or([atom,dict])
    origin//1, % ?Origins:list(dict)
    'origin-list-or-null'//1,
    'origin-list'//1
  ]
).

/** <module> RFC 6454: Web Origin

@author Wouter Beek
@compat RFC 6454
@license MIT License
@see http://tools.ietf.org/html/rfc6454
@version 2015/11
*/

:- use_module(library(apply)).
:- use_module(library(default)).
:- use_module(library(http/rfc4790)).
:- use_module(library(http/rfc6454_code)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986_component), [
     scheme//1, % ?Scheme:string
     host//1, % ?Host:compound
     port//1 % ?Port:nonneg
   ]).
:- use_module(library(uuid)).





%! has_same_origin(+X:or([atom,dict]), +Y:or([atom,dict])) is semidet.
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
  origin{scheme: Scheme1, host: Host1, port: Port1},
  origin{scheme: Scheme2, host: Host2, port: Port2}
):-
  maplist(==, [Scheme1,Host1,Port1], [Scheme2,Host2,Port2]), !.
has_same_origin(X1, Y1):-
  maplist(is_iri, [X1,Y1]), !,
  maplist(origin, [X1,Y1], [X2,Y2]),
  has_same_origin(X2, Y2).



%! origin(+Uri:atom, -Origin:or([atom,dict])) is det.
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

origin(Uri, Uid):-
  \+ uri_is_global(Uri), !,
  uuid(Uid).
origin(Uri, origin{scheme: Scheme, host: Host, port: Port}):-
  uri_components(Uri, uri_components(Scheme0,Auth,_,_,_)),
  downcase_atom(Scheme0, Scheme),
  uri_authority_components(Auth, uri_authority(_,_,Host0,Port)),
  ascii_casemap(Host0, Host),
  defval(Port, 80).



%! origin(?Origins:list(dict))// .
% ```abnf
% origin = "Origin:" OWS origin-list-or-null OWS
% ```

origin(L) --> "Origin:", 'OWS', 'origin-list-or-null'(L), 'OWS'.



%! 'origin-list'(?Origins:list(dict))// .
% ```abnf
% origin-list = serialized-origin *( SP serialized-origin )
% ```

'origin-list'([H|T]) --> 'serialized-origin'(H), !, origin_list(T).
origin_list([H|T]) --> 'SP', 'serialized-origin'(H), !, origin_list(T).
origin_list([])    --> "".



%! 'origin-list-or-null'(?Origins:list(dict))// .
% ```abnf
% origin-list-or-null = %x6E %x75 %x6C %x6C / origin-list
% ```

'origin-list-or-null'([]) --> "null", !.
'origin-list-or-null'(L) --> 'origin-list'(L).



%! 'serialized-origin'(?Origin:dict)// .
% ```abnf
% serialized-origin = scheme "://" host [ ":" port ]
%                   ; <scheme>, <host>, <port> from RFC 3986
% ```

'serialized-origin'(origin{scheme: Scheme, host: Host, port: Port}) -->
  scheme(Scheme),
  "://",
  host(Host),
  (":", port(Port) ; {Port = 80}).
