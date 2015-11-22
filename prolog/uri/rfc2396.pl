:- module(
  rfc2396,
  [
    abs_path//1, % ?Path:list(string)
    absoluteURI//2, % ?Scheme:string
                    % ?OpaquePart:string
    absoluteURI//4, % ?Scheme:string
                    % ?Authority:compound
                    % ?Path:list(string)
                    % ?Query:string
    authority//1, % ?Authority:compound
    hier_part//3, % ?Authority:compound
                  % ?Path:list(string)
                  % ?Query:string
    host//1, % ?Host:or([list(nonneg),list(string)])
    hostname//1, % ?Labels:list(string)
    hostport//2, % ?Host:or([list(nonneg),list(string)])
                 % ?Port:nonneg
    net_path//2, % ?Authority:compound
                 % ?Path:list(string)
    path//1, % ?Path:list(string)
    path_segments//1, % ?Segments:list(string)
    rel_path//1, % ?Path:list(string)
    relativeURI//3, % ?Authority:compound
                    % ?Path:list(string)
                    % ?Query:string
    segment//1, % ?Segment:string
    server//3, % ?UserInfo:string
               % ?Host:or([list(nonneg),list(string)])
               % ?Port:nonneg
    'URI-reference'//3, % ?Scheme:string
                        % ?OpaquePart:string
                        % ?Fragment:string
    'URI-reference'//5 % ?Scheme:string
                       % ?Authority:compound
                       % ?Path:list(string)
                       % ?Query:string
                       % ?Fragment:string
  ]
).

/** <module> RFC 2396: Uniform Resource Identifiers (URI): Generic Syntax

@author Wouter Beek
@compat RFC 2396
@deprecated Use module `rfc3986` instead.
@version 2015/11
*/

:- use_module(library(dcg/dcg_re)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(uri/rfc2396_code)).
:- use_module(library(uri/rfc2396_token)).





%! abs_path(?Path:list(string))// .
% ```abnf
% abs_path = "/"  path_segments
% ```

abs_path(L) --> "/", path_segments(L).



%! absoluteURI(?Scheme:string, ?OpaquePart:string)// .
%! absoluteURI(
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Path:list(string),
%!   ?Query:string
%! )// .
% ```abnf
% absoluteURI = scheme ":" ( hier_part | opaque_part )
% ```

absoluteURI(Scheme, Opaque) --> scheme(Scheme), ":", opaque_part(Opaque).
absoluteURI(Scheme, Auth, Path, Query) -->
  scheme(Scheme),
  ":",
  hier_part(Auth, Path, Query).



%! authority(?Authority:compound)// .
% ```abnf
% authority = server | reg_name
% ```

authority(auth(UserInfo,Host,Port)) --> server(UserInfo, Host, Port).
authority(auth(RegisteredName)) --> reg_name(RegisteredName).



%! hier_part(?Authority:compound, ?Path:list(string), ?Query:string)// .
% ```abnf
% hier_part = ( net_path | abs_path ) [ "?" query ]
% ```

hier_part(Auth, Path, Query) -->
  (net_path(Auth, Path) ; abs_path(Path)), !,
  ("?", query(Query), ! ; "").



%! host(?Host:or([list(nonneg),list(string)]))// .
% ```abnf
% host = hostname | IPv4address
% ```

host(Ls) --> hostname(Ls), !.
host(Ns) --> 'IPv4address'(Ns).



%! hostname(?Labels:list(string))// .
% ```abnf
% hostname = *( domainlabel "." ) toplabel [ "." ]
% ```

hostname(L) -->
  domainlabels(L0),
  toplabel(Z),
  (".", ! ; ""),
  {append(L0, [Z], L)}.
domainlabels([H|T]) --> domainlabel(H), !, domainlabels(T).
domainlabels([]) --> "".



%! hostport(?Host:or([list(nonneg),list(string)]), ?Port:nonneg)// .
% ```abnf
% hostport = host [ ":" port ]
% ```

hostport(Host, Port) -->
  host(Host),
  (":", !, port(Port) ; {Port = 80}).



%! net_path(?Authority:compound, ?Path:list(list(string))// .
% ```abnf
% net_path = "//" authority [ abs_path ]
% ```

net_path(Auth, Path) -->
  "//",
  authority(Auth),
  (abs_path(Path), ! ; {Path = []}).



%! path(?Path:list(string))// .
% ```abnf
% path = [ abs_path | opaque_part ]
% ```

path(Path) --> abs_path(Path), !.
path([OpaquePart]) --> opaque_part(OpaquePart), !.
path([]) --> "".



%! path_segments(?Segments:list(string))// .
% ```abnf
% path_segments = segment *( "/" segment )
% ```

path_segments([H|T]) --> segment(H), segments0(T).
segments0([H|T]) --> "/", !, segment(H), segments0(T).
segments0([]) --> "".



%! rel_path(?Path:list(string))// .
% ```abnf
% rel_path = rel_segment [ abs_path ]
% ```

rel_path([H|T]) --> rel_segment(H), ?(abs_path, T).



%! relativeURI(?Authority:compound, ?Path:list(string), ?Query:string)// .
% ```abnf
% relativeURI = ( net_path | abs_path | rel_path ) [ "?" query ]
% ```

relativeURI(Auth, Path, Query) -->
  (net_path(Auth, Path) ; abs_path(Path) ; rel_path(Path)), !,
  ("?" -> query(Query) ; "").



%! segment(?Segment:list(string))// .
% ```abnf
% segment = *pchar *( ";" param )
% ```

segment(S) --> param(H), !, params(T), {string_list_concat([H|T], ";", S)}.
params([H|T]) --> ";",   !, param(H), params([H|T]).
params([])    --> "".



%! server(
%!   ?UserInfo:string,
%!   ?Host:or([list(nonneg),list(string)]),
%!   ?Port:nonneg
%! )// .
% ```abnf
% server = [ [ userinfo "@" ] hostport ]
% ```

server(UserInfo, Host, Port) -->
  ((userinfo(UserInfo) -> "@" ; "") -> hostport(Host, Port) ; "").



%! 'URI-reference'(?Scheme:string, ?OpaquePart:string, ?Fragment:string)// .
%! 'URI-reference'(
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Path:list(string),
%!   ?Query:string,
%!   ?Fragment:string
%! )// .
% ```abnf
% URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
% ```

'URI-reference'(Scheme, OpaquePart, Frag) -->
  absoluteURI(Scheme, OpaquePart),
  ("#", fragment(Frag) ; "").
'URI-reference'(_, Auth, Path, Query, Frag) -->
  relativeURI(Auth, Path, Query),
  ("#", fragment(Frag) ; "").
