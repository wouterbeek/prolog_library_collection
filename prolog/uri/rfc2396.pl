:- module(
  rfc2396,
  [
    abs_path//1, % ?Path:list(string)
    absoluteURI//1, % -Uri:dict
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
    relativeURI//1, % -Uri:dict
    segment//1, % ?Segment:string
    server//3, % ?UserInfo:string
               % ?Host:or([list(nonneg),list(string)])
               % ?Port:nonneg
    'URI-reference'//1 % -Uri:dict
  ]
).

/** <module> RFC 2396: Uniform Resource Identifiers (URI): Generic Syntax

@author Wouter Beek
@compat RFC 2396
@deprecated Use module `rfc3986` instead.
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(uri/rfc2396_code)).
:- use_module(library(uri/rfc2396_token)).





%! abs_path(?Path:list(string))// .
% ```abnf
% abs_path = "/"  path_segments
% ```

abs_path(L) --> "/", path_segments(L).



%! absoluteURI(-Uri:dict)// is det.
% ```abnf
% absoluteURI = scheme ":" ( hier_part | opaque_part )
% ```

absoluteURI(absolute_uri{scheme: Scheme, opaque: Opaque}) -->
  scheme(Scheme),
  ":",
  opaque_part(Opaque), !.
absoluteURI(Uri) -->
  scheme(Scheme),
  ":",
  hier_part(Auth, Path, Query),
  {dict_remove_uninstantiated(
    absolute_uri{scheme: Scheme, authority: Auth, path: Path, query: Query},
    Uri
  )}.



%! authority(?Authority:compound)// .
% ```abnf
% authority = server | reg_name
% ```

authority(auth(RegisteredName))     --> reg_name(RegisteredName), !.
authority(auth(UserInfo,Host,Port)) --> server(UserInfo, Host, Port).



%! hier_part(?Authority:compound, ?Path:list(string), ?Query:string)// .
% ```abnf
% hier_part = ( net_path | abs_path ) [ "?" query ]
% ```

hier_part(Auth, Path, Query) -->
  (net_path(Auth, Path), ! ; abs_path(Path)),
  ("?" -> query(Query) ; "").



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
  *(domainlabel_sep, L1), toplabel(X), ?("."), {append(L1, [X], L)}.
domainlabel_sep(C) --> domainlabel(C), ".".



%! hostport(?Host:or([list(nonneg),list(string)]), ?Port:nonneg)// .
% ```abnf
% hostport = host [ ":" port ]
% ```

hostport(Host, Port) --> host(Host), (":" -> port(Port) ; {Port = 80}).



%! net_path(?Authority:compound, ?Path:list(list(string))// .
% ```abnf
% net_path = "//" authority [ abs_path ]
% ```

net_path(Auth, Path) -->
  "//", authority(Auth), (abs_path(Path) -> "" ; {Path = []}).



%! path(?Path:list(string))// .
% ```abnf
% path = [ abs_path | opaque_part ]
% ```

path(Path)         --> abs_path(Path), !.
path([OpaquePart]) --> opaque_part(OpaquePart), !.
path([])           --> "".



%! path_segments(?Segments:list(string))// .
% ```abnf
% path_segments = segment *( "/" segment )
% ```

path_segments([H|T]) --> segment(H), *(sep_segment, T).
sep_segment(X) --> "/", segment(X).



%! rel_path(?Path:list(string))// .
% ```abnf
% rel_path = rel_segment [ abs_path ]
% ```

rel_path([H|T]) --> rel_segment(H), (abs_path(T) -> "" ; {T = []}).



%! relativeURI(-Uri:dict)// .
% ```abnf
% relativeURI = ( net_path | abs_path | rel_path ) [ "?" query ]
% ```

relativeURI(Uri) -->
  (net_path(Auth, Path), ! ; abs_path(Path), ! ; rel_path(Path)),
  ("?" -> query(Query) ; ""),
  {dict_remove_uninstantiated(
    relative_uri{authority: Auth, path: Path, query: Query},
    Uri
  )}.



%! segment(?Segment:list(string))// .
% ```abnf
% segment = *pchar *( ";" param )
% ```

segment([H|T]) --> *(pchar, Cs), {string_codes(H, Cs)}, *(sep_param, T).
sep_param(S) --> ";", param(S).



%! server(
%!   ?UserInfo:string,
%!   ?Host:or([list(nonneg),list(string)]),
%!   ?Port:nonneg
%! )// .
% ```abnf
% server = [ [ userinfo "@" ] hostport ]
% ```

server(UserInfo, Host, Port) -->
  (userinfo(UserInfo) -> "@" ; ""), !,
  hostport(Host, Port).
server(_, _, _) --> "".



%! 'URI-reference'(-Uri:dict)// is det.
% ```abnf
% URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
% ```

'URI-reference'(Uri2) -->
  (absoluteURI(Uri1), ! ; relativeURI(Uri1)),
  ("#" -> fragment(Frag), {Uri2 = Uri1.put(fragment, Frag)} ; {Uri2 = Uri1}).
