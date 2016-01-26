:- module(rfc3987_gen, [iri_norm/2,test_iri/1]).

/** <module> RFC 3987 generators

urn:ftd:cogchar.org:2012:runtime#liftconfig
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/rfc2234), [
     'DIGIT'//2, % ?Weight:between(0,9), ?Code:code
     'HEXDIG'//1 % ?Weight:between(0,15)
   ]).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986)).
:- use_module(library(iri/rfc3987)).

test_iri('http://dbpedia.org/resource/Category:Politics').
test_iri('http://dbpedia.org/resource/Category%3APolitics').

iri_norm(X, Y) :-
  uri_components(X, uri_components(Scheme1,Authority,Path1,Query1,Fragment1)),
  uri_authority_components(Authority, uri_authority(User1,Password1,Host1,Port1)),
  maplist(
    ensure_atom,
    [Scheme1,Path1,Query1,Fragment1,User1,Password1,Host1,Port1],
    [Scheme2,Path2,Query2,Fragment2,User2,Password2,Host2,Port2]
  ),
  (   User2 == '',
      Password2 == ''
  ->  Userinfo = ''
  ;   User2 == ''
  ->  Userinfo = Password2
  ;   Password2 == ''
  ->  Userinfo = User2
  ;   atomic_list_concat([User2,Password2], :, Userinfo)
  ),
  D = iri{
        fragment: Fragment2,
        host: Host2,
        path: Path2,
        port: Port2,
        query: Query2,
        scheme: Scheme2,
        userinfo: Userinfo
      },
  once(atom_phrase('IRI_gen'(D), Y)).

% IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
'IRI_gen'(D) -->
  scheme_gen(D),
  ":",
  'ihier-part_gen'(D, D.scheme),
  ({D.query == ''} -> "" ; "?", iquery_gen(D)),
  ({D.fragment == ''} -> "" ; "#", ifragment_gen(D)).

% absolute-IRI = scheme ":" ihier-part [ "?" iquery ]
'absolute-IRI_gen'(D) -->
  scheme_gen(D),
  ":",
  'ihier-part_gen'(D, D.scheme),
  ({D.query == ''} -> "" ; "?", iquery_gen(D)).

% iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
iauthority_gen(D) -->
  (({D.userinfo == : ; D.userinfo == ''}) -> "" ; iuserinfo_gen(D), "@"),
  ihost_gen(D),
  % If the port subcomponent is empty or not given,
  % TCP port 80 (the reserved port for WWW services) is the default.
  (   {D.port == ''}
  ->  ""
  ;   {default_port(D.scheme, D.port)}
  ->  ""
  ;   ":",
      port_gen(D.port)
  ).

% ifragment = *( ipchar / "/" / "?" )
ifragment_gen(D) -->
  {atom_codes0(D.fragment, Cs)},
  *(ifragment_code, Cs).
ifragment_code(0'/) --> "/".
ifragment_code(0'?) --> "?".
ifragment_code(C)   --> ipchar(C).

% ihier-part = "//" iauthority ipath-abempty
%            / ipath-absolute
%            / ipath-rootless
%            / ipath-empty
'ihier-part_gen'(D, Scheme) -->
  {memberchk(Scheme, [http,https,irc])}, !,
  (   "//", iauthority_gen(D), 'ipath-abempty_gen'(D)
  ;   'ipath-absolute_gen'(D)
  ;   'ipath-rootless_gen'(D)
  ;   'ipath-empty_gen'(D)
  ).
'ihier-part_gen'(D, _) -->
  (   'ipath-absolute_gen'(D)
  ;   'ipath-rootless_gen'(D)
  ;   "//", iauthority_gen(D), 'ipath-abempty_gen'(D)
  ;   'ipath-empty_gen'(D)
  ).

% ihost = IP-literal / IPv4address / ireg-name
ihost_gen(D) --> 'ireg-name_gen'(D).
%ihost_gen(D) --> 'IP-literal_gen'(D).
%ihost_gen(D) --> 'IPv4address_gen'(D).

% ipath-abempty = *( "/" isegment )
% isegment = *ipchar
'ipath-abempty_gen'(D) -->
  {atom_codes0(D.path, Cs)},
  *(isegment_sep_code, Cs).

% ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
% isegment-nz = 1*ipchar
'ipath-absolute_gen'(D) -->
  {D.userinfo == '', D.port == '', atom_codes0(D.path, [0'/|Cs])},
  "/",
  *(isegment_sep_code, Cs).

% ipath-empty = 0<ipchar>
'ipath-empty_gen'(D) -->
  {D.userinfo == '', D.port == '', D.path = ''},
  "".

% ipath-rootless = isegment-nz *( "/" isegment )
'ipath-rootless_gen'(D) -->
  {D.userinfo == '', D.port == '', atom_codes0(D.path, Cs)},
  *(isegment_sep_code, Cs).

% iquery = *( ipchar / iprivate / "/" / "?" )
iquery_gen(D) -->
  {atom_codes0(D.query, Cs)},
  *(iquery_code, Cs).

% ireg-name = *( iunreserved / pct-encoded / sub-delims )
'ireg-name_gen'(D) -->
  {atom_codes0(D.host, Cs)},
  *(ireg_name_code, Cs).

% iuserinfo = *( iunreserved / pct-encoded / sub-delims / ":" )
iuserinfo_gen(D) -->
  {atom_codes0(D.userinfo, Cs)},
  *(iuserinfo_code, Cs).

% port = *DIGIT
port_gen(D) -->
  {atom_codes0(D.port, Cs)},
  *('DIGIT', _, Cs).

% scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
scheme_gen(D) -->
  {atom_codes0(D.scheme, Cs)},
  *(scheme_code, Cs).

atom_codes0(A, Cs2) :-
  atom_codes(A, Cs1),
  phrase(deesc, Cs1, Cs2).

deesc, [C] --> "%", 'HEXDIG'(H1), 'HEXDIG'(H2), !, {C is H1 * 16 + H2}, deesc.
deesc, [C] --> [C], !, deesc.
deesc      --> "".

default_port("http", 80).
default_port("https", 443).

ensure_atom(Var, '') :- var(Var), !.
ensure_atom(A, A).

isegment_sep_code(0'/) --> "/".
isegment_sep_code(C)   --> ipchar(C).
