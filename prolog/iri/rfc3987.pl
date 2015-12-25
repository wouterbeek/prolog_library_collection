:- module(
  rfc3987,
  [
    'absolute-IRI'//1, % -AbsoluteIri:dict
    'IRI'//1, % -Iri:dict
    'IRI-reference'//1 % -IriReference:dict
  ]
).

/** <module> RFC 3987: Internationalized Resource Identifiers (IRIs)

## Definitions

  * *Character*
    A member of a set of elements used for the organization,
    control, or representation of data.
  * *|character encoding|*
    A method of representing a sequence of characters as a sequence of octets.
    Also, a method of (unambiguously) converting a sequence of octets into
    a sequence of characters.
  * *|Character repertoire|*
    A set of characters.
  * *Charset*
    The name of a parameter or attribute used to identify
    a character encoding.
  * *|IRI reference|*
    An IRI reference may be absolute or relative.
    However, the "IRI" that results from such a reference only includes
    absolute IRIs; any relative IRI references are resolved to their
    absolute form.
  * *Octet*
    An ordered sequence of eight bits considered as a unit.
  * *|Presentation element|*
    A presentation form corresponding to a protocol element; for example,
    using a wider range of characters.
  * *|Protocol element|*
    Any portion of a message that affects processing of that message
    by the protocol in question.
  * *|Running text|*
    Human text (paragraphs, sentences, phrases) with syntax according to
    orthographic conventions of a natural language, as opposed to syntax
    defined for ease of processing by machines (e.g., markup,
    programming languages).
  * *|UCS: Universal Character Set|*
    The coded character set defined by ISO/IEC 10646 and the Unicode Standard.

@author Wouter Beek
@compat RFC 3987
@see http://tools.ietf.org/html/rfc3987
@version 2015/08, 2015/11-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(string_ext)).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986)).





%! 'absolute-IRI'(-AbsoluteIri:dict)// is det.
% ```abnf
% absolute-IRI = scheme ":" ihier-part [ "?" iquery ]
% ```

'absolute-IRI'(D) -->
  scheme(Scheme),
  ":",
  'ihier-part'(D0),
  {dict_pairs(D0, hier_part, T)},
  ("?" -> iquery(Query), {T = [query-Query|T0]} ; {T = T0}),
  {dict_pairs(D, iri, [scheme-Scheme|T])}.



%! iauthority(-Scheme:string, -Authority:dict)// is det.
% ```abnf
% iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
% ```

iauthority(Scheme. Auth) -->
  (iuserinfo(UserInfo), "@", {T = [userinfo-Userinfo]} ; {T = []}),
  ihost(Host), !,
  % If the port subcomponent is empty or not given,
  % TCP port 80 (the reserved port for WWW services) is the default.
  (":" -> port(Port) ; {default_port(Scheme, Port)}),
  {dict_pairs(Auth, authority, [host-Host,port-Port|T])}.



%! ifragment(-Fragment:string)// is det.
% ```abnf
% ifragment = *( ipchar / "/" / "?" )
% ```

ifragment(S) --> *(ifragment_code, Cs), {string_codes(S, Cs)}.
ifragment_code(C)   --> ipchar(C).
ifragment_code(0'/) --> "/".
ifragment_code(0'?) --> "?".



%! 'ihier-part'(-HierarchicalPart:dict)// .
% ```abnf
% ihier-part = "//" iauthority ipath-abempty
%            / ipath-absolute
%            / ipath-rootless
%            / ipath-empty
% ```

'ihier-part'(hier_part{authority: Auth, path: L, scheme: Scheme}) -->
  "//",
  iauthority(Scheme, Auth),
  'ipath-abempty'(L).
'ihier-part'(hier_part{path: L}) --> 'ipath-absolute'(L).
'ihier-part'(hier_part{path: L}) --> 'ipath-rootless'(L).
'ihier-part'(hier_part{path: L}) --> 'ipath-empty'(L).



%! ihost(-Host:dict)// is det.
% ```abnf
% ihost = IP-literal / IPv4address / ireg-name
% ```

ihost(Host) --> 'IP-literal'(Host), !.
ihost(Host) --> 'IPv4address'(Host), !.
ihost(Host) --> 'ireg-name'(Host).



%! ipath(-Segments:list(string))// is det.
% ```abnf
% ipath = ipath-abempty    ; begins with "/" or is empty
%       / ipath-absolute   ; begins with "/" but not "//"
%       / ipath-noscheme   ; begins with a non-colon segment
%       / ipath-rootless   ; begins with a segment
%       / ipath-empty      ; zero characters
% ```

% Begins with "/" or is empty.
ipath(L) --> 'ipath-abempty'(L), !.
% Begins with "/" but not "//".
ipath(L) --> 'ipath-absolute'(L), !.
% Begins with a non-colon segment
ipath(L) --> 'ipath-noscheme'(L), !.
% Begins with a segment
ipath(L) --> 'ipath-rootless'(L), !.
% Empty path (i.e., no segments).
ipath(L) --> 'ipath-empty'(L).



%! 'ipath-abempty'(-Segments:list(string))// is det.
% ```abnf
% ipath-abempty = *( "/" isegment )
% ```

'ipath-abempty'(L) --> *(sep_isegment, L).



%! 'ipath-absolute'(-Segments:list(string))// is det.
% ```abnf
% ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
% ```

'ipath-absolute'(L) -->
  "/", ('isegment-nz'(H) -> *(sep_isegment, T), {L = [H|T]} ; {L = []}).



%! 'ipath-empty'(-Segments:list(string))// is det.
% ```abnf
% ipath-empty = 0<ipchar>
% ```

'ipath-empty'([]) --> "".



%! 'ipath-noscheme'(-Segments:list(string))// is det.
% ```abnf
% ipath-noscheme = isegment-nz-nc *( "/" isegment )
% ```

'ipath-noscheme'([H|T]) --> 'isegment-nz-nc'(H), *(sep_isegment, T).



%! 'ipath-rootless'(-Segments:list(string))// is det.
% ```abnf
% ipath-rootless = isegment-nz *( "/" isegment )
% ```

'ipath-rootless'([H|T]) --> 'segment-nz'(H), *(sep_isegment, T).



%! ipchar(-Code:code)// .
% ```abnf
% ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
% ```

ipchar(C) --> iunreserved(C).
ipchar(C) --> 'pct-encoded'(C).
ipchar(C) --> 'sub-delims'(C).
ipchar(0':) --> ":".
ipchar(0'@) --> "@".



%! iprivate(-Code:code)// .
% ```abnf
% iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
% ```

iprivate(C) --> between_code_radix(hex('E000'),   hex('F8FF'),   C).
iprivate(C) --> between_code_radix(hex('F0000'),  hex('FFFFD'),  C).
iprivate(C) --> between_code_radix(hex('100000'), hex('10FFFD'), C).



%! iquery(-Query:string)// is det.
% ``abnf
% iquery = *( ipchar / iprivate / "/" / "?" )
% ```

iquery(S) --> *(iquery_code, Cs), {string_codes(S, Cs)}.
iquery_code(C)   --> ipchar(C).
iquery_code(C)   --> iprivate(C).
iquery_code(0'/) --> "/".
iquery_code(0'?) --> "?".



%! 'ireg-name'(-RegisteredName:string)// is det.
% ```abnf
% ireg-name = *( iunreserved / pct-encoded / sub-delims )
% ```

'ireg-name'(S) --> *(ireg_name_code, Cs), {string_codes(S, Cs)}.
ireg_name_code(C) --> iunreserved(C).
ireg_name_code(C) --> 'pct-encoded'(C).
ireg_name_code(C) --> 'sub-delims'(C).



%! 'irelative-part'(-RelativePart:dict)// is det.
% ```abnf
% irelative-part = "//" iauthority ipath-abempty
%                / ipath-absolute
%                / ipath-noscheme
%                / ipath-empty
% ```

'irelative-part'(relative_part{authority: Auth, path: L, scheme: Scheme}) -->
  "//",
  iauthority(Scheme, Auth),
  'ipath-abempty'(L), !.
'irelative-part'(relative_part{path: L}) --> 'ipath-absolute'(L), !.
'irelative-part'(relative_part{path: L}) --> 'ipath-noscheme'(L), !.
'irelative-part'(relative_part{path: L}) --> 'ipath-empty'(L).



%! 'IRI'(-Iri:dict)// is det.
% ```abnf
% IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
% ```

'IRI'(D) -->
  scheme(Scheme),
  ":",
  'ihier-part'(D0),
  {dict_pairs(D0, hier_part, T0)},
  ("?" -> iquery(Query) ; ""),
  ("#" -> ifragment(Frag) ; ""),
  {
    L0 = [query-Query,fragment-Frag,scheme-Scheme|T0],
    exclude(pair_has_var_value, L0, L),
    dict_pairs(D, iri, L)
  }.



%! 'IRI-reference'(-IriReference:dict)// is det.
% There are two types of IRI reference: (1) IRI, (2) IRI relative reference.
%
% ```abnf
% IRI-reference = IRI / irelative-ref
% ```

'IRI-reference'(D) --> 'IRI'(D), !.
'IRI-reference'(D) --> 'irelative-ref'(D).



%! 'irelative-ref'(-RelativeReference:dict)// is det.
% Relative IRI reference.
%
% ```abnf
% irelative-ref = irelative-part [ "?" iquery ] [ "#" ifragment ]
% ```

'irelative-ref'(D) -->
  'irelative-part'(D0),
  {dict_pairs(D0, relative_part, T0)},
  ("?" -> iquery(Query) ; ""),
  ("#" -> ifragment(Frag) ; ""),
  {
    L0 = [fragment-Frag,query-Query,scheme-Scheme|T0],
    exclude(pair_has_var_value, L0, L),
    dict_pairs(D, relative_ref, L)
  }.



%! isegment(-Segment:string)// is det.
% ```abnf
% isegment = *ipchar
% ```

isegment(S) --> *(ipchar, Cs), {string_codes(S, Cs)}.



%! 'isegment-nz'(-Segment:string)// is det.
% ```abnf
% isegment-nz = 1*ipchar
% ```

'isegment-nz'(S) --> +(ipchar, Cs), {string_codes(S, Cs)}.



%! 'isegment-nz-nc'(-Segment:string)// is det.
% ```abnf
% isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims / "@" )
%                ; non-zero-length segment without any colon ":"
% ```

'isegment-nz-nc'(S) --> +(isegment_nz_nc_code, Cs), {string_codes(S, Cs)}.
isegment_nz_nc_code(C)   --> iunreserved(C).
isegment_nz_nc_code(C)   --> 'pct-encoded'(C).
isegment_nz_nc_code(C)   --> 'sub-delims'(C).
isegment_nz_nc_code(0'@) --> "@".



%! iunreserved(-Code:code)// .
% ```abnf
% iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
% ```

iunreserved(C) --> unreserved(C).
iunreserved(C) --> ucschar(C).



%! iuserinfo(-UserInfo:string)// is det.
% ```abnf
% iuserinfo = *( iunreserved / pct-encoded / sub-delims / ":" )
% ```

iuserinfo(S) --> *(iuserinfo_code, Cs), {string_codes(S, Cs)}.
iuserinfo_code(C) -->   iunreserved(C).
iuserinfo_code(C) -->   'pct-encoded'(C).
iuserinfo_code(C) -->   'sub-delims'(C).
iuserinfo_code(0':) --> ":".



%! ucschar(-Code:code)// .
% ```abnf
% ucschar = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
%         / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
%         / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
%         / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
%         / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
%         / %xD0000-DFFFD / %xE1000-EFFFD
% ```

ucschar(C) --> between_code_radix(hex('A0'),    hex('D7FF'),  C).
ucschar(C) --> between_code_radix(hex('F900'),  hex('FDCF'),  C).
ucschar(C) --> between_code_radix(hex('FDF0'),  hex('FFEF'),  C).
ucschar(C) --> between_code_radix(hex('10000'), hex('1FFFD'), C).
ucschar(C) --> between_code_radix(hex('20000'), hex('2FFFD'), C).
ucschar(C) --> between_code_radix(hex('30000'), hex('3FFFD'), C).
ucschar(C) --> between_code_radix(hex('40000'), hex('4FFFD'), C).
ucschar(C) --> between_code_radix(hex('50000'), hex('5FFFD'), C).
ucschar(C) --> between_code_radix(hex('60000'), hex('6FFFD'), C).
ucschar(C) --> between_code_radix(hex('70000'), hex('7FFFD'), C).
ucschar(C) --> between_code_radix(hex('80000'), hex('8FFFD'), C).
ucschar(C) --> between_code_radix(hex('90000'), hex('9FFFD'), C).
ucschar(C) --> between_code_radix(hex('A0000'), hex('AFFFD'), C).
ucschar(C) --> between_code_radix(hex('B0000'), hex('BFFFD'), C).
ucschar(C) --> between_code_radix(hex('C0000'), hex('CFFFD'), C).
ucschar(C) --> between_code_radix(hex('D0000'), hex('DFFFD'), C).
ucschar(C) --> between_code_radix(hex('E1000'), hex('EFFFD'), C).





% HELPERS %

%! default_port(+Scheme:string, -Port:oneof([443,80])) is det.
% The default port for the given scheme.

default_port("http", 80).
default_port("https", 443).


sep_isegment(S) --> "/", isegment(S).
