:- module(
  rfc3987,
  [
    'absolute-IRI'//1,  % -Uri:compound
    ipchar//1,          % ?Code:code
    iprivate//1,        % ?Code:code
    iquery_code//1,     % ?Code:code
    ireg_name_code//1,  % ?Code:code
    'IRI'//1,           % -Uri:compound
    'IRI-reference'//1, % -Uri:compound
    iunreserved//1,     % ?Code:code
    iuserinfo_code//1   % ?Code:code
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
@version 2015/08, 2015/11-2015/12, 2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(string_ext)).
:- use_module(library(uri)).
:- use_module(library(uri/rfc3986)).





%! 'absolute-IRI'(-Uri:compound)// is det.
%
% ```abnf
% absolute-IRI = scheme ":" ihier-part [ "?" iquery ]
% ```

'absolute-IRI'(uri(Scheme,Auth,Segments,Query,_)) -->
  scheme(Scheme),
  ":",
  'ihier-part'(Scheme, Auth, Segments),
  ("?" -> iquery(Query) ; "").



%! iauthority(+Scheme:atom, -Auth:compound)// is det.
%
% ```abnf
% iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
% ```

iauthority(Scheme, auth(User,_,Host,Port)) -->
  (iuserinfo(User) -> "@" ; ""),
  ihost(Host), !,
  % If the port subcomponent is empty or not given, TCP port 80 (the
  % reserved port for WWW services) is the default.
  (":" -> port(Port) ; {default_port(Scheme, Port)}).



%! ifragment(-Frag:atom)// is det.
%
% ```abnf
% ifragment = *( ipchar / "/" / "?" )
% ```

ifragment(Frag) -->
  *(ifragment_code, Cs), !,
  {atom_codes(Frag, Cs)}.

ifragment_code(C)   --> ipchar(C).
ifragment_code(0'/) --> "/".
ifragment_code(0'?) --> "?".



%! 'ihier-part'(+Scheme:atom, -Auth:compound, -Segments:list(atom))// is det.
%
% ```abnf
% ihier-part = "//" iauthority ipath-abempty
%            / ipath-absolute
%            / ipath-rootless
%            / ipath-empty
% ```

'ihier-part'(Scheme, Auth, Segments) -->
  "//",
  iauthority(Scheme, Auth),
  'ipath-abempty'(Segments).
'ihier-part'(_, _, Segments) -->
  'ipath-absolute'(Segments).
'ihier-part'(_, _, Segments) -->
  'ipath-rootless'(Segments).
'ihier-part'(_, _, Segments) -->
  'ipath-empty'(Segments).



%! ihost(-Host:atom)// is det.
%
% ```abnf
% ihost = IP-literal / IPv4address / ireg-name
% ```

ihost(Ip) -->
  'IP-literal'(Ip), !.
ihost(Ip) -->
  'IPv4address'(Ip), !.
ihost(Host) -->
  'ireg-name'(Host).



%! ipath(-Segments:list(atom))// is det.
%
% ```abnf
% ipath = ipath-abempty    ; begins with "/" or is empty
%       / ipath-absolute   ; begins with "/" but not "//"
%       / ipath-noscheme   ; begins with a non-colon segment
%       / ipath-rootless   ; begins with a segment
%       / ipath-empty      ; zero characters
% ```

% Begins with "/" or is empty.
ipath(Segments) -->
  'ipath-abempty'(Segments), !.
% Begins with "/" but not "//".
ipath(Segments) -->
  'ipath-absolute'(Segments), !.
% Begins with a non-colon segment
ipath(Segments) -->
  'ipath-noscheme'(Segments), !.
% Begins with a segment
ipath(Segments) -->
  'ipath-rootless'(Segments), !.
% Empty path (i.e., no segments).
ipath(Segments) -->
  'ipath-empty'(Segments).



%! 'ipath-abempty'(-Segments:list(atom))// is det.
%
% ```abnf
% ipath-abempty = *( "/" isegment )
% ```

'ipath-abempty'(Segments) -->
  *(sep_isegment, Segments), !.



%! 'ipath-absolute'(-Segments:list(atom))// is det.
%
% ```abnf
% ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
% ```

'ipath-absolute'(L) -->
  "/",
  ('isegment-nz'(H) -> *(sep_isegment, T), !, {L = [H|T]} ; {L = []}).



%! 'ipath-empty'(-Segments:list(atom))// is det.
%
% ```abnf
% ipath-empty = 0<ipchar>
% ```

'ipath-empty'([]) --> "".



%! 'ipath-noscheme'(-Segments:list(atom))// is det.
%
% ```abnf
% ipath-noscheme = isegment-nz-nc *( "/" isegment )
% ```

'ipath-noscheme'([H|T]) -->
  'isegment-nz-nc'(H),
  *(sep_isegment, T).



%! 'ipath-rootless'(-Segments:list(atom))// is det.
%
% ```abnf
% ipath-rootless = isegment-nz *( "/" isegment )
% ```

'ipath-rootless'([H|T]) -->
  'segment-nz'(H),
  *(sep_isegment, T).



%! ipchar(-Code:code)// .
%
% ```abnf
% ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
% ```

ipchar(C)   --> iunreserved(C).
ipchar(C)   --> 'sub-delims'(C).
ipchar(0':) --> ":".
ipchar(0'@) --> "@".
ipchar(C)   --> 'pct-encoded'(C).



%! iprivate(-Code:code)// .
%
% ```abnf
% iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
% ```

iprivate(C) --> between_code_rad(hex('E000'),   hex('F8FF'),   C).
iprivate(C) --> between_code_rad(hex('F0000'),  hex('FFFFD'),  C).
iprivate(C) --> between_code_rad(hex('100000'), hex('10FFFD'), C).



%! iquery(-Query:atom)// is det.
%
% ``abnf
% iquery = *( ipchar / iprivate / "/" / "?" )
% ```

iquery(Query) -->
  *(iquery_code, Cs), !,
  {atom_codes(Query, Cs)}.

iquery_code(C)   --> iprivate(C).
iquery_code(0'/) --> "/".
iquery_code(0'?) --> "?".
iquery_code(C)   --> ipchar(C).



%! 'ireg-name'(-Host:atom)// is det.
%
% ```abnf
% ireg-name = *( iunreserved / pct-encoded / sub-delims )
% ```

'ireg-name'(Host) -->
  *(ireg_name_code, Cs), !,
  {atom_codes(Host, Cs)}.

ireg_name_code(C) --> iunreserved(C).
ireg_name_code(C) --> 'sub-delims'(C).
ireg_name_code(C) --> 'pct-encoded'(C).



%! 'irelative-part'(+Scheme:atom, -Auth:compound, -Segments:list(atom))// is det.
%
% ```abnf
% irelative-part = "//" iauthority ipath-abempty
%                / ipath-absolute
%                / ipath-noscheme
%                / ipath-empty
% ```

'irelative-part'(Scheme, Auth, Segments) -->
  "//",
  iauthority(Scheme, Auth),
  'ipath-abempty'(Segments), !.
'irelative-part'(_, _, Segments) -->
  'ipath-absolute'(Segments), !.
'irelative-part'(_, _, Segments) -->
  'ipath-noscheme'(Segments), !.
'irelative-part'(_, _, Segments) -->
  'ipath-empty'(Segments).



%! 'irelative-ref'(-Uri:compound)// is det.
%
% Relative IRI reference.
%
% ```abnf
% irelative-ref = irelative-part [ "?" iquery ] [ "#" ifragment ]
% ```

'irelative-ref'(uri(Scheme,Auth,Segments,Query,Frag)) -->
  'irelative-part'(Scheme, Auth, Segments),
  ("?" -> iquery(Query) ; ""),
  ("#" -> ifragment(Frag) ; "").



%! 'IRI'(-Uri:compound)// is det.
%
% ```abnf
% IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
% ```

'IRI'(uri(Scheme,Auth,Segments,Query,Frag)) -->
  scheme(Scheme),
  ":",
  'ihier-part'(Scheme, Auth, Segments),
  ("?" -> iquery(Query) ; ""),
  ("#" -> ifragment(Frag) ; "").



%! 'IRI-reference'(-Uri:compound)// is det.
%
% There are two types of IRI reference: (1) IRI, (2) IRI relative
% reference.
%
% ```abnf
% IRI-reference = IRI / irelative-ref
% ```

'IRI-reference'(Uri) -->
  'IRI'(Uri), !.
'IRI-reference'(Uri) -->
  'irelative-ref'(Uri).



%! isegment(-Segment:atom)// is det.
%
% ```abnf
% isegment = *ipchar
% ```

isegment(Segment) -->
  *(ipchar, Cs), !,
  {atom_codes(Segment, Cs)}.



%! 'isegment-nz'(-Segment:atom)// is det.
%
% ```abnf
% isegment-nz = 1*ipchar
% ```

'isegment-nz'(Segment) -->
  +(ipchar, Cs), !,
  {atom_codes(Segment, Cs)}.



%! 'isegment-nz-nc'(-Segment:atom)// is det.
%
% ```abnf
% isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims / "@" )
%                ; non-zero-length segment without any colon ":"
% ```

'isegment-nz-nc'(Segment) -->
  +(isegment_nz_nc_code, Cs), !,
  {string_codes(Segment, Cs)}.

isegment_nz_nc_code(C)   --> iunreserved(C).
isegment_nz_nc_code(C)   --> 'pct-encoded'(C).
isegment_nz_nc_code(C)   --> 'sub-delims'(C).
isegment_nz_nc_code(0'@) --> "@".



%! iunreserved(-Code:code)// .
%
% ```abnf
% iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
% ```

iunreserved(C) --> unreserved(C).
iunreserved(C) --> ucschar(C).



%! iuserinfo(-User:atom)// is det.
%
% ```abnf
% iuserinfo = *( iunreserved / pct-encoded / sub-delims / ":" )
% ```

iuserinfo(User) -->
  *(iuserinfo_code, Cs), !,
  {atom_codes(User, Cs)}.

iuserinfo_code(C) -->   iunreserved(C).
iuserinfo_code(0':) --> ":".
iuserinfo_code(C) -->   'sub-delims'(C).
iuserinfo_code(C) -->   'pct-encoded'(C).



%! ucschar(-Code:code)// .
%
% ```abnf
% ucschar = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
%         / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
%         / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
%         / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
%         / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
%         / %xD0000-DFFFD / %xE1000-EFFFD
% ```

ucschar(C) --> between_code_rad(hex('A0'),    hex('D7FF'),  C).
ucschar(C) --> between_code_rad(hex('F900'),  hex('FDCF'),  C).
ucschar(C) --> between_code_rad(hex('FDF0'),  hex('FFEF'),  C).
ucschar(C) --> between_code_rad(hex('10000'), hex('1FFFD'), C).
ucschar(C) --> between_code_rad(hex('20000'), hex('2FFFD'), C).
ucschar(C) --> between_code_rad(hex('30000'), hex('3FFFD'), C).
ucschar(C) --> between_code_rad(hex('40000'), hex('4FFFD'), C).
ucschar(C) --> between_code_rad(hex('50000'), hex('5FFFD'), C).
ucschar(C) --> between_code_rad(hex('60000'), hex('6FFFD'), C).
ucschar(C) --> between_code_rad(hex('70000'), hex('7FFFD'), C).
ucschar(C) --> between_code_rad(hex('80000'), hex('8FFFD'), C).
ucschar(C) --> between_code_rad(hex('90000'), hex('9FFFD'), C).
ucschar(C) --> between_code_rad(hex('A0000'), hex('AFFFD'), C).
ucschar(C) --> between_code_rad(hex('B0000'), hex('BFFFD'), C).
ucschar(C) --> between_code_rad(hex('C0000'), hex('CFFFD'), C).
ucschar(C) --> between_code_rad(hex('D0000'), hex('DFFFD'), C).
ucschar(C) --> between_code_rad(hex('E1000'), hex('EFFFD'), C).





% HELPERS %

%! default_port(+Scheme:atom, -Port:oneof([80,443])) is det.
%
% The default port for the given scheme.

default_port(http, 80).
default_port(https, 443).



%! sep_isegment(-Segment)// is det.

sep_isegment(Segment) -->
  "/",
  isegment(Segment).
