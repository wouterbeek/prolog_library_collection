:- module(
  rfc7233,
  [
  ]
).

/** <module> RFC 7233 - HTTP/1.1: Range Requests

@author Wouter Beek
@compat RFC 7233
@see https://tools.ietf.org/html/rfc7233
@version 2017/05-2017/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc7230)).
:- use_module(library(http/rfc7231)).
:- use_module(library(http/rfc7232)).
:- use_module(library(math_ext)).

:- discontiguous
    http:http_header/1.

:- dynamic
    http:http_header/1.

:- multifile
    http:http_header/1.





%! 'accept-ranges'(-Ranges:list)// is det.
%
% ```abnf
% Accept-Ranges = acceptable-ranges
% ```

http:http_header('accept-ranges').
'accept-ranges'(Ranges) -->
  'acceptable-ranges'(Ranges).



%! 'acceptable-ranges'(-AcceptableRanges:list)// is det.
%
% ```abnf
% acceptable-ranges = 1#range-unit | "none"
% ```

'acceptable-ranges'(L) -->
  +##('range-unit', L), !.
'acceptable-ranges'([]) -->
  atom_ci(none).



%! 'byte-content-range'(-Range:pair(nonneg), -Len:nonneg)// is det.
%
% ```abnf
% byte-content-range = bytes-unit SP ( byte-range-resp | unsatisfied-range )
% ```

'byte-content-range'(Range, Len) -->
  'bytes-unit',
  'SP',
  ('byte-range-resp'(Range, Len) -> "" ; 'unsatisfied-range'(Range, Len)).



%! 'byte-range'(-Range:pair(nonneg))// is det.
%
% ```abnf
% byte-range = first-byte-pos "-" last-byte-pos
% ```

'byte-range'(First-Last) -->
  'first-byte-pos'(First),
  "-",
  'last-byte-pos'(Last).



%! 'byte-range-resp'(-Range:pair(nonneg), -Length:nonneg)// is det.
%
% ```abnf
% byte-range-resp = byte-range "/" ( complete-length | "*" )
% ```

'byte-range-resp'(Range, Length) -->
  'byte-range'(Range),
  "/",
  ("*" -> "" ; 'complete-length'(Length)).



%! 'byte-range-spec'(-Range:pair(nonneg))// .
%
% ```abnf
% byte-range-spec = first-byte-pos "-" [ last-byte-pos ]
% ```

'byte-range-spec'(FirstPosition-LastPosition) -->
  'first-byte-pos'(FirstPosition),
  "-",
  ?('last-byte-pos'(LastPosition)).



%! 'byte-ranges-specifier'(-Set:list(or([nonneg,pair(nonneg)])))// .
%
% ```abnf
% byte-ranges-specifier = bytes-unit "=" byte-range-set
% ```

'byte-ranges-specifier'(Set) -->
  'bytes-unit',
  "=",
  'byte-range-set'(Set).



%! 'byte-range-set'(-Set:list(or([nonneg,pair(nonneg)])))// .
%
% ```abnf
% byte-range-set = 1#( byte-range-spec | suffix-byte-range-spec )
% ```

'byte-range-set'(Set) -->
  +##(byte_range_set_part0, Set), !.

byte_range_set_part0(Range) --> 'byte-range-spec'(Range).
byte_range_set_part0(Len) --> 'suffix-byte-range-spec'(Len).



%! 'bytes-unit'// .
%
% ```abnf
% bytes-unit = "bytes"
% ```

'bytes-unit' -->
  atom_ci(bytes).



%! 'complete-length'(-Length:nonneg)// is det.
%
% ```abnf
% complete-length = 1*DIGIT
% ```

'complete-length'(Length) -->
  +('DIGIT', Weights), !,
  {integer_weights(Length, Weights)}.



%! 'content-range'(-Unit:atom, -Range:pair(nonneg))// is det.
%
% ```abnf
% Content-Range = byte-content-range | other-content-range
% ```

http:http_header('content-range').
'content-range'(byte, Range-Len) -->
  'byte-content-range'(Range, Len).
'content-range'(Unit, Resp) -->
  'other-content-range'(Unit, Resp).



%! 'first-byte-pos'(-Position:nonneg)// .
%
% ```abnf
% first-byte-pos = 1*DIGIT
% ```

'first-byte-pos'(Position) -->
  +('DIGIT', Weights), !,
  {integer_weights(Position, Weights)}.



%! 'if-range'(-Value:compound)// is det.
%
% ```abnf
% If-Range = entity-tag | HTTP-date
% ```

http:http_header('if-range').
'if-range'(EntityTag) -->
  'entity-tag'(EntityTag), !.
'if-range'(DateTime) -->
  'HTTP-date'(DateTime).



%! 'last-byte-pos'(-Position:nonneg)// .
%
% ```abnf
% last-byte-pos = 1*DIGIT
% ```

'last-byte-pos'(Position) -->
  +('DIGIT', Weights), !,
  {integer_weights(Position, Weights)}.



%! 'other-content-range'(-Unit:atom, -Rest:atom)// is det.
%
% ```abnf
% other-content-range = other-range-unit SP other-range-resp
% ```

'other-content-range'(Unit, Resp) -->
  'other-range-unit'(Unit),
  'SP',
  'other-range-resp'(Resp).



%! 'other-range-resp'(-Resp:atom)// is det.
%
% ```abnf
% other-range-resp = *CHAR
% ```

'other-range-resp'(Resp) -->
  *('CHAR', Cs), !,
  {atom_codes(Resp, Cs)}.



%! 'other-range-set'(-Set:atom)// .
%
% ```abnf
% other-range-set = 1*VCHAR
% ```

'other-range-set'(Set) -->
  +('VCHAR', Cs), !,
  {atom_codes(Set, Cs)}.



%! 'other-range-unit'(-Unit:atom)// .
%
% ```abnf
% other-range-unit = token
% ```

'other-range-unit'(Unit) -->
  token(Unit).



%! 'other-ranges-specifier'(-Unit:atom, -Set:atom)// .
%
% ```abnf
% other-ranges-specifier = other-range-unit "=" other-range-set
% ```

'other-ranges-specifier'(Unit, Set) -->
  'other-range-unit'(Unit),
  "=",
  'other-range-set'(Set).



%! range(-Unit:atom, -Range:pair(nonneg))// .
%
% ```abnf
% Range = byte-ranges-specifier | other-ranges-specifier
% ```

http:http_header(range).
range(byte, Range) -->
  'byte-ranges-specifier'(Range).
range(Unit, Set) -->
  'other-ranges-specifier'(Unit, Set).



%! 'range-unit'(-Unit:atom)// .
%
% ```abnf
% range-unit = bytes-unit | other-range-unit
% ```

'range-unit'(bytes) -->
  'bytes-unit', !.
'range-unit'(Unit) -->
  'other-range-unit'(Unit).



%! 'suffix-byte-range-spec'(-Length:nonneg)// .
%
% ```abnf
% suffix-byte-range-spec = "-" suffix-length
% ```

'suffix-byte-range-spec'(Length) -->
  "-",
  'suffix-length'(Length).



%! 'suffix-length'(-Length:nonneg)// .
%
% ```abnf
% suffix-length = 1*DIGIT
% ```

'suffix-length'(Length) -->
  +('DIGIT', Weights),
  {integer_weights(Length, Weights)}.



%! 'unsatisfied-range'(-Range:string, -Length:nonneg)// is det.
%
% ```abnf
% unsatisfied-range = "*/" complete-length
% ```

'unsatisfied-range'(_-_, Length) -->
  "*/",
  'complete-length'(Length).
