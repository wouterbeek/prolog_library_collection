:- module(
  rfc7232,
  [
    'entity-tag'//1 % -Tag:pair(atom,boolean)
  ]
).

/** <module> RFC 7232 - HTTP/1.1: Range Requests

@author Wouter Beek
@see https://tools.ietf.org/html/rfc7232
@version 2017/05-2017/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc5234)).
:- use_module(library(http/rfc7230)).
:- use_module(library(http/rfc7231)).

:- discontiguous
    http:http_header/1.

:- dynamic
    http:http_header/1.

:- multifile
    http:http_header/1.





%! etag(-EntityTag:pair(atom,boolean))// is det.
%
% Used for Web cache validation and optimistic concurrency control.
%
% ```abnf
% ETag = "ETag" ":" entity-tag
% ```

http:http_header(etag).
etag(EntityTag) -->
  'entity-tag'(EntityTag).



%! 'entity-tag'(-Tag:pair(atom,boolean))// is det.
%
% ```abnf
% entity-tag = [ weak ] opaque-tag
% ```

'entity-tag'(Tag-Weak) -->
  (weak -> {Weak = true} ; {Weak = false}),
  'opaque-tag'(Tag).



%! etagc(-Code:code)// is det.
%
% ```abnf
% etagc = %x21 | %x23-7E | obs-text   ; VCHAR except double quotes, plus obs-text
% ```

etagc(0x21) --> [0x21].
etagc(C)    --> [C], {between(0x23, 0x7E, C)}.
etagc(C)    --> 'obs-text'(C).



%! 'if-match'(-EntityTags:list(pair(atom,boolean)))// is det.
%
% ```abnf
% If-Match = "*" | 1#entity-tag
% ```

http:http_header('if-match').
'if-match'([]) -->
  "*", !.
'if-match'(EntityTags)  -->
  +##('entity-tag', EntityTags), !.



%! 'if-modified-since'(-Datetime:compound)// is det.
%
% ```abnf
% If-Modified-Since = HTTP-date
% ```

http:http_header('if-modified=since').
'if-modified-since'(Datetime) -->
  'HTTP-date'(Datetime).



%! 'if-none-match'(-EntityTags:list(pair(atom,boolean)))// is det.
%
% ```abnf
% If-None-Match = "*" | 1#entity-tag
% ```

http:http_header('if-none-match').
'if-none-match'([]) -->
  "*", !.
'if-none-match'(EntityTags) -->
  +##('entity-tag', EntityTags).



%! 'if-unmodified-since'(-Datetime:compound)// is det.
%
% ```abnf
% If-Unmodified-Since = HTTP-date
% ```

http:http_header('if-unmodified-since').
'if-unmodified-since'(Datetime) -->
  'HTTP-date'(Datetime).



%! 'last-modified'(-DT:compound)// is det.
%
% ```abnf
% Last-Modified = HTTP-date
% ```
%
% Example: `Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT`

http:http_header('last-modified').
'last-modified'(DT) -->
  'HTTP-date'(DT).



%! 'opaque-tag'(-Tag:atom)// is det.
%
% ```abnf
% opaque-tag = DQUOTE *etagc DQUOTE
% ```

'opaque-tag'(Tag) -->
  'DQUOTE',
  dcg_atom(*(etagc), Tag),
  'DQUOTE'.



%! weak// is det.
%
% ```abnf
% weak = %x57.2F   ; "W/", case-sensitive
% ```

weak -->
  "W/".
