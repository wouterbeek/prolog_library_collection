:- module(
  rfc3987_token,
  [
    'ipath-abempty'//1, % ?Segments:list(string)
    'ipath-absolute'//1, % ?Segments:list(string)
    'ipath-empty'//1, % ?Segments:list(string)
    'ipath-noscheme'//1, % ?Segments:list(string)
    'ipath-rootless'//1, % ?Segments:list(string)
    'ireg-name'//1, % ?RegisteredName:string
    isegment//1, % ?Segment:string
    'isegment-nz'//1, % ?Segment:string
    'isegment-nz-nc'//1 % ?Segment:string
  ]
).

/** <module> RFC 3987: Tokens

@author Wouter Beek
@compat RFC 3987
@see tools.ietf.org/html/rfc3987
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(iri/rfc3987_code)).





%! 'ipath-abempty'(?Segments:list(string))// .
% ```abnf
% ipath-abempty = *( "/" isegment )
% ```

'ipath-abempty'(L) --> *(sep_isegment, L).



%! 'ipath-absolute'(?Segments:list(string))// .
% ```abnf
% ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
% ```

'ipath-absolute'(L) -->
  "/", ('isegment-nz'(H) -> *(sep_isegment, T), {L = [H|T]} ; {L = []}).



%! 'ipath-empty'(?Segments:list(string))// .
% ```abnf
% ipath-empty = 0<ipchar>
% ```

'ipath-empty'([]) --> "".



%! 'ipath-noscheme'(?Segments:list(string))// .
% ```abnf
% ipath-noscheme = isegment-nz-nc *( "/" isegment )
% ```

'ipath-noscheme'([H|T]) --> 'isegment-nz-nc'(H), *(sep_isegment, T).



%! 'ipath-rootless'(?Segments:list(string))// .
% ```abnf
% ipath-rootless = isegment-nz *( "/" isegment )
% ```

'ipath-rootless'([H|T]) --> 'segment-nz'(H), *(sep_isegment, T).



%! 'ireg-name'(?RegisteredName:compound)// .
% ```abnf
% ireg-name = *( iunreserved / pct-encoded / sub-delims )
% ```

'ireg-name'(S) --> *(ireg_name_code, Cs), {string_codes(S, Cs)}.
ireg_name_code(C) --> iunreserved(C).
ireg_name_code(C) --> 'pct-encoded'(C).
ireg_name_code(C) --> 'sub-delims'(C).



%! isegment(?Segment:string)// .
% ```abnf
% isegment = *ipchar
% ```

isegment(S) --> *(ipchar, Cs), {string_codes(S, Cs)}.



%! 'isegment-nz'(?Segment:string)// .
% ```abnf
% isegment-nz = 1*ipchar
% ```

'isegment-nz'(S) --> +(ipchar, Cs), {string_codes(S, Cs)}.



%! 'isegment-nz-nc'(?Segment:string)// .
% ```abnf
% isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims / "@" )
%                ; non-zero-length segment without any colon ":"
% ```

'isegment-nz-nc'(S) --> +(isegment_nz_nc_code, Cs), {string_codes(S, Cs)}.
isegment_nz_nc_code(C)   --> iunreserved(C).
isegment_nz_nc_code(C)   --> 'pct-encoded'(C).
isegment_nz_nc_code(C)   --> 'sub-delims'(C).
isegment_nz_nc_code(0'@) --> "@".





% HELPERS %

sep_isegment(S) --> "/", isegment(S).
