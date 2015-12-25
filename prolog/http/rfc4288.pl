:- module(
  rfc4288,
  [
    'subtype-name'//1, % -SubtypeName:string
    'type-name'//1 % -TypeName:string
  ]
).

/** <module> RFC 4288: Media Type Specifications and Registration Procedures

@author Wouter Beek
@compat RFC 4288
@see https://tools.ietf.org/html/rfc4288
@version 2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'DIGIT'//2 % ?Weight:between(0,9)
                % ?Code:code
   ]).





%! 'reg-name'(-Name:string)// is det.
% ```abnf
% reg-name = 1*127reg-name-chars
% ```

'reg-name'(S) --> 'm*n'(1, 127, 'reg-name-chars', Cs), {string_codes(S, Cs)}.



%! 'reg-name-chars'(-Code:code)// .
% ```abnf
% reg-name-chars = ALPHA / DIGIT / "!"
%                / "#" / "$" / "&" / "."
%                / "+" / "-" / "^" / "_"
% ```

'reg-name-chars'(C)   --> 'ALPHA'(C).
'reg-name-chars'(C)   --> 'DIGIT'(_, C).
'reg-name-chars'(0'!) --> "!".
'reg-name-chars'(0'#) --> "#".
'reg-name-chars'(0'$) --> "$".
'reg-name-chars'(0'&) --> "&".
'reg-name-chars'(0'.) --> ".".
'reg-name-chars'(0'+) --> "+".
'reg-name-chars'(0'-) --> "-".
'reg-name-chars'(0'^) --> "^".
'reg-name-chars'(0'_) --> "_".



%! 'subtype-name'(-SubtypeName:string)// is det.
% ```abnf
% subtype-name = reg-name
% ```

'subtype-name'(S) --> 'reg-name'(S).



%! 'type-name'(-TypeName:string)// is det.
% ```abnf
% type-name = reg-name
% ```

'type-name'(S) --> 'reg-name'(S).
