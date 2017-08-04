:- module(
  rfc4288,
  [
    'subtype-name'//1, % -Subtype
    'type-name'//1     % -Type
  ]
).

/** <module> RFC 4288: Media Type Specifications and Registration Procedures

@author Wouter Beek
@compat RFC 4288
@see https://tools.ietf.org/html/rfc4288
@version 2017/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code
     'DIGIT'//2  % ?Weight, ?Code
   ]).





%! 'reg-name'(-Name:atom)// is det.
%
% ```abnf
% reg-name = 1*127reg-name-chars
% ```

'reg-name'(Name) -->
  'm*n'(1, 127, 'reg-name-chars', Cs), !,
  {atom_codes(Name, Cs)}.



%! 'reg-name-chars'(?Code:ode)// .
%
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



%! 'subtype-name'(-Subtype:atom)// is det.
%
% ```abnf
% subtype-name = reg-name
% ```

'subtype-name'(Subtype) -->
  'reg-name'(Subtype).



%! 'type-name'(-Type:atom)// is det.
%
% ```abnf
% type-name = reg-name
% ```

'type-name'(Type) -->
  'reg-name'(Type).
