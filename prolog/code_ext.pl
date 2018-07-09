:- module(
  code_ext,
  [
    put_codes/1, % +Codes
    put_codes/2  % +Out:stream, +Codes
  ]
).

/** <module> Code extensions

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).





%! put_codes(+Codes:list(code)) is det.
%! put_codes(+Out:stream, +Codes:list(code)) is det.
%
% @see Wrappers around put_code/1 that work on lists of codes and
%      that can write to an arbitrary output stream.

put_codes(Codes):-
  maplist(put_code, Codes).



put_codes(Out, Codes):-
  maplist(put_code(Out), Codes).
