:- module(code_ext, [put_codes/1]).

/** <module> Code extension

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(apply)).



%! put_codes(+Codes:list(code)) is det.
%! put_codes(+Out:stream, +Codes:list(code)) is det.
% @see Wrapper around put_code/1 that works on lists of codes
%      and that can write to an arbitrary stream.

put_codes(Codes):-
  maplist(put_code, Codes).
