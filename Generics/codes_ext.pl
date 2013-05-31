:- module(
  codes_ext,
  [
    split_codes/3, % +Codes:list(code)
                   % +Split:list(code)
                   % -Results:list(list(code))
    strip_codes/3 % +Strip:list(code)
                  % +In:list(code)
                  % -Out:list(code)
  ]
).

/** <module> CODES_EXT

Predicates for handling codes.

@author Wouter Beek
@version 2013/05
*/

:- use_module(library(lists)).



split_codes(Codes, Split, Results):-
  \+ is_list(Split),
  !,
  split_codes(Codes, [Split], Results).
split_codes(Codes, Split, [Result | Results]):-
  append(Result, Temp, Codes),
  append(Split, NewCodes, Temp),
  !,
  split_codes(NewCodes, Split, Results).
split_codes(Result, _Split, [Result]).

strip_codes(_Strip, [], []):-
  !.
strip_codes(Strip, [H | In], Out):-
  memberchk(H, Strip),
  !,
  strip_codes(Strip, In, Out).
strip_codes(Strip, [H | In], [H | Out]):-
  strip_codes(Strip, In, Out).

