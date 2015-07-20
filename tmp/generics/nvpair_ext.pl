:- module(
  nvpair_ext,
  [
    merge_nvpairs/3, % +Pairs1:list(nvpair)
                     % +Pairs2:list(nvpair)
                     % -Merge:list(nvpair)
    nvpair/3 % ?Name:atom
             % ?Value
             % ?NameValuePair:compound
  ]
).

/** <module> Name-value pair extensions

Support for name-value pairs.

@author Wouter Beek
@version 2014/08, 2014/10, 2015/01
*/

:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plc(prolog/pl_mode)).





merge_nvpairs(L1, L2, L3):-
  append(L1, L2, L0),
  sort(L0, L3).



%! nvpair(+Name:atom, +Value, +NameValuePair:compound) is semidet.
%! nvpair(+Name:atom, +Value, -NameValuePair:compound) is multi.
%! nvpair(-Name:atom, -Value, +NameValuePair:compound) is det.

nvpair(Name, Value, NVPair):-
  call_det(nvpair0, term-Name, term-Value, nonvar-NVPair).

nvpair0(Name, Value, Name=Value).
nvpair0(Name, Value, Name-Value).
nvpair0(Name, Value, NVPair):-
  NVPair =.. [Name,Value].

