:- module(
  marcxml,
  [
    marcxml_controlfield/3, % +Dom, +Field, -Val
    marcxml_datafield/4,    % +Dom, +Field, +Subfield, -Val
    marcxml_datafield/4,    % +Dom, +Field, -Indicator1, -Indicator2, +Subfield, -Val
    marcxml_profile/2,      % +Dom, -D
    marcxml_subfield/3      % +Dom, +Subfield, -Val
  ]
).

/** <module> MARCXML

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(xpath)).





%! marcxml_controlfield(+Dom, +Field, -Val) is nondet.

marcxml_controlfield(Dom, Field, Val) :-
  xpath(Dom, //'marc:controlfield'(@tag=Field,text), Val).



%! marcxml_datafield(+Dom, +Field, +Subfield, -Val) is nondet.
%! marcxml_datafield(
%!   +Dom,
%!   +Field,
%!   -Indicator1,
%!   -Indicator2,
%!   +Subfield,
%!   -Val
%! ) is nondet.

marcxml_datafield(Dom, Field, Subfield, Val) :-
  marcxml_datafield(Dom, Field, _, _, Subfield, Val).


marcxml_datafield(Dom, Field, Indicator1, Indicator2, Subfield, Val) :-
  marcxml_datafield0(Dom, Field, Datafield),
  xpath_chk(Datafield, /self(@ind1), Indicator1),
  xpath_chk(Datafield, /self(@ind2), Indicator2),
  marcxml_subfield(Datafield, Subfield, Val).

marcxml_datafield0(Dom, Field, Datafield) :-
  xpath(Dom, //'marc:datafield', Datafield),
  xpath_chk(Datafield, /self(@tag), Field).



%! marcxml_profile(+Dom, -D) is det.

marcxml_profile(Dom, D) :-
  aggregate_all(
    set(Field-Val),
    marcxml_controlfield(Dom, Field, Val),
    Pairs1a
  ),
  group_pairs_by_key(Pairs1a, Pairs1b),
  maplist(flatten_singleton_value, Pairs1b, Pairs1c),
  aggregate_all(set(Field), marcxml_datafield0(Dom, Field, _), Fields),
  findall(
    Field-D,
    (
      member(Field, Fields),
      marcxml_profile0(Dom, Field, D)
    ),
    Pairs2
  ),
  append(Pairs1c, Pairs2, Pairs),
  dict_pairs(D, Pairs).

marcxml_profile0(Dom, Field, D) :-
  aggregate_all(
    set(Subfield-Val),
    (
      marcxml_datafield(Dom, Field, Subfield, Val),
      (Vals = [Val] -> true ; Val = Vals)
    ),
    Pairs1
  ),
  group_pairs_by_key(Pairs1, Pairs2),
  maplist(flatten_singleton_value, Pairs2, Pairs3),
  dict_pairs(D, Pairs3).

flatten_singleton_value(Key-[Val], Key-Val) :- !.
flatten_singleton_value(Pair, Pair).



%! marcxml_subfield(+Dom, +Subfield, -Val) is semidet.

marcxml_subfield(Dom, Subfield, Val) :-
  xpath(Dom, //'marc:subfield'(@code=Subfield,text), Val).
