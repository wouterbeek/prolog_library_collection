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
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(print_ext)).
:- use_module(library(xpath)).

:- dynamic
    marcxml:field_property/3,
    marcxml:field_subfield_property/4.

:- multifile
    marcxml:field_property/3,
    marcxml:field_subfield_property/4.





%! marcxml_controlfield(+Dom, +Field, -Val) is nondet.

marcxml_controlfield(Dom, Field, Val) :-
  xpath(Dom, //'marc:controlfield'(@tag=Field,text), Val),
  Val \== ''.



%! marcxml_datafield(+Dom, +Field, +Subfield, -Val) is nondet.
%! marcxml_datafield(+Dom, +Field, -Indicator1, -Indicator2, +Subfield, -Val) is nondet.

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
  findall(
    Field-Val,
    (
      marcxml_controlfield(Dom, Field, Val),
      (   marcxml:field_property(_, Field, _)
      ->  true
      ;   msg_warning("[MARCXML] Unrecognized field: ~w~n", [Field])
      )
    ),
    Pairs1
  ),
  aggregate_all(set(Field), marcxml_datafield0(Dom, Field, _), Fields),
  findall(
    Field-D,
    (
      member(Field, Fields),
      marcxml_profile0(Dom, Field, D)
    ),
    Pairs2
  ),
  append(Pairs1, Pairs2, Pairs),
  dict_pairs(D, Pairs).

marcxml_profile0(Dom, Field, D) :-
  findall(
    Subfield-Val,
    (
      marcxml_datafield(Dom, Field, Subfield, Val),
      (   marcxml:field_subfield_property(_, Field, Subfield, _)
      ->  true
      ;   msg_warning(
            "[MARCXML] Unrecognized field&subfield: ~w ~w~n",
            [Field,Subfield]
          )
      )
    ),
    Pairs
  ),
  dict_pairs(D, Pairs).



%! marcxml_subfield(+Dom, +Subfield, -Val) is semidet.

marcxml_subfield(Dom, Subfield, Val) :-
  xpath(Dom, //'marc:subfield'(@code=Subfield,text), Val),
  Val \== ''.
