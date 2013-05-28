:- module(
  xpath_ext,
  [
    xpath2/3, % +DOM:oneof([element,list(element)])
              % +Spec
              % -Content
    xpath_chk2/3 % +DOM:oneof([element,list(element)])
                 % +Spec
                 % -Content
  ]
).

/** <module> XPATH_EXT

Extensions to the XPath library.

@author Wouter Beek
@version 2013/05
*/

:- reexport(library(xpath)).



xpath2(DOM, Spec, Content):-
  is_list(DOM),
  !,
  xpath2(element(outer, [], DOM), Spec, Content).
xpath2(DOM, Spec, Content):-
  var(Content),
  xpath(DOM, Spec, Content).

xpath_chk2(DOM, Spec, Content):-
  is_list(DOM),
  !,
  xpath_chk2(element(outer, [], DOM), Spec, Content).
xpath_chk2(DOM, Spec, Content):-
  var(Content),
  xpath_chk(DOM, Spec, Content).

