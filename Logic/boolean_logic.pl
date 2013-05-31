:- module(
  boolean_logic,
  [
    boolean_or/3 % ?X:boolean
                 % ?Y:boolean
                 % ?Z:boolean
  ]
).

/** <module> BOOLEAN_LOGIC

Predicates implementing Boolean logic.

@author Wouter Beek
@version 2013/05
*/

boolean_or(fail, fail, fail).
boolean_or(true, fail, true).
boolean_or(fail, true, true).
boolean_or(true, true, true).

