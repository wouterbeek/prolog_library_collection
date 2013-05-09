:- module(
  iso_8859_1,
  [
    non_breaking_space/3
  ]
).

/** <module> ISO 8859-1 character set

@author Wouter Beek
@version 2013/03
*/



non_breaking_space(_O, [160 | R0]-R0, [160 | C0]-C0).

