:- module(
  dcg_volume,
  [
    volume//2 % ?Lang:atom
              % ?Volume:integer
  ]
).

/** <module> DCG_VOLUME

DCGs for volume information (for publications).

@author Wouter Beek
@version 2013/05
*/

:- use_module(dcg(dcg_year)).
:- use_module(library(dcg/basics)).



volume(Lang, Volume) -->
  volume_noun(Lang, number), blank,
  integer(Volume).
volume(Lang, Volume) -->
  volume_noun(Lang, year), blank,
  (integer(Volume) ; year(Lang, Volume)).

volume_noun(en, number) --> "Volume".
volume_noun(en, number) --> "volume".
volume_noun(nl, number) --> "Deel".
volume_noun(nl, number) --> "deel".
volume_noun(nl, number) --> "Volume".
volume_noun(nl, number) --> "volume".
volume_noun(nl, year) --> "Jaargang".
volume_noun(nl, year) --> "jaargang".

