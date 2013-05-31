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

:- use_module(library(dcg/basics)).



volume(Lang, Volume) -->
  volume_noun(Lang), blank,
  integer(Volume).

volume_noun(en) --> "volume".
volume_noun(en) --> "Volume".
volume_noun(nl) --> "Deel".
volume_noun(nl) --> "deel".
volume_noun(nl) --> "volume".
volume_noun(nl) --> "Volume".

