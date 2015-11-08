:- module(
  test_metadata,
  [
    test_metadata/1 % ?Name:oneof([openany])
  ]
).

/* <module> Test metadata

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dict_ext)).
:- use_module(library(os/open_any2)).





test_metadata(openany):-
  test_iri(Iri),
  setup_call_cleanup(
    open_any2(Iri, read, _, Close_0, [metadata(M)]),
    print_dict(M),
    close_any2(Close_0)
  ).

test_iri('http://datendienst.dnb.de/cgi-bin/mabit.pl?cmd=fetch&userID=opendata&pass=opendata&mabheft=ZDBTitel.ttl.gz').
