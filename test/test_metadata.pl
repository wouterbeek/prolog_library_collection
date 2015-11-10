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
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).





test_metadata(openany):-
  test_source(Source),
  setup_call_cleanup(
    open_any2(Source, read, _, Close_0, [metadata(M)]),
    print_dict(M),
    close_any2(Close_0)
  ).
test_metadata(call_on_archive):-
  test_source(Source),
  call_on_archive(Source, print_dict0).

print_dict0(M, _):-
  print_dict(M).

%test_source('http://datendienst.dnb.de/cgi-bin/mabit.pl?cmd=fetch&userID=opendata&pass=opendata&mabheft=ZDBTitel.ttl.gz').
test_source('~/Git/Prolog-Library-Collection/call_hierarchy.dot').
