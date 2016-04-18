:- module(
  test_metadata,
  [
    test_open_any2/0
  ]
).

/* <module> Test metadata

@author Wouter Beek
@version 2015/11-2015/12
*/

:- use_module(library(debug_ext)).
:- use_module(library(lodapi/lodapi_document)).
:- use_module(library(lodapi/lodapi_metadata)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(print_ext)).
:- use_module(library(yall)).

:- debug(http(parse)).





test_open_any2:-
  test_source(Source),
  debug_collect_messages(test_open_any2(Source)).


test_open_any2(Source):-
  call_on_stream(Source, print_dict0).

print_dict0(MIn, In) :-
  print_dict(MIn), nl.





% HELPERS %

%test_source('https://www.dropbox.com/s/okzyej25j2aypkg/BEL2RDFexample.owl?dl=1').
test_source(Source):-
  document(Doc),
  metadata(Doc, llo:url, Source).
