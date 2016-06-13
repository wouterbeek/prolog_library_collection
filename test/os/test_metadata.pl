:- module(
  test_metadata,
  [
    test_open_any2/0
  ]
).

/* <module> Test metadata

@author Wouter Beek
@version 2015/11-2015/12, 2016/04
*/

:- use_module(library(debug_ext)).
:- use_module(library(lodapi/lodapi_doc)).
:- use_module(library(lodapi/lodapi_meta)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(print_ext)).
:- use_module(library(yall)).

:- debug(http(parse)).





test_open_any2:-
  test_source(Source),
  debug_collect_messages(test_open_any2(Source)).


test_open_any2(Source):-
  call_on_stream(
    Source,
    [In,M,M]>>copy_stream_data(In, user_output),
    [metadata(M)]
  ),
  print_dict(M), nl.





% HELPERS %

%test_source('https://www.dropbox.com/s/okzyej25j2aypkg/BEL2RDFexample.owl?dl=1').
test_source(Source):-
  document(Doc),
  metadata(Doc, llo:url, Source).
