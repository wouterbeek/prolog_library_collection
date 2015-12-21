:- module(
  test_metadata,
  [
    test_metadata/1 % ?Name:oneof([call_on_archive,openany])
  ]
).

/* <module> Test metadata

@author Wouter Beek
@version 2015/11-2015/12
*/

:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(lodapi/lodapi_document)).
:- use_module(library(lodapi/lodapi_metadata)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).

:- debug(http(parse)).





test_metadata(openany):-
  test_source(Source),
  call_collect_messages(test_metadata(openany, Source)).
test_metadata(call_on_archive):-
  test_source(Source),
  call_on_archive(Source, print_dict0).
print_dict0(M, _):- print_dict(M).


test_metadata(openany, Source):-
  setup_call_cleanup(
    open_any2(Source, read, _, Close_0, [metadata(M)]),
    print_dict(M),
    close_any2(Close_0)
  ).

test_source(Source):-
  document(Doc),
  metadata(Doc, llo:url, Source).
