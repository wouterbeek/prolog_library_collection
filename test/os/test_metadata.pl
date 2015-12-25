:- module(
  test_metadata,
  [
    test_call_on_archive/0,
    test_open_any2/0
  ]
).

/* <module> Test metadata

@author Wouter Beek
@version 2015/11-2015/12
*/

:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(gui_tracer)).
:- use_module(library(lodapi/lodapi_document)).
:- use_module(library(lodapi/lodapi_metadata)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(swi_ide)).

:- guitracer.
:- prolog_ide(debug_monitor).

:- debug(http(parse)).





test_call_on_archive:-
  test_source(Source),
  call_collect_messages(call_on_archive(Source, print_dict0)).
print_dict0(M, _):- print_dict(M), nl.



test_open_any2:-
  test_source(Source),
  debug_collect_messages(test_open_any2(Source)).


test_open_any2(Source):-
  setup_call_cleanup(
    open_any2(Source, read, _, Close_0, [metadata(M)]),
    (print_dict(M), nl),
    close_any2(Close_0)
  ).





% HELPERS %

test_source('https://www.dropbox.com/s/okzyej25j2aypkg/BEL2RDFexample.owl?dl=1').
test_source(Source):-
  document(Doc),
  metadata(Doc, llo:url, Source).
