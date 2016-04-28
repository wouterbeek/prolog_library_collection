:- module(
  hdt_ext,
  [
    hdt_goal/2 % +HdtFile, :Goal_1
  ]
).

:- reexport(library(hdt), [
     hdt_search/4 as hdt,
     hdt_subject/2
   ]).

/** <module> HDT extensions

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(hdt)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- rdf_meta
   hdt(+, r, r, o).

:- meta_predicate
    hdt_goal(+, 1).





hdt_goal(HdtFile, Goal_1) :-
  setup_call_cleanup(
    hdt_open(Hdt, HdtFile),
    call(Goal_1, Hdt),
    hdt_close(Hdt)
  ).
