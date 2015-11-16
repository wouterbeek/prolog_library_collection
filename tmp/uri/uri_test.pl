:- module(uri_test, []).

/** <module> URI Syntax: Test

@author Wouter Beek
@version 2013/07, 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(plunit)).

:- use_module(plc(dcg/dcg_generics)).

:- use_module(plUri(rfc3986)).
:- use_module(plUri(rfc3987)).



:- begin_tests(uri).

test_uri('https://example.com/a/b;g/c?q=aba#rr').
test_uri('https://11.22.33.44:5566/a/b;g/c?q=aba#rr').

test_uri_components(
  http,
  'example.com',
  [[a,b],[c,d],[e],[f]],
  'q=aap',
  me
).
test_uri_components(
  https,
  authority(wouter,[44,55,33,11],7777),
  [[a,b],[c,d],[e],[f]],
  'q=aap',
  me
).

test(
  uri_generate,
  [forall(test_uri_components(Scheme,Authority,Path,Query,Fragment))]
):-
  once(atom_phrase('URI'(Scheme,Authority,Path,Query,Fragment), Atom1)),
  once(atom_phrase('IRI'(Scheme,Authority,Path,Query,Fragment), Atom2)),
  maplist(writeln, [Atom1,Atom2]).

test(uri_parse, [forall(test_uri(Atom))]):-
  once(atom_phrase('URI'(Scheme1,Authority1,Path1,Query1,Fragment1), Atom)),
  once(atom_phrase('IRI'(Scheme2,Authority2,Path2,Query2,Fragment2), Atom)),
  maplist(writeln, [Scheme1,Authority1,Path1,Query1,Fragment1]),
  maplist(writeln, [Scheme2,Authority2,Path2,Query2,Fragment2]).

:- end_tests(uri).
