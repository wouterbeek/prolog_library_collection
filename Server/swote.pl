:- module(
  swote,
  [
    swote_web/1 % -Markup:dom
  ]
).

/** <module> SWOTE

Semantic Web Note Taking Application.

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

% HTTP handler for the index.
:- http_handler(root(swote), swote, [prefix]).



object_field(DOM):-
  DOM = [
    element(label, [], ['Object:']),
    element(input, [type=text], [])
  ].

predicate_field(DOM):-
  DOM = [
    element(label, [], ['Predicate:']),
    element(input, [type=text], [])
  ].

subject_field(DOM):-
  DOM = [
    element(label, [], ['Subject:']),
    element(input, [type=text], [])
  ].

swote(_Request):-
  swote_web(Body),
  reply_html_page(swote, [], Body).

swote_web(DOM):-
  DOM = [element(form, [], FormContent)],
  subject_field(SubjectField),
  predicate_field(PredicateField),
  object_field(ObjectField),
  append(
    [
      SubjectField,
      element(br, [], []),
      PredicateField,
      element(br, [], []),
      ObjectField
    ],
    FormContent
  ).

user:body(swote, Body) -->
  html(body([h1('SWOTE') | Body])).

user:head(swote, Head) -->
  html(head([title('SWOTE') | Head])).

