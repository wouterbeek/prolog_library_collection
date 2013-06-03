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



email_field(DOM):-
  DOM = [
    element(label, [], ['Email:']),
    element(input, [type=text], [])
  ].

name_field(DOM):-
  DOM = [
    element(label, [], ['Full name:']),
    element(input, [type=text], [])
  ].

swote(_Request):-
  swote_web(Body),
  reply_html_page(swote, [], Body).

swote_web(DOM):-
  DOM = [element(form, [], FormContent)],
  name_field(NameField),
  email_field(EmailField),
  append([NameField,EmailField], FormContent).

user:body(swote, Body) -->
  html(body([h1('SWOTE') | Body])).

user:head(swote, Head) -->
  html(head([title('SWOTE') | Head])).

