:- module(
  html_jq,
  [
    jq_accordion//1,     % :Content_1
    jq_accordion_item//2 % :Head_0, :Body_0
  ]
).

/** <module> HTML jQuery & jQuery UI

These DSL rules require jQuery and jQuery UI to be loaded.

@author Wouter Beek
@version 2015/08, 2016/06
*/

:- use_module(library(debug)).
:- use_module(library(html/html_resource)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/jquery)).
:- use_module(library(http/js_write)).
:- use_module(library(settings)).

:- set_setting(jquery:version, '3.1.1.min').

:- if(debugging(css('jquery-css'))).
  :- html_resource(
    css('jquery-ui'),
    [requires([css('jquery-ui-1.12.1.css')]),virtual(true)]
  ).
:- else.
  :- html_resource(
    css('jquery-ui'),
    [requires([css('jquery-ui-1.12.1.min.css')]),virtual(true)]
  ).
:- endif.

:- if(debugging(js('jquery-ui'))).
  :- html_resource(
    js('jquery-ui'),
    [ordered(true),requires([jquery,js('jquery-ui-1.12.1.js')]),virtual(true)]
  ).
:- else.
  :- html_resource(
    js('jquery-ui'),
    [ordered(true),requires([jquery,js('jquery-ui-1.12.1.min.js')]),virtual(true)]
  ).
:- endif.

:- html_resource(
  'jquery-ui',
  [requires([css('jquery-ui'),js('jquery-ui')]),virtual(true)]
).

:- html_meta
   jq_accordion_item(html, html, ?, ?).

:- meta_predicate
    jq_accordion(3, ?, ?).





%! jq_accordion(:Content_1)// is det.

jq_accordion(Content_1) -->
  html([
    \js_script({|javascript(_)||
$(function() {
  $( "#accordion" ).accordion();
});
    |}),
    div(id=accordion, Content_1)
  ]).



%! jq_accordion_item(:Head_0, :Body_0)// is det.

jq_accordion_item(Head_0, Body_0) -->
  html([h3(Head_0),div(Body_0)]).
