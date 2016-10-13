:- module(
  html_highlight,
  [
    html_highlight//0,
    pl_code//1 % :Content_2
  ]
).

/** <module> HTML highlight

Code highlighting in HTML.

@author Wouter Beek
@version 2016/03, 2016/06
*/

:- use_module(library(html/html_ext)).
:- use_module(library(html/html_resource)).
:- use_module(library(http/html_write)).
:- use_module(library(http/jquery)).
:- use_module(library(http/js_write)).
:- use_module(library(settings)).

:- html_meta
   pl_code(html, ?, ?).

:- set_setting(jquery:version, '2.2.0.min').

:- html_resource(
     css(highlight),
     [requires([css('highlight-0.9.2.css')]),virtual(true)]
   ).

:- html_resource(
     js(highlight),
     [ordered(true),requires([jquery,js('highlight-0.9.2.js')]),virtual(true)]
   ).

:- html_resource(
     highlight,
     [requires([css(highlight),js(highlight)]),virtual(true)]
   ).





html_highlight -->
  html([
    \html_requires(highlight),
    \js_script({|javascript(_)||
      hljs.initHighlightingOnLoad();
    |})
  ]).



pl_code(Content_2) -->
  html(pre(code(class=prolog, Content_2))).
