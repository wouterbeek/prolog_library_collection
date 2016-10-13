:- module(qtip2, []).

/** <module> qTip²

@author Wouter Beek
@see http://qtip2.com/
@version 2016/07
*/

:- use_module(library(debug)).
:- use_module(library(html/html_resource)).
:- use_module(library(http/html_head)).





:- if(debugging(css(qtip2))).
  :- html_resource(
       css(qtip2),
       [requires([css('qtip2-3.0.3.css')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       css(qtip2),
       [requires([css('qtip2-3.0.3.min.css')]),virtual(true)]
     ).
:- endif.

:- if(debugging(js(qtip2))).
  :- html_resource(
       js(qtip2),
       [requires([js('qtip2-3.0.3.js')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       js(qtip2),
       [requires([js('qtip2-3.0.3.min.js')]),virtual(true)]
     ).
:- endif.

:- html_resource(qtip2, [requires([css(qtip2),js(qtip2)]),virtual(true)]).
