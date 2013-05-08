:- module(
  http,
  [
    serve_nothing/1, % +Request
    serve_xml/1, % +XML
    serve_xml/3 % +DTD_Name:atom
                % +Style_Name:atom
                % +DOM
  ]
).

/** <module> HTTP

Predicates for sending out HTTP requests.

@author Wouter Beek
@version 2013/02
*/

:- use_module(library(http/http_header)).
:- use_module(xml(xml)).



serve_nothing(Request):-
  memberchk(pool(client(_, _ , _In, Out)), Request),
  http_reply_header(Out, status(no_content), []).

serve_xml(XML):-
  % The User Agent needs to know the content type.
  format('Content-type: application/xml~n~n'),
  format(XML).

serve_xml(DTD_Name, Style_Name, DOM):-
  !,
  dom_to_xml(DTD_Name, Style_Name, DOM, XML),
  serve_xml(XML).

