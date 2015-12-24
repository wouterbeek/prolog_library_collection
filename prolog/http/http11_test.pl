:- module(http11_test, []).

/** <module> HTTP 1.1: Unit tests

@author Wouter Beek
@version 2015/11-2015/12
*/

:- use_module(library(http/http11)).
:- use_module(library(plunit)).





:- begin_tests(http11).

test(date, "Tue, 15 Nov 1994 08:12:31 GMT.", datetime(1994,11,15,8,12,31,_)).
test('user-agent', "User-Agent: CERN-LineMode/2.15 libwww/2.17b3", []).
test(server, "Server: Apache/0.8.4", ["Apache","0.8.4"]).

test(http11, [forall(test(Dcg_1,S,DT))]):-
  Dcg_0 =.. [Dcg_1,DT],
  once(string_phrase(Dcg_0, S)).

:- end_tests(http11).
