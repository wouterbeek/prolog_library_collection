:- module(rfc2616_test, []).

/** <module> RFC 2616: Unit tests

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(http/rfc2616_date)).
:- use_module(library(plunit)).





:- begin_tests(rfc2616).

test('Date', "Tue, 15 Nov 1994 08:12:31 GMT.", dateTime(1994,11,15,8,12,31,_)).
test('User-Agent', "User-Agent: CERN-LineMode/2.15 libwww/2.17b3", []).
test('Server', "Server: Apache/0.8.4", ["Apache","0.8.4"]).

test(rfc2616_date, [forall(test_date(DT))]):-
  once(string_phrase('Date'(DT), S)).

:- end_tests(rfc2616).
