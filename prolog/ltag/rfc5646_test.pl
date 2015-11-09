:- module(rfc5646_test, []).

/** <module> RFC 5646: Unit tests

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(plunit)).





:- begin_tests(dcg_ltag_test).

test_ltag("zh").
test_ltag("zh-Latn").
test_ltag("zh-Latn-CN").
test_ltag("zh-Latn-CN-variant1").
test_ltag("zh-Latn-CN-variant1-a-extend1").
test_ltag("zh-Latn-CN-variant1-a-extend1-x-wadegile").
test_ltag("zh-Latn-CN-variant1-a-extend1-x-wadegile-private1").

test(dcg_ltag_parse, [forall(test_ltag(LTag))]):-
  once(string_phrase('Language-Tag'(LTag), _)).

:- end_tests(dcg_ltag_test).
