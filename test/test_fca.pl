:- use_module(library(aggregate)).
:- use_module(library(fca/fca)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).


:- begin_tests('os2as/3').

test(
  'os2as(+,+,-) is det. TRUE',
  [forall(os2as_test(Context,Os,As,true))]
):-
  os2as(Context, Os, As).
test(
  'os2as(+,+,-) is det. FALSE',
  [fail,forall(os2as_test(Context,Os,As,true))]
):-
  context_components(Context, _, AllAs, _),
  ord_subset(CounterExample, AllAs),
  CounterExample \== As,
  os2as(Context, Os, CounterExample).

os2as_test(tab(1), [x2], [y1,y3,y4], true).
os2as_test(tab(1), [x2,x3], [y3,y4], true).
os2as_test(tab(1), [x1,x4,x5], [], true).

:- end_tests('os2as/3').
