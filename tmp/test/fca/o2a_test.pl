:- module(o2a_test, []).

:- use_module(library(plunit)).

:- begin_tests('o2a(+,+,-) is det. TRUE').

:- use_module(fca_test).
:- use_module(library(fca/fca)).
:- use_module(library(ordsets)).

test(
  'o2a(+,+,-) is det. TRUE',
  [forall(os2as_test(ContextName,Os,As,true))]
):-
  fca_test(ContextName, Context),
  os2as(Context, Os, As).
test(
  'o2a(+,+,-) is det. FALSE',
  [fail,forall(os2as_test(ContextName,Os,As1,true))]
):-
  context_attributes(ContextName, AllAs),
  ord_subset(As2, AllAs),
  As2 \== As1,
  os2as(ContextName, Os, As2).

os2as_test(tab(1), [x2], [y1,y3,y4], true).
os2as_test(tab(1), [x2,x3], [y3,y4], true).
os2as_test(tab(1), [x1,x4,x5], [],   true).

:- end_tests('o2a(+,+,-) is det. TRUE').
