:- module(
  reverse_polish_notation,
  [
    rpn/2 % +Notation:list(atomic)
          % -Outcome:number
  ]
).

/** <module> Reverse Polish Notation (RPN)

Evaluating arithmetic expressions in Reverse Polish Notation (RPN).

# Chain calculation method

Described by HP for their series of RPN calculators:

```
As was demonstrated in the Algebraic mode, it is usually easier
(fewer keystrokes) in working a problem like this to begin with
the arithmetic operations inside the parentheses first.
```

E.g., [2] i.o. [1]:

```prolog
[1]   rpn([5,1,2,+,4,*,+,3,-], 14).
[2]   rpn([1,2,+,4,*,5,+,3,-], 14).
```

---

@author Wouter Beek
@version 2014/10
*/

:- use_module(plc(generics/op_ext)).





%! rpn(+Notation:list(atomic), -Outcome:number) is det.

rpn(Notation, Outcome):-
  rpn(Notation, [], Outcome).


%! rpn(+Notation:list(atomic), +Stack:list(atomic), -Outcome:number) is det.

% Done!
rpn([], [Outcome], Outcome):-
  number(Outcome).
% Push operands onto the stack.
rpn([Operand|Notation], Stack, Outcome):-
  number(Operand), !,
  rpn(Notation, [Operand|Stack], Outcome).
% Evaluate n-ary operators w.r.t. the top n operands on the stack.
rpn([Op|Notation], Stack, Outcome):-
  % Notice that there can be multiple operators with the same name.
  current_op(_, OpType, Op),
  op_type_arity(OpType, OpArity),

  % Select the appropriate operands.
  length(OperandsRev, OpArity),
  append(OperandsRev, NewStack, Stack),

  % Apply the operator to its operands.
  reverse(OperandsRev, Operands),
  Expression =.. [Op|Operands],
  Result is Expression,

  rpn(Notation, [Result|NewStack], Outcome).
