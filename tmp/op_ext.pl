:- module(
  op_ext,
  [
    op_arity/2, % +OperatorName:atom
                % +Arity:nonneg
    op_type_arity/2 % ?OperatorType:op_type
                    % ?Arity:nonneg
  ]
).

/** <module> Operator extensions

Extensions to operator support in Prolog.

@author Wouter Beek
@version 2014/10
*/

:- use_module(library(error)).

:- use_module(plc(prolog/pl_mode)).

:- multifile(error:has_type/2).

error:has_type(op_type, Term):-
  error:has_type(oneof([fx,fy,xf,xfx,xfy,yf,yfx]), Term).



%! op_arity(+OperatorName:atom, +Arity:nonneg) is semidet.
%! op_arity(+OperatorName:atom, -Arity:nonneg) is nondet.
%! op_arity(-OperatorName:atom, +Arity:nonneg) is nondet.
%! op_arity(-OperatorName:atom, -Arity:nonneg) is nondet.
% @throws domain_error If Arity is instantiated to a negative integer.
% @throws existence_error If OperatorName does not denote
%                         a currently declared operator.
% @throws type_error If Arity is instantiated to a non-integer.

op_arity(Op, Arity):-
  (   current_op(_, OpType, Op)
  ->  op_type_arity(OpType, Arity)
  ;   existence_error(callable, Op)
  ).



%! op_type_arity(+OperatorType:op_type, +Arity:nonneg) is semidet.
%! op_type_arity(+OperatorType:op_type, -Arity:nonneg) is det.
%! op_type_arity(-OperatorType:op_type, +Arity:nonneg) is multi.
%! op_type_arity(-OperatorType:op_type, -Arity:nonneg) is multi.
% @throws type_error If OpType or Arity are instanted to values
%                    that are not of the required types.

op_type_arity(OpType, Arity):-
  (   nonvar(OpType)
  ->  must_be(op_type, OpType)
  ;   true
  ),
  (   nonvar(Arity)
  ->  must_be(nonneg, Arity)
  ;   true
  ),
  call_det(op_type_arity_fact, nonvar-OpType, nonvar-Arity).

op_type_arity_fact(fx,  1).
op_type_arity_fact(fy,  1).
op_type_arity_fact(xf,  1).
op_type_arity_fact(xfx, 2).
op_type_arity_fact(xfy, 2).
op_type_arity_fact(yf,  1).
op_type_arity_fact(yfx, 2).
