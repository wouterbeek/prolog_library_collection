:- module(
  dcg_peek,
  [
    dcg_peek//1, % :Dcg_0
    dcg_peek//2, % +Length:nonneg
                 % ?Codes:list(code)
    dcg_peek_code//1 % ?Code:code
  ]
).

/** <module> DCG peek

Peeking in difference lists.

@author Wouter Beek
@version 2015/07, 2015/11
*/

:- meta_predicate(dcg_peek(//,?,?)).
:- meta_predicate(dcg_peek(+,?,?,?)).





%! dcg_peek(:Dcg_0)// is det.
% Returns the next code in the codes list, if any.
% Does not consume anything.

dcg_peek(Dcg_0), Dcg_0 -->
  Dcg_0.



%! dcg_peek(+Length:nonneg, ?Peek:list(code))// is nondet.

dcg_peek(Length, Peek), Peek -->
  {length(Peek, Length)},
  Peek.



dcg_peek_code(C), [C] --> [C].
