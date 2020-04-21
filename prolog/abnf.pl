:- encoding(utf8).
:- module(
  abnf,
  [
  % NON-DETERMINISTIC SEQUENCE PATTERNS WITH NO SEPARATOR
    '#'//2,    % ?N, :Dcg_0
    '#'//3,    % ?N, :Dcg_1, -Args
    '*'//1,    % :Dcg_0
    '*'//2,    % :Dcg_1, ?Args
    '+'//1,    % :Dcg_0
    '+'//2,    % :Dcg_1, ?Args
    '?'//1,    % :Dcg_0
    '?'//2,    % :Dcg_1, ?Arg
    'm*'//2,   % ?M, :Dcg_0
    'm*'//3,   % ?M, :Dcg_1, -Args
    '*n'//2,   % ?N, :Dcg_0
    '*n'//3,   % ?N, :Dcg_1, -Args
    'm*n'//3,  % ?M, ?N, :Dcg_0
    'm*n'//4,  % ?M, ?N, :Dcg_1, -Args
  % DETERMINISTIC SEQUENCE PATTERNS WITH NO SEPARATOR
    '#!'//2,   % ?N, :Dcg_0
    '#!'//3,   % ?N, :Dcg_1, -Args
    '*!'//1,   % :Dcg_0
    '*!'//2,   % :Dcg_1, ?Args
    '+!'//1,   % :Dcg_0
    '+!'//2,   % :Dcg_1, ?Args
    '?!'//1,   % :Dcg_0
    '?!'//2,   % :Dcg_1, ?Arg
    'm*!'//2,  % ?M, :Dcg_0
    'm*!'//3,  % ?M, :Dcg_1, -Args
    '*n!'//2,  % ?N, :Dcg_0
    '*n!'//3,  % ?N, :Dcg_1, -Args
    'm*n!'//3, % ?M, ?N, :Dcg_0
    'm*n!'//4, % ?M, ?N, :Dcg_1, -Args
  % NON-DETERMINISTIC SEQUENCE PATTERNS WITH SEPARATOR
    '#&'//3,   % ?N, :Dcg_0, :Sep_0
    '#&'//4,   % ?N, :Dcg_1, :Sep_0, -Args
    '*&'//2,   % :Dcg_0, :Sep_0
    '*&'//3,   % :Dcg_1, :Sep_0, ?Args
    '+&'//2,   % :Dcg_0, :Sep_0
    '+&'//3,   % :Dcg_1, :Sep_0, ?Args
    'm*&'//3,  % ?M, :Dcg_0, :Sep_0
    'm*&'//4,  % ?M, :Dcg_1, :Sep_0, -Args
    '*&n'//3,  % ?N, :Dcg_0, :Sep_0
    '*&n'//4,  % ?N, :Dcg_1, :Sep_0, -Args
    'm*&n'//4, % ?M, ?N, :Dcg_0, :Sep_0
    'm*&n'//5  % ?M, ?N, :Dcg_1, :Sep_0, -Args
  ]
).

/** <module> RegEx-like extensions for DCG

This module introduces support for the *variable repetition*
meta-syntactic construct as defined in RFC 5234.  It also offers
several other metasyntactic constructs that are specializations of
variable repetition, including *specific repetition* (`#'), *Kleene
star* (`*'), *Kleene sum* (`+'), and *optional sequence* (`?').

There are variants that allow a separator Sep_0 to be processed in
between productions of Dcg_n.  This covers several very common cases,
like comma-separated lists or tokens separated by whitespace.

This module also defines the DCGs correlates to the ISO call/[1-8]
predicates.  Since DCGs take two extra predicate, we can only define
dcg_call//[1,6].

@compat RFC 5234 ― Augmented BNF for Syntax Specifications: ABNF

@see https://tools.ietf.org/html/rfc5234

*/

:- use_module(library(dcg)).

:- meta_predicate
    #(+, //, ?, ?),
    #(+, 3, -, ?, ?),
    *(//, ?, ?),
    *(3, -, ?, ?),
    +(//, ?, ?),
    +(3, -, ?, ?),
    ?(//, ?, ?),
    ?(3, ?, ?, ?),
    'm*'(?, //, ?, ?),
    'm*'(?, 3, -, ?, ?),
    '*n'(?, //, ?, ?),
    '*n'(?, 3, -, ?, ?),
    'm*n'(?, ?, //, ?, ?),
    'm*n'(?, ?, 3, -, ?, ?),
    'm*n__g'(?, ?, +, //, ?, ?),
    'm*n__g'(?, ?, +, 3, -, ?, ?),
    'm*n__p'(?, ?, +, //, ?, ?),
    'm*n__p'(?, ?, +, 3, -, ?, ?),
    '#!'(+, //, ?, ?),
    '#!'(+, 3, -, ?, ?),
    '*!'(//, ?, ?),
    '*!'(3, -, ?, ?),
    '+!'(//, ?, ?),
    '+!'(3, -, ?, ?),
    '?!'(//, ?, ?),
    '?!'(3, ?, ?, ?),
    'm*!'(?, //, ?, ?),
    'm*!'(?, 3, -, ?, ?),
    '*n!'(?, //, ?, ?),
    '*n!'(?, 3, -, ?, ?),
    'm*n!'(?, ?, //, ?, ?),
    'm*n!'(?, ?, 3, -, ?, ?),
    'm*n__p!'(?, ?, +, //, ?, ?),
    'm*n__p!'(?, ?, +, 3, -, ?, ?),
    #&(+, //, //, ?, ?),
    #&(+, 3, //, -, ?, ?),
    *&(//, //, ?, ?),
    *&(3, //, -, ?, ?),
    +&(//, //, ?, ?),
    +&(3, //, -, ?, ?),
    'm*&'(?, //, //, ?, ?),
    'm*&'(?, 3, //, -, ?, ?),
    '*&n'(?, //, //, ?, ?),
    '*&n'(?, 3, //, -, ?, ?),
    'm*&n'(?, ?, //, //, ?, ?),
    'm*&n'(?, ?, 3, //, -, ?, ?),
    'm*&n__g'(?, ?, +, //, //, ?, ?),
    'm*&n__g'(?, ?, +, 3, //, -, ?, ?),
    'm*&n__p'(?, ?, +, //, //, ?, ?),
    'm*&n__p'(?, ?, +, 3, //, -, ?, ?).





%! #(?N, :Dcg_0)// is semidet.
%! #(?N, :Dcg_1, ?Args:list)// is semidet.

#(N, Dcg_0) -->
  'm*n'(N, N, Dcg_0).


#(N, Dcg_1, Args) -->
  'm*n'(N, N, Dcg_1, Args).



%! *(:Dcg_0)// is nondet.
%! *(:Dcg_1, ?Args:list)// is nondet.

*(Dcg_0) -->
  'm*n'(0, _, Dcg_0).


*(Dcg_1, Args) -->
  'm*n'(0, _, Dcg_1, Args).



%! +(:Dcg_0)// is nondet.
%! +(:Dcg_1, ?Args:list)// is nondet.

+(Dcg_0) -->
  'm*n'(1, _, Dcg_0).


+(Dcg_1, Args) -->
  'm*n'(1, _, Dcg_1, Args).



%! ?(:Dcg_0)// is nondet.
%! ?(:Dcg_1, ?Arg)// is nondet.

?(Dcg_0) -->
  Dcg_0.
?(_) --> "".


?(Dcg_1, Arg) -->
  dcg_call(Dcg_1, Arg).
?(_, _) --> "".



%! 'm*'(?M:nonneg, :Dcg_0)// is nondet.
%! 'm*'(?M:nonneg, :Dcg_1, ?Args:list)// is nondet.

'm*'(M, Dcg_0) -->
  'm*n'(M, _, Dcg_0).


'm*'(M, Dcg_1, Args) -->
  'm*n'(M, _, Dcg_1, Args).



%! '*n'(?N:nonneg, :Dcg_0)// is nondet.
%! '*n'(?N:nonneg, :Dcg_1, ?Args:list)// is nondet.

'*n'(N, Dcg_0) -->
  'm*n'(_, N, Dcg_0).

'*n'(N, Dcg_1, Args) -->
  'm*n'(_, N, Dcg_1, Args).



%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg_0)// is nondet.
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg_1, ?Args:list)// is nondet.

'm*n'(M, N, Dcg_0) -->
  parsing, !,
  'm*n__p'(M, N, 0, Dcg_0).
'm*n'(M, N, Dcg_0) -->
  'm*n__g'(M, N, 0, Dcg_0).

'm*n__g'(M, _, Count, _) -->
  {(var(M) -> true ; M =< Count)}.
'm*n__g'(M, N, Count1, Dcg_0) -->
  {(var(N) -> true ; Count1 < N)},
  Dcg_0,
  {Count2 is Count1 + 1},
  'm*n__g'(M, N, Count2, Dcg_0).

'm*n__p'(M, N, Count1, Dcg_0) -->
  {(var(N) -> true ; Count1 < N)},
  Dcg_0,
  {Count2 is Count1 + 1},
  'm*n__p'(M, N, Count2, Dcg_0).
'm*n__p'(M, _, Count, _) -->
  {(var(M) -> true ; M =< Count)}.


'm*n'(M, N, Dcg_1, Args) -->
  parsing, !,
  'm*n__p'(M, N, 0, Dcg_1, Args).
'm*n'(M, N, Dcg_1, Args) -->
  'm*n__g'(M, N, 0, Dcg_1, Args).

'm*n__g'(M, _, Count, _, []) -->
  {(var(M) -> true ; M =< Count)}.
'm*n__g'(M, N, Count1, Dcg_1, [H|T]) -->
  {(var(N) -> true ; Count1 < N)},
  dcg_call(Dcg_1, H),
  {Count2 is Count1 + 1},
  'm*n__g'(M, N, Count2, Dcg_1, T).

'm*n__p'(M, N, Count1, Dcg_1, [H|T]) -->
  {(var(N) -> true ; Count1 < N)},
  dcg_call(Dcg_1, H),
  {Count2 is Count1 + 1},
  'm*n__p'(M, N, Count2, Dcg_1, T).
'm*n__p'(M, _, Count, _, []) -->
  {(var(M) -> true ; M =< Count)}.



%! '#!'(?N, :Dcg_0)// is semidet.
%! '#!'(?N, :Dcg_1, ?Args:list)// is semidet.

'#!'(N, Dcg_0) -->
  'm*n!'(N, N, Dcg_0).


'#!'(N, Dcg_1, Args) -->
  'm*n!'(N, N, Dcg_1, Args).



%! '*!'(:Dcg_0)// is nondet.
%! '*!'(:Dcg_1, ?Args:list)// is nondet.

'*!'(Dcg_0) -->
  'm*n!'(0, _, Dcg_0).


'*!'(Dcg_1, Args) -->
  'm*n!'(0, _, Dcg_1, Args).



%! '+!'(:Dcg_0)// is nondet.
%! '+!'(:Dcg_1, ?Args:list)// is nondet.

'+!'(Dcg_0) -->
  'm*n!'(1, _, Dcg_0).


'+!'(Dcg_1, Args) -->
  'm*n!'(1, _, Dcg_1, Args).



%! '?!'(:Dcg_0)// is nondet.
%! '?!'(:Dcg_1, ?Arg)// is nondet.

'?!'(Dcg_0) -->
  Dcg_0, !.
'?!'(_) --> "".


'?!'(Dcg_1, Arg) -->
  dcg_call(Dcg_1, Arg), !.
'?!'(_, _) --> "".



%! 'm*!'(?M:nonneg, :Dcg_0)// is nondet.
%! 'm*!'(?M:nonneg, :Dcg_1, ?Args:list)// is nondet.

'm*!'(M, Dcg_0) -->
  'm*n!'(M, _, Dcg_0).


'm*!'(M, Dcg_1, Args) -->
  'm*n!'(M, _, Dcg_1, Args).



%! '*n!'(?N:nonneg, :Dcg_0)// is nondet.
%! '*n!'(?N:nonneg, :Dcg_1, ?Args:list)// is nondet.

'*n!'(N, Dcg_0) -->
  'm*n!'(_, N, Dcg_0).

'*n!'(N, Dcg_1, Args) -->
  'm*n!'(_, N, Dcg_1, Args).



%! 'm*n!'(?M:nonneg, ?N:nonneg, :Dcg_0)// is nondet.
%! 'm*n!'(?M:nonneg, ?N:nonneg, :Dcg_1, ?Args:list)// is nondet.

'm*n!'(M, N, Dcg_0) -->
  parsing, !,
  'm*n__p!'(M, N, 0, Dcg_0).
'm*n!'(M, N, Dcg_0) -->
  'm*n__g'(M, N, 0, Dcg_0).

'm*n__p!'(M, N, Count1, Dcg_0) -->
  {(var(N) -> true ; Count1 < N)},
  Dcg_0, !,
  {Count2 is Count1 + 1},
  'm*n__p!'(M, N, Count2, Dcg_0).
'm*n__p!'(M, _, Count, _) -->
  {(var(M) -> true ; M =< Count)}.


'm*n!'(M, N, Dcg_1, Args) -->
  parsing, !,
  'm*n__p!'(M, N, 0, Dcg_1, Args).
'm*n!'(M, N, Dcg_1, Args) -->
  'm*n__g'(M, N, 0, Dcg_1, Args).

'm*n__p!'(M, N, Count1, Dcg_1, [H|T]) -->
  {(var(N) -> true ; Count1 < N)},
  dcg_call(Dcg_1, H), !,
  {Count2 is Count1 + 1},
  'm*n__p!'(M, N, Count2, Dcg_1, T).
'm*n__p!'(M, _, Count, _, []) -->
  {(var(M) -> true ; M =< Count)}.



%! #&(?N, :Dcg_0, :Sep_0)// is semidet.
%! #&(?N, :Dcg_1, :Sep_0, ?Args:list)// is semidet.

#&(N, Dcg_0, Sep_0) -->
  'm*&n'(N, N, Dcg_0, Sep_0).


#&(N, Dcg_1, Sep_0, Args) -->
  'm*&n'(N, N, Dcg_1, Sep_0, Args).



%! *&(:Dcg_0, :Sep_0)// is nondet.
%! *&(:Dcg_1, :Sep_0, ?Args:list)// is nondet.

*&(Dcg_0, Sep_0) -->
  'm*&n'(0, _, Dcg_0, Sep_0).


*&(Dcg_1, Sep_0, Args) -->
  'm*&n'(0, _, Dcg_1, Sep_0, Args).



%! +&(:Dcg_0, :Sep_0)// is nondet.
%! +&(:Dcg_1, :Sep_0, ?Args:list)// is nondet.

+&(Dcg_0, Sep_0) -->
  'm*&n'(1, _, Dcg_0, Sep_0).


+&(Dcg_1, Sep_0, Args) -->
  'm*&n'(1, _, Dcg_1, Sep_0, Args).



%! 'm*&'(?M:nonneg, :Dcg_0, :Sep_0)// is nondet.
%! 'm*&'(?M:nonneg, :Dcg_1, :Sep_0, ?Args:list)// is nondet.

'm*&'(M, Dcg_0, Sep_0) -->
  'm*&n'(M, _, Dcg_0, Sep_0).


'm*&'(M, Dcg_1, Sep_0, Args) -->
  'm*&n'(M, _, Dcg_1, Sep_0, Args).



%! '*&n'(?N:nonneg, :Dcg_0, :Sep_0)// is nondet.
%! '*&n'(?N:nonneg, :Dcg_1, :Sep_0, ?Args:list)// is nondet.

'*&n'(N, Dcg_0, Sep_0) -->
  'm*&n'(_, N, Dcg_0, Sep_0).

'*&n'(N, Dcg_1, Sep_0, Args) -->
  'm*&n'(_, N, Dcg_1, Sep_0, Args).



%! 'm*&n'(?M:nonneg, ?N:nonneg, :Dcg_0, :Sep_0)// is nondet.
%! 'm*&n'(?M:nonneg, ?N:nonneg, :Dcg_1, :Sep_0, ?Args:list)// is nondet.
%
% Notice that it is possible for a production of ~Sep_0~ to appear
% after the last production of ~Dcg_n~.  This meand that eager parsing
% must cut after ~(Sep_0, Dcg_n)~, and not after ~Sep_0~ alone.

'm*&n'(M, N, Dcg_0, Sep_0) -->
  parsing, !,
  'm*&n__p'(M, N, 0, Dcg_0, Sep_0).
'm*&n'(M, N, Dcg_0, Sep_0) -->
  'm*&n__g'(M, N, 0, Dcg_0, Sep_0).

'm*&n__g'(M, _, Count, _, _) -->
  {(var(M) -> true ; M =< Count)}.
'm*&n__g'(M, N, Count1, Dcg_0, Sep_0) -->
  {(var(N) -> true ; Count1 < N)},
  ({Count1 =:= 0} -> "" ; Sep_0),
  Dcg_0,
  {Count2 is Count1 + 1},
  'm*&n__g'(M, N, Count2, Dcg_0, Sep_0).

'm*&n__p'(M, N, Count1, Dcg_0, Sep_0) -->
  {(var(N) -> true ; Count1 < N)},
  ({Count1 =:= 0} -> "" ; Sep_0),
  Dcg_0,
  {Count2 is Count1 + 1},
  'm*&n__p'(M, N, Count2, Dcg_0, Sep_0).
'm*&n__p'(M, _, Count, _, _) -->
  {(var(M) -> true ; M =< Count)}.


'm*&n'(M, N, Dcg_1, Sep_0, Args) -->
  parsing, !,
  'm*&n__p'(M, N, 0, Dcg_1, Sep_0, Args).
'm*&n'(M, N, Dcg_1, Sep_0, Args) -->
  'm*&n__g'(M, N, 0, Dcg_1, Sep_0, Args).

'm*&n__g'(M, _, Count, _, _, []) -->
  {(var(M) -> true ; M =< Count)}.
'm*&n__g'(M, N, Count1, Dcg_1, Sep_0, [H|T]) -->
  {(var(N) -> true ; Count1 < N)},
  ({Count1 =:= 0} -> "" ; Sep_0),
  dcg_call(Dcg_1, H),
  {Count2 is Count1 + 1},
  'm*&n__g'(M, N, Count2, Dcg_1, Sep_0, T).

'm*&n__p'(M, N, Count1, Dcg_1, Sep_0, [H|T]) -->
  {(var(N) -> true ; Count1 < N)},
  ({Count1 =:= 0} -> "" ; Sep_0),
  dcg_call(Dcg_1, H),
  {Count2 is Count1 + 1},
  'm*&n__p'(M, N, Count2, Dcg_1, Sep_0, T).
'm*&n__p'(M, _, Count, _, _, []) -->
  {(var(M) -> true ; M =< Count)}.
