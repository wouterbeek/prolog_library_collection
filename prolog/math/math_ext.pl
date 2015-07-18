:- module(
  math_ext,
  [
    absolute/2, % ?Number:number
                % ?Absolute:number
    average/2, % +Numbers:list(number)
               % -Average:number
    betwixt/3, % +Low:integer
               % +High:integer
               % ?Value:integer
    betwixt/4, % +Low:integer
               % +High:integer
               % +Interval:integer
               % ?Value:integer
    binomial_coefficient/3, % +M:integer
                            % +N:integer
                            % -BinomialCoefficient:integer
    circumfence/2, % +Radius:float
                   % -Circumfence:float
    combinations/3, % +NumberOfObjects:integer
                    % +CombinationLength:integer
                    % -NumberOfCombinations:integer
    decimal_parts/3, % ?Decimal:compound
                     % ?Integer:integer
                     % ?Fraction:compound
    div_zero/3,
    euclidean_distance/3, % +Coordinate1:coordinate
                          % +Coordinate2:coordinate
                          % -EuclideanDistance:float
    even/1, % +Integer:integer
    factorial/2, % +N:integer
                 % -F:integer
    fibonacci/2, % ?Index:integer
                 % ?Fibonacci:integer
    fractional_integer/2, % +Number:or([float,integer,rational])
                          % -Fractional:integer
    is_fresh_age/2, % +Age:between(0.0,inf)
                    % +FreshnessLifetime:between(0.0,inf)
    is_stale_age/2, % +Age:between(0.0,inf)
                    % +FreshnessLifetime:between(0.0,inf)
    log/3, % +Base:integer
           % +X:float
           % +Y:float
    minus/3, % ?X:number
             % ?Y:number
             % ?Z:number
    mod/3,
    normalized_number/3, % +Decimal:compound
                         % -NormalizedDecimal:compound
                         % -Exponent:nonneg
    number_length/2, % +Number:number
                     % -Length:integer
    number_length/3, % +Number:number
                     % +Radix:integer
                     % -Length:integer
    odd/1, % +Integer:integer
    permutations/2, % +NumberOfObjects:integer
                    % -NumberOfPermutations:integer
    permutations/3, % +NumbersOfObjects:list(integer)
                    % +PermutationLength:integer
                    % -NumberOfPermutations:integer
    permutations/3, % +NumberOfObjects:integer
                    % +PermutationLength:integer
                    % -NumberOfPermutations:integer
    pred/2, % +X:integer
            % -Y:integer
    square/2, % ?N:float
              % ?Square:float
    succ_inf/2 % ?X:integer
               % ?Y:integer
  ]
).

/** <module> Math extensions

Extra arithmetic operations for use in SWI-Prolog.

### Library `apply`

Notice that many arithmetic operations can be defined with `library(apply)`.

The following produces the same result as `sum_list([1,-2,3], X)`:

```prolog
?- foldl(plus, [1,-2,3], 0, X).
X = 2.
```

The following calculates the outcome of the given `minus list':

```prolog
?- foldl(\Y^X^Z^(Z is X - Y), [1,-2,3], 0, X).
X = -2.
```

The following calculates the outcome of the given `multiplication list':

```prolog
?- foldl(\Y^X^Z^(Z is X * Y), [1,-2,3], 1, X).
X = -6.
```

---

@Author Wouter Beek
@version 2015/07
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(typecheck)).





%! absolute(+Number:number, +Absolute:number) is semidet.
%! absolute(+Number:number, -Absolute:number) is det.
%! absolute(-Number:number, +Absolute:number) is multi.
% Succeeds if Absolute is the absolute value of Number.
%
% @throws instantiation_error
% @throws type_error
%
% ### Examples
%
% ```prolog
% ?- absolute(1, Y).
% Y = 1.
%
% ?- absolute(-1, Y).
% Y = 1.
%
% ?- absolute(X, 1).
% X = 1 ;
% X = -1.
% ```
%
% ### CLP(FD)
%
% The integer version of this predicate could have been written
% in CLP(FD):
%
% ```prolog
% :- use_module(library(clpfd)).
% absolute(X, Y):- Y #= abs(X), label([X,Y]).
% ```

absolute(N, Abs):-
  nonvar(N),
  must_be(number, N), !,
  Abs is abs(N).
absolute(N, Abs):-
  nonvar(Abs),
  must_be(number, Abs), !,
  (   N is Abs
  ;   N is -1 * Abs
  ).
absolute(_, _):-
  instantiation_error(_).



%! average(+Numbers:list(number), +Average:number) is semidet.
%! average(+Numbers:list(number), -Average:number) is det.
% @throws instantiation_error if Numbers is non-ground.
%
% ### Examples
%
% ```prolog
% ?- average([1 rdiv 3, 1 rdiv 6], X).
% X = 1 rdiv 4.
% ```
%
% ### Special cases
%
% Average is the integer 0 in case Numbers is the empty list.
% This is in line with sum_list/2.

average([], 0):- !.
average(L, Average):-
  sum_list(L, Sum),
  length(L, N),
  Average is Sum / N.



%! betwixt(+Low:integer, +High:integer, +Value:integer) is semidet.
%! betwixt(+Low:integer, +High:integer, -Value:integer) is multi.
%! betwixt(-Low:integer, +High:integer, +Value:integer) is semidet.
%! betwixt(-Low:integer, +High:integer, -Value:integer) is multi.
%! betwixt(+Low:integer, -High:integer, +Value:integer) is semidet.
%! betwixt(+Low:integer, -High:integer, -Value:integer) is multi.
% Like ISO between/3, but allowing either `Low` or `High`
% to be uninstantiated.
%
% We allow `Low` to be instantiated to `minf` and `High` to be
% instantiated to `inf`.
% In these cases, their values are replaced by fresh variables.

betwixt(Low, High, Value):-
  betwixt_bound(low, Low, Low0),
  betwixt_bound(high, High, High0),
  betwixt0(Low0, High0, Value).

% Behavior of ISO between/3.
betwixt0(Low, High, Value):-
  nonvar(Low),
  nonvar(High), !,
  between(Low, High, Value).
% The higher bound is missing.
betwixt0(Low, High, Value):-
  nonvar(Low), !,
  betwixt_low(Low, Low, High, Value).
% The lower bound is missing.
betwixt0(Low, High, Value):-
  nonvar(High), !,
  betwixt_high(Low, High, High, Value).
% Instantiation error: at least one bound must be present.
betwixt0(_, _, _):-
  instantiation_error(_).

betwixt_bound(high, inf, _):- !.
betwixt_bound(low, minf, _):- !.
betwixt_bound(_, High, High):-
  var(High), !.
betwixt_bound(_, High, High):-
  must_be(integer, High).

betwixt_high(_, Value, _, Value).
betwixt_high(Low, Between1, High, Value):-
  succ(Between2, Between1),
  betwixt_high(Low, Between2, High, Value).

betwixt_low(_, Value, _, Value).
betwixt_low(Low, Between1, High, Value):-
  succ(Between1, Between2),
  betwixt_low(Low, Between2, High, Value).



%! betwixt(
%!   +Low:integer,
%!   +High:integer,
%!   +Interval:integer,
%!   +Value:integer
%! ) is semidet.
%! betwixt(
%!   +Low:integer,
%!   +High:integer,
%!   +Interval:integer,
%!   -Value:integer
%! ) is nondet.

betwixt(Low, _, _, Low).
betwixt(Low0, High, Interval, Value):-
  Low is Low0 + Interval,
  (   High == inf
  ->  true
  ;   Low =< High
  ),
  betwixt(Low, High, Interval, Value).



binomial_coefficient(M, N, BC):-
  factorial(M, F_M),
  factorial(N, F_N),
  MminN is M - N,
  factorial(MminN, F_MminN),
  BC is F_M / (F_N * F_MminN).


%! circumfence(+Radius:float, -Circumfence:float) is det.
% Returns the circumfence of a circle with the given radius.

circumfence(Rad, Circ):-
  Circ is Rad * pi * 2.



%! combinations(
%!   +NumberOfObjects:integer,
%!   +CombinationLength:integer,
%!   -NumberOfCombinations:integer
%! ) is det.
% Returns the number of combinations from the given objects and
% of the given size.
%
% *Definition*: A combination is a permutation in which the order
%               neglected. Therefore, $r!$ permutations correspond to
%               one combination (with r the combination length).

combinations(NObjects, CombinationLength, NCombinations):-
  permutations(NObjects, CombinationLength, NPermutations),
  factorial(CombinationLength, F),
  NCombinations is NPermutations / F.



%! decimal_parts(
%!   +Decimal:number,
%!   -Integer:integer,
%!   -Fractional:nonneg
%! ) is det.
%! decimal_parts(
%!   -Decimal:number,
%!   +Integer:integer,
%!   +Fractional:nonneg
%! ) is det.
% @throws instantation_error
% @throws type_error
%
% ### Examples
%
% ```prolog
% 
% ```

decimal_parts(N, I, Frac):-
  nonvar(N),
  must_be(number, N), !,
  I is floor(float_integer_part(N)),
  fractional_integer(N, Frac).
decimal_parts(N, I, Frac):-
  must_be(integer, I),
  must_be(nonneg, Frac),
  number_length(Frac, L),
  N is copysign(abs(I) + (Frac rdiv (10 ^ L)), I).



div_zero(X, 0, 0):-
  integer(X), !.
div_zero(X, 0.0, 0.0):-
  float(X), !.
div_zero(X, Y, Z):-
  Z is X / Y.


%! euclidean_distance(
%!   +Coordinate1:coordinate,
%!   +Coordinate2:coordinate,
%!   -EuclideanDistance:float
%! ) is det.
% Returns the Euclidean distance between two coordinates.

euclidean_distance(
  coordinate(Dimension, Args1),
  coordinate(Dimension, Args2),
  EuclideanDistance
):-
  maplist(minus, Args1, Args2, X1s),
  maplist(square, X1s, X2s),
  sum_list(X2s, X2),
  EuclideanDistance is sqrt(X2).


%! even(+Number:number) is semidet.
% Succeeds if the integer is even.

even(N):-
  mod(N, 2, 0).


%! factorial(+N:integer, -F:integer) is det.
% Returns the factorial of the given number.
%
% The standard notation for the factorial of _|n|_ is _|n!|_.
%
% *Definition*: $n! = \prod_{i = 1}^n i$

factorial(N, F):-
  numlist(1, N, Ns), !,
  foldl(\Y^X^Z^(Z is X * Y), Ns, 1, F).  
% E.g., $0!$.
factorial(_, 1).

fibonacci(0, 1):- !.
fibonacci(1, 1):- !.
fibonacci(N, F):-
  N1 is N - 1,
  N2 is N - 2,
  fibonacci(N1, F1),
  fibonacci(N2, F2),
  F is F1 + F2.



%! fractional_integer(
%!   +Number:or([float,integer,rational]),
%!   -Fractional:integer
%! ) is det.
% Variant of float_fractional_part/2 where
% the integer value of the fractional part is returned.

fractional_integer(N, Frac):-
  N = A rdiv B, !,
  Float is A / B,
  fractional_integer(Float, Frac).
fractional_integer(Number, Frac):-
  atom_number(NumberAtom, Number),
  % We assume that there is at most one occurrence of `.`.
  sub_atom(NumberAtom, IndexOfDot, 1, _, '.'), !,
  succ(IndexOfDot, Skip),
  sub_atom(NumberAtom, Skip, _, 0, FracAtom),
  atom_number(FracAtom, Frac).
fractional_integer(_, 0).


%! is_fresh_age(
%!   +Age:between(0.0,inf),
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.

is_fresh_age(_, inf):- !.
is_fresh_age(Age, FreshnessLifetime):-
  Age =< FreshnessLifetime.


%! is_stale_age(
%!   +Age:between(0.0,inf),
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.

is_stale_age(_, inf):- !, fail.
is_stale_age(Age, FreshnessLifetime):-
  Age > FreshnessLifetime.


%! log(+Base:integer, +X:integer, -Y:double) is det.
% Logarithm with arbitrary base `Y = log_{Base}(X)`.
%
% @arg Base An integer.
% @arg X An integer.
% @arg Y A double.

log(Base, X, Y):-
  Numerator is log(X),
  Denominator is log(Base),
  Y is Numerator / Denominator.


%! minus(+X:number, +Y:number, +Z:number) is semidet.
%! minus(+X:number, +Y:number, -Z:number) is det.
%! minus(+X:number, -Y:number, +Z:number) is det.
%! minus(-X:number, +Y:number, +Z:number) is det.

minus(X, Y, Z):-
  nonvar(X), nonvar(Y), !,
  Z is X - Y.
minus(X, Y, Z):-
  nonvar(X), nonvar(Z), !,
  Y is X - Z.
minus(X, Y, Z):-
  nonvar(Y), nonvar(Z), !,
  X is Y + Z.


mod(X, Y, Z):-
  rational(X),
  rational(Y), !,
  rational_mod(X, Y, Z).
mod(X, Y, Z):-
  float(X),
  float(Y), !,
  DIV is X / Y,
  Z is X - round(DIV) * Y.


%! normalized_number(
%!   +Decimal:compound,
%!   -NormalizedDecimal:compound,
%!   -Exponent:integer
%! ) is det.
% A form of **Scientific notation**, i.e., $a \times 10^b$,
% in which $0 \leq a < 10$.
%
% The exponent $b$ is negative for a number with absolute value between
% $0$ and $1$ (e.g. $0.5$ is written as $5×10^{-1}$).
%
% The $10$ and exponent are often omitted when the exponent is $0$.

normalized_number(D, D, 0):-
  1.0 =< D,
  D < 10.0, !.
normalized_number(D1, ND, Exp1):-
  D1 >= 10.0, !,
  D2 is D1 / 10.0,
  normalized_number(D2, ND, Exp2),
  Exp1 is Exp2 + 1.
normalized_number(D1, ND, Exp1):-
  D1 < 1.0, !,
  D2 is D1 * 10.0,
  normalized_number(D2, ND, Exp2),
  Exp1 is Exp2 - 1.



%! number_length(+Number:number, -Length:integer) is det.
% @see number_length/3 with radix set to `10` (decimal).

number_length(M, L):-
  number_length(M, 10.0, L).

%! number_length(+Number:number, +Radix:integer, -Length:integer) is det.
% Returns the length of the given number 'before the dot'.
% The number is in decimal notation.
%
% @arg An integer representing a decimal number.
% @arg Radix An integer representing the radix used.
%      Common values are `2` (binary), `8` (octal),
%      `10` (decimal), and `16` (hexadecimal).
% @arg Length An integer representing the number of digits in
%      the given number.

number_length(N1, Radix, L1):-
  N2 is N1 / Radix,
  N2 >= 1.0, !,
  number_length(N2, Radix, L2),
  L1 is L2 + 1.
number_length(_N, _Radix, 1):- !.



%! odd(?Number:number) is semidet.
% Succeeds if the integer is odd.

odd(N):-
  mod(N, 2, 1).



%! permutations(
%!   +NumbersOfObjects:list(integer),
%!   -NumberOfPermutations:integer
%! ) is det.
%! permutations(
%!   +NumberOfObjects:integer,
%!   -NumberOfPermutations:integer
%! ) is det.
% Returns the number of permutations that can be created with
% the given number of distinct objects.
%
% @see permutations/3

permutations(NumbersOfObjects, NumberOfPermutations):-
  is_list(NumbersOfObjects), !,
  sum_list(NumbersOfObjects, NumberOfObjects),
  permutations(NumbersOfObjects, NumberOfObjects, NumberOfPermutations).
permutations(NumberOfObjects, NumberOfPermutations):-
  permutations([NumberOfObjects], NumberOfPermutations).

%! permutations(
%!   +NumbersOfObjects:list(integer),
%!   +PermutationLength:integer,
%!   -NumberOfPermutations:integer
%! ) is det.
%! permutations(
%!   +NumberOfObjects:integer,
%!   +PermutationLength:integer,
%!   -NumberOfPermutations:integer
%! ) is det.
% Returns the number of permutations that can be created with
% the given numbers of distinct objects and that have (exactly)
% the given length.
%
% *Definition*: The number of permutations of _|m|_ groups of unique objects
%               (i.e., types) and with _|n_i|_ group members or occurences
%               (i.e., tokens), for $0 \leq i \leq m$ and that are (exactly)
%               of length _|r|_ is $\frac{n!}{\mult_{i = 1}^m(n_i!)(n - r)!}$.
%
% @arg NumbersOfObject A list of numbers, each indicating the number of
%      objects in a certain group.
% @arg PermutationLength The (exact) number of objects that occur
%      in a permutation.
% @arg NumberOfPermutations The number of permutations that can be created.

permutations(NumbersOfObjects, PermutationLength, NumberOfPermutations):-
  is_list(NumbersOfObjects), !,

  % The objects.
  sum_list(NumbersOfObjects, NumberOfObjects),
  factorial(NumberOfObjects, F1),

  % The length compensation.
  Compensation is NumberOfObjects - PermutationLength,
  factorial(Compensation, F3),

  % The groups.
  maplist(factorial, NumbersOfObjects, F2s),
  foldl(\Y^X^Z^(Z is X * Y), [F3|F2s], 1, F23),

  NumberOfPermutations is F1 / F23.
permutations(NumberOfObjects, PermutationLength, NumberOfPermutations):-
  factorial(NumberOfObjects, F1),
  Compensation is NumberOfObjects - PermutationLength,
  factorial(Compensation, F2),
  NumberOfPermutations is F1 / F2.



%! pred(?X:integer, ?Y:integer)
% Succeeds if Y is the direct predecessor of X.
%
% Variant of built-in succ/2.

pred(X, Y):-
  succ(Y, X).



%! square(+X:float, +Square:float) is semidet.
%! square(+X:float, -Square:float) is det.
%! square(-X:float, +Square:float) is det.
% Returns the square of the given number.

square(N, Sq):-
  nonvar(N), !,
  Sq is N ** 2.
square(N, Sq):-
  N is sqrt(Sq).



%! succ_inf(+X:or([oneof([inf]),integer]), +Y:or([oneof([inf]),integer])) is semidet.
%! succ_inf(+X:or([oneof([inf]),integer]), -Y:or([oneof([inf]),integer])) is det.
%! succ_inf(-X:or([oneof([inf]),integer]), +Y:or([oneof([inf]),integer])) is det.
% Variant of succ/2 that allows the value `inf` to be used.

succ_inf(inf, inf):- !.
succ_inf(X, Y):-
  succ(X, Y).
