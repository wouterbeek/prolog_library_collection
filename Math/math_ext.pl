:- module(
  math_ext,
  [
    average/2, % +Numbers:list(number)
               % -Average:number
    cartesian_distance/3, % +Coordinate1:coordinate
                          % +Coordinate2:coordinate
                          % -CartesianDistance:float
    circumfence/2, % +Radius:float
                   % -Circumfence:float
    combinations/3, % +NumberOfObjects:integer
                    % +CombinationLength:integer
                    % -NumberOfCombinations:integer
    cyclic_numlist/4, % +Min:integer
                      % +Max:integer
                      % +CycleLength:integer
                      % -NumList:list(integer)
    default/3, % ?Value
               % +Default:term
               % +SetValue:term
    dpred/2, % +X:integer
             % -Y:integer
    dsucc/2, % +X:integer
             % -Y:integer
    even/1, % +Integer:integer
    factorial/2, % +N:integer
                 % -F:integer
    fibonacci/2, % ?Index:integer
                 % ?Fibonacci:integer
    id/2, % ?X:integer
          % ?Y:integer
    log/3, % +Base:integer
           % +X:float
           % +Y:float
    minus/3, % ?X:number
             % ?Y:number
             % ?Z:number
    minus_list/3, % +N:number
                  % +Ms:list(number)
                  % -N_Minus_Ms:number
    multiply/3, % ?X:float
                % ?Y:float
                % ?Z:float
    multiply_list/2, % +Numbers:list(number)
                     % -Multiplication:number
    nth0chk/3, % ?Index:integer
               % ?List:List
               % ?Element
    nth1chk/3, % ?Index:integer
               % ?List:List
               % ?Element
    odd/1, % +Integer:integer
    permutations/2, % +NumberOfObjects:integer
                    % -NumberOfPermutations:integer
    permutations/3, % +NumbersOfObjects:list(integer)
                    % +PermutationLength:integer
                    % -NumberOfPermutations:integer
    permutations/3, % +NumberOfObjects:integer
                    % +PermutationLength:integer
                    % -NumberOfPermutations:integer
    plus_float/3, % ?X:number
                  % ?Y:number
                  % ?Z:number
    pred/2, % +X:integer
            % -Y:integer
    random_betwixt/2, % +UpperLimit:number
                      % -Random:number
    random_betwixt/3, % +LowerLimit:number
                      % +UpperLimit:number
                      % -Random:number
    random_coordinate/2, % +Size:size,
                         % -Coordinate:coordinate
    seconds/3, % ?Hours:integer
               % ?Minutes:integer
               % ?Second:integer
    sign_multiply/3, % +Sign1:atom
                     % +Sign2:atom
                     % -Sign:atom
    square/2 % +X:float
             % -Square:float
  ]
).

/** <module> Artihmetic extensions for SWI-Prolog

Extra arithmetic functions for use in SWI-Prolog.

---+ Types

---++ Coordinates

=|coordinate(<dimension>, <list(<float>)>)|=.

---++ Sizes

=|size(<dimension>, <list(<number>)>)|=.

@author Wouter Beek
@version 2011/08-2012/02, 2012/09-2012/10, 2012/12
*/

:- use_module(generics(meta_ext)).



average(Numbers, Average):-
  sum_list(Numbers, Sum),
  length(Numbers, NumberOfNumbers),
  Average is Sum / NumberOfNumbers.

%% cartesian_distance(
%%   +Coordinate1:coordinate,
%%   +Coordinate2:coordinate,
%%   -CartesianDistance:float
%% ) is det.
% Returns the Cartesian distance between two coordinates.

cartesian_distance(
  coordinate(Dimension, Args1),
  coordinate(Dimension, Args2),
  CartesianDistance
):-
  maplist(minus, Args1, Args2, X1s),
  maplist(square, X1s, X2s),
  sum_list(X2s, X2),
  CartesianDistance is sqrt(X2).

%% circumfence(+Radius:float, -Circumfence:float) is det.
% Returns the circumfence of a circle with the given radius.

circumfence(Radius, Circumfence):-
  Circumfence is Radius * pi * 2.

%% combinations(
%%   +NumberOfObjects:integer,
%%   +CombinationLength:integer,
%%   -NumberOfCombinations:integer
%% ) is det.
% Returns the number of combinations from the given objects and
% of the given size.
%
% *Definition*: A combination is a permutation in which the order
%               neglected. Therefore, $r!$ permutations correspond to
%               one combination (with r the combination length).

combinations(NumberOfObjects, CombinationLength, NumberOfCombinations):-
  permutations(NumberOfObjects, CombinationLength, NumberOfPermutations),
  factorial(CombinationLength, F),
  NumberOfCombinations is NumberOfPermutations / F.

%% cyclic_numlist(
%%   +Min:integer,
%%   +Max:integer,
%%   +CycleLength:integer,
%%   -NumList:list(integer)
%% ) is det.
% Generates a number list for a cyclic list of numbers.
% This method works on a off-by-zero basis.
% We return the numbers in a sorted order.

cyclic_numlist(Min, Max, _CycleLength, NumList):-
  Min < Max,
  !,
  numlist(Min, Max, NumList).
cyclic_numlist(Min, Max, CycleLength, NumList):-
  Top is CycleLength - 1,
  numlist(Min, Top, HigherNumList),
  numlist(0, Max, LowerNumList),
  append(LowerNumList, HigherNumList, NumList).

%% default(?Value, +Default:term, -SetValue:term) is det.
% Returns either the given value or the default value in case there is no
% value given.
%
% @param Value A term or a variable.
% @param Default A term.
% @param SetValue A term.

default(Value, Default, Default):-
  var(Value),
  !.
default(Value, _Default, Value).

%% dpred(+X:integer, -Y:integer) is det.
% Returns the double predecessor of the given integer.
%
% @param X An integer.
% @param Y An integer.

dpred(X, Y):-
  multi(pred, 2, X, Y).

%% dsucc(+X:integer, -NewX:integer) is det.
% Returns the double successor of the given integer.
%
% @param X An integer.
% @param NewX An integer.

dsucc(X, NewX):-
  multi(succ, 2, X, NewX).

%% even(+Integer:integer) is semidet.
% Succeeds if the integer is even.
%
% @param Integer An integer.

even(Integer):-
  0 is Integer mod 2.

%% factorial(+N:integer, -F:integer) is det.
% Returns the factorial of the given number.
%
% The standard notation for the factorial of _|n|_ is _|n!|_.
%
% *Definition*: $n! = \prod_{i = 1}^n i$

factorial(N, F):-
  numlist(1, N, Numbers),
  !,
  multiply_list(Numbers, F).
% E.g., $0!$.
factorial(_N, 1).

fibonacci(0, 1):-
  !.
fibonacci(1, 1):-
  !.
fibonacci(N, F):-
  N1 is N - 1,
  N2 is N - 2,
  fibonacci(N1, F1),
  fibonacci(N2, F2),
  F is F1 + F2.

id(X, Y):-
  X = Y.

%% log(+Base:integer, +X:integer, -Y:double) is det.
% Logarithm with arbitrary base =|Y = log_{Base}(X)|=.
%
% @param Base An integer.
% @param X An integer.
% @param Y A double.

log(Base, X, Y):-
  Numerator is log(X),
  Denominator is log(Base),
  Y is Numerator / Denominator.

minus(X, Y, Z):-
  nonvar(X),
  nonvar(Y),
  !,
  Z is X - Y.
minus(X, Y, Z):-
  nonvar(X),
  nonvar(Z),
  !,
  Y is X - Z.
minus(X, Y, Z):-
  nonvar(Y),
  nonvar(Z),
  !,
  X is Y + Z.

%% minus_list(+N:number, +Ms:list(number), -N_Minus_Ms:number) is det.
% Subtracts the given numbers for the given start number
% and returns the result.

minus_list(N, Ms, N_Minus_Ms):-
  sum_list(Ms, M),
  N_Minus_Ms is N - M.

%% multiply(+X:number, +Y:number, -Z:number) is det.
% Predicate alternative for the builtin multiplication function.
%
% @param X A number.
% @param Y A number.
% @param Z A number.

multiply(X, Y, Z):-
  Z is X * Y.

%% multiply_list(+List:list(number), -Multiplication:number) is det.
% Multiplies the numbers in the given list.
%
% @param List A list of numbers.
% @param Multiplication A number.
% @see Extends the builin list manipulators sum_list/2, max_list/2
%      and min_list/2.

multiply_list([], 0):-
  !.
multiply_list([Number], Number):-
  !.
multiply_list([Number | Numbers], Multiplication):-
  multiply_list(Numbers, Multiplication1),
  Multiplication is Number * Multiplication1.

nth0chk(Index, List, Element):-
  nth0(Index, List, Element),
  !.

nth1chk(Index, List, Element):-
  nth1(Index, List, Element),
  !.

%% odd(?Integer:integer) is semidet.
% Succeeds if the integer is odd.
%
% @param Integer An integer.

odd(Integer):-
  1 is Integer mod 2.

%% permutations(
%%   +NumbersOfObjects:list(integer),
%%   -NumberOfPermutations:integer
%% ) is det.
%% permutations(
%%   +NumberOfObjects:integer,
%%   -NumberOfPermutations:integer
%% ) is det.
% Returns the number of permutations that can be created with
% the given number of distinct objects.
%
% @see permutations/3

permutations(NumbersOfObjects, NumberOfPermutations):-
  is_list(NumbersOfObjects),
  !,
  sum_list(NumbersOfObjects, NumberOfObjects),
  permutations(NumbersOfObjects, NumberOfObjects, NumberOfPermutations).
permutations(NumberOfObjects, NumberOfPermutations):-
  permutations([NumberOfObjects], NumberOfPermutations).

%% permutations(
%%   +NumbersOfObjects:list(integer),
%%   +PermutationLength:integer,
%%   -NumberOfPermutations:integer
%% ) is det.
%% permutations(
%%   +NumberOfObjects:integer,
%%   +PermutationLength:integer,
%%   -NumberOfPermutations:integer
%% ) is det.
% Returns the number of permutations that can be created with
% the given numbers of distinct objects and that have (exactly)
% the given length.
%
% *Definition*: The number of permutations of _|m|_ groups of unique objects
%               (i.e., types) and with _|n_i|_ group members or occurences
%               (i.e., tokens), for $0 \leq i \leq m$ and that are (exactly)
%               of length _|r|_ is $\frac{n!}{\mult_{i = 1}^m(n_i!)(n - r)!}$.
%
% @param NumbersOfObject A list of numbers, each indicating the number of
%        objects in a certain group.
% @param PermutationLength The (exact) number of objects that occur
%        in a permutation.
% @param NumberOfPermutations The number of permutations that can be created.

permutations(NumbersOfObjects, PermutationLength, NumberOfPermutations):-
  is_list(NumbersOfObjects),
  !,

  % The objects.
  sum_list(NumbersOfObjects, NumberOfObjects),
  factorial(NumberOfObjects, F1),

  % The length compensation.
  Compensation is NumberOfObjects - PermutationLength,
  factorial(Compensation, F3),

  % The groups.
  maplist(factorial, NumbersOfObjects, F2s),
  multiply_list([F3 | F2s], F23),

  NumberOfPermutations is F1 / F23.
permutations(NumberOfObjects, PermutationLength, NumberOfPermutations):-
  factorial(NumberOfObjects, F1),
  Compensation is NumberOfObjects - PermutationLength,
  factorial(Compensation, F2),
  NumberOfPermutations is F1 / F2.

%% plus_float(?X:number, ?Y:number, ?Z:number) is det.
% Calculates the sum Z = X + Y as long as at least two arguments are
% instantiated.
%
% @see The builin plus/3 only works for integers.

plus_float(X, Y, Z):-
  nonvar(X),
  nonvar(Y),
  !,
  Z is X + Y.
plus_float(X, Y, Z):-
  nonvar(X),
  nonvar(Z),
  !,
  Y is Z - X.
plus_float(X, Y, Z):-
  nonvar(Y),
  nonvar(Z),
  !,
  X is Z - Y.

%% pred(?Integer:integer, ?Predecessor:integer)
% A integer and its direct predecessor integer.
%
% This is used by meta-predicates that require uniform instantiation patterns.
%
% @param Integer An integer.
% @param Predecessor An integer.
% @see This extends the builin succ/2.

pred(Integer, Predecessor):-
  succ(Predecessor, Integer).

%% random_betwixt(+UpperLimit:number, -Random:float) is det.
% @see random_betwixt/3

random_betwixt(UpperLimit, Random):-
  integer(UpperLimit),
  !,
  math_ext:random_betwixt(0, UpperLimit, Random).
random_betwixt(UpperLimit, Random):-
  float(UpperLimit),
  !,
  math_ext:random_betwixt(0.0, UpperLimit, Random).

%% random_betwixt(
%%   +LowerLimit:number,
%%   +UpperLimit:number,
%%   -Random:number
%% ) is det.
% Returns a random floating point number between the given lower and
% upper limits, inclusive.
%
% @param LowerLimit A number.
% @param UpperLimit A number.
% @param Random In case the lower and upper limits are integers, the
%	 return value is an integer as well. Otherwise it is a floating
%	 point number.
% @tbd Because we take the floor for the random value between two integers,
%      the chance that =UpperLimit= comes out is very much lower than all
%      the other values, i.e. =|[LowerLimit, UpperLimit)|=.

random_betwixt(LowerLimit, UpperLimit, Random):-
  integer(LowerLimit),
  integer(UpperLimit),
  !,
  random_betwixt0(LowerLimit, UpperLimit, Random0),
  Random is floor(Random0).
random_betwixt(LowerLimit, _UpperLimit, _Random):-
  \+ number(LowerLimit),
  type_error(number, LowerLimit).
random_betwixt(_LowerLimit, UpperLimit, _Random):-
  \+ number(UpperLimit),
  type_error(number, UpperLimit).
random_betwixt(LowerLimit, UpperLimit, Random):-
  random_betwixt0(LowerLimit, UpperLimit, Random).

random_betwixt0(LowerLimit, UpperLimit, Random):-
  Random is LowerLimit + random_float * (UpperLimit - LowerLimit).

%% random_coordinate(+Size:size, -Coordinate:coord) is det.
% Returns a random coordinate of the indicates dimension, i.e.,
% the dimension argument of the given size compound term.
%
% @param Size A compound term specifying the dimension of the generated
%        coordinate, as well as the ceiling limit values for each dimension.
%        The floor limit value for every dimension is 0.0.
% @param Coordinate A coordinate that has the same dimension as the given
%        size. Its second argument is a list of lists of floating point
%        values.

random_coordinate(size(Dimension, Sizes), coordinate(Dimension, Args)):-
  maplist(random_betwixt, Sizes, Args).

%% seconds(?Hours:integer, ?Minutes:integer, ?Seconds:integer) is det.
% Converts hours and minutes into seconds and vice versa.
%
% @param Hours An integer
% @param Minutes An integer
% @param Seconds An integer

seconds(Hours, Minutes, Seconds):-
  nonvar(Seconds),
  !,
  Minutes is Seconds mod 60,
  Hours is Seconds / 60.
seconds(Hours, Minutes, Seconds):-
  default(Hours, 0, SetHours),
  default(Minutes, 0, SetMinutes),
  Seconds is (SetMinutes + (SetHours * 60)) * 60.

%% sign_multiply(+Sign1, +Sign2, -Sign3)
% Performs sign multiplication according to the following table:
%
% |   | + | 0 | - |
% | + | + | 0 | - |
% | 0 | 0 | 0 | 0 |
% | - | - | 0 | + |
%
% @param Sign1 The first argument for the sign multiplication.
% @param Sign2 The second argument for the sign multiplication.
% @param Sign3 The result of the multiplication of the given two signs.

sign_multiply(zero, _Sign, zero):-
  !.
sign_multiply(_Sign, zero, zero):-
  !.
sign_multiply(Sign, Sign, plus):-
  !.
sign_multiply(Sign1, Sign2, min):-
  Sign1 \== Sign2.

%% square(+X:float, -Square:float) is det.
% Returns the square of the given number.

square(X, Square):-
  Square is X ** 2.
