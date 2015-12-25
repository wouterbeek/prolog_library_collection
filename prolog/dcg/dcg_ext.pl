:- module(
  dcg_ext,
  [
    '?'//1, % :Dcg_1
    '?'//2, % :Dcg_1
            % -Content:list
    '*'//1, % :Dcg_1
    '*'//2, % :Dcg_1
            % -Content:list
    '+'//1, % :Dcg_1
    '+'//2, % :Dcg_1
            % -Content:list
    '#'//2, % +Occurrences, :Dcg_1
    '#'//3, % +Occurrences:nonneg
            % :Dcg_1
            % -Content:list
    '*n'//2, % ?High:nonneg
             % :Dcg_1
    '*n'//3, % ?High:nonneg
             % :Dcg_1
             % -Content:list
    'm*'//3, % ?Low:nonneg
             % :Dcg_1
             % -Content:list
    'm*n'//3, % ?Low, ?High, :Dcg_1
    'm*n'//4, % ?Low:nonneg
              % ?High:nonneg
              % :Dcg_1
              % -Content:list
    bit//1, % ?Integer:between(0,1)
    def//3, % :Dcg_1
            % -Argument
            % +Default
    digit_code//1, % ?Code:code
    frac_pos/2, % +Fractional:between(0.0,1.0)
                % -Digits:list(between(0,9))
    generate_as_digits//2, % +Number, +NumberOfDigits
    generate_as_digits//3, % +Number:nonneg
                           % +Base:nonneg
                           % +NumberOfDigits:nonneg
    opt//2, % :Dcg_1
            % ?Argument
    pos/2, % +Integer, -Digits
    pos/3, % +Integer:nonneg
           % +Base:nonneg
           % -Digits:list(between(0,9))
    pos_frac/2, % +Digits:list(between(0,9))
                % -FractionalPart:rational
    pos_sum/2, % +Digits, -Number
    pos_sum/3, % +Digits:list(nonneg)
               % +Base:positive_integer
               % -Number:nonneg
    sum_pos/2, % +Number, -Digits
    sum_pos/3 % +Number:nonneg
              % +Base:nonneg
              % -Digits:list(between(0,9))
  ]
).
:- reexport(library(dcg/basics), except([digit//1,digits//1])).
:- reexport(library(url/rfc1738), [
     alpha//1, % ?Code:code
     alphadigit//1, % ?Code:code
     digit//1, % ?Weight
     digit//2, % ?Weight:between(0,9)
               % ?Code:code
     escape//1 as percent_escape, % ?Code:code
     hex//1, % ?Weigth
     hex//2, % ?Weigth:between(0,15)
             % ?Code:code
     hialpha//1, % ?Code:code
     lowalpha//1 % ?Code:code
   ]).

/** <module> DCG Extensions

My favorite collection of DCG rules.

@author Wouter Beek
@version 2015/11-2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(lists)).
:- use_module(library(math/math_ext)).

:- meta_predicate(?(//,?,?)).
:- meta_predicate(?(3,-,?,?)).
:- meta_predicate(*(//,?,?)).
:- meta_predicate(*(3,-,?,?)).
:- meta_predicate(+(//,?,?)).
:- meta_predicate(+(3,-,?,?)).
:- meta_predicate(#(+,//,?,?)).
:- meta_predicate(#(+,3,-,?,?)).
:- meta_predicate('*n'(?,//,?,?)).
:- meta_predicate('*n'(?,3,-,?,?)).
:- meta_predicate('m*'(?,//,?,?)).
:- meta_predicate('m*n'(?,?,//,?,?)).
:- meta_predicate('m*n'(?,?,3,-,?,?)).
:- meta_predicate('m*n__'(?,?,+,//,?,?)).
:- meta_predicate('m*n__'(?,?,+,3,-,?,?)).
:- meta_predicate(def(3,-,+,?,?)).
:- meta_predicate(opt(3,?,?,?)).





?(Dcg_1) --> 'm*n'(0, 1, Dcg_1).

?(Dcg_1, L) --> 'm*n'(0, 1, Dcg_1, L).



*(Dcg_1) --> 'm*n'(0, _, Dcg_1).

*(Dcg_1, L) --> 'm*n'(0, _, Dcg_1, L).



#(N, Dcg_1) --> 'm*n'(N, N, Dcg_1).

#(N, Dcg_1, L) --> 'm*n'(N, N, Dcg_1, L).



+(Dcg_1) --> 'm*n'(1, _, Dcg_1).

+(Dcg_1, L) --> 'm*n'(1, _, Dcg_1, L).



%! '*n'(?High:nonneg, :Dcg_1, -Content:list)// .

'*n'(High, Dcg_1) --> 'm*n'(_, High, Dcg_1).


%! '*n'(?High:nonneg, :Dcg_1, -Content:list)// .

'*n'(High, Dcg_1, L) --> 'm*n'(_, High, Dcg_1, L).



%! 'm*'(?Low:nonneg, :Dcg_1, -Content:list)// .

'm*'(Low, Dcg_1, L) --> 'm*n'(Low, _, Dcg_1, L).


%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_0)// .

'm*n'(Low, High, Dcg_0) --> 'm*n__'(Low, High, 0, Dcg_0).

'm*n__'(Low, High, Count1, Dcg_0) -->
  {(var(High) -> true ; Count1 < High)},
  Dcg_0, !,
  {Count2 is Count1 + 1},
  'm*n__'(Low, High, Count2, Dcg_0).
'm*n__'(Low, _, Count, _) --> {(var(Low) -> true ; Low =< Count)}.


%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_1, -Content:list)// .

'm*n'(Low, High, Dcg_1, L) --> 'm*n__'(Low, High, 0, Dcg_1, L).

'm*n__'(Low, High, Count1, Dcg_1, [H|T]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_1, H), !,
  {Count2 is Count1 + 1},
  'm*n__'(Low, High, Count2, Dcg_1, T).
'm*n__'(Low, _, Count, _, []) --> {(var(Low) -> true ; Low =< Count)}.



%! bit(?Integer:between(0,1))// .
% Wrapper around bit//2.

bit(I) --> bit(I, _).


%! bit(?Integer:between(0,1), ?Code:code)// .
% Binary digit.
%
% ```abnf
% BIT = "0" / "1"
% ```

bit(0, 0'0) --> "0".
bit(1, 0'1) --> "1".



%! def(:Dcg_1, -Argument, +Default)// .

def(Dcg_1, Arg, _) --> dcg_call(Dcg_1, Arg), !.
def(_, Def, Def) --> "".



%! digit_code(?Code:code)// .
% Wrapper around digit//2.

digit_code(C) --> digit(C, _).



%! frac_pos(+Fractional:between(0.0,1.0), -Digits:list(between(0,9))) is det.

frac_pos(Frac, Ds):-
  fractional_integer(Frac, I),
  sum_pos(I, Ds).



%! generate_as_digits(+Number:nonneg, +NumberOfDigits:nonneg)// is det.

generate_as_digits(N, M) -->
  generate_as_digits(N, 10, M).


%! generate_as_digits(
%!   +Number:nonneg,
%!   +Base:nonneg,
%!   +NumberOfDigits:nonneg
%! )// is det.

generate_as_digits(_, _, 0) --> !, "".
generate_as_digits(N1, Base, M1) -->
  {M2 is M1 - 1},
  {D is N1 // Base ^ M2},
  digit(D),
  {N2 is N1 mod Base ^ M2},
  generate_as_digits(N2, Base, M2).



%! opt(:Dcg_1, ?Argument)// .

opt(Dcg_1, Arg) --> dcg_call(Dcg_1, Arg), !.
opt(_, _) --> "".



%! pos(+Integer:nonneg, -Digits:list(between(0,9))) is det.
% Wrapper around pois/2 with decimal base.

pos(I, Ds):- pos(I, 10, Ds).


%! pos(+Integer:nonneg, +Base:nonneg, -Digits:list(between(0,9))) is det.

pos(I, Base, Ds):-
  pos_rev(I, Base, Ds0),
  reverse(Ds0, Ds).

pos_rev(I, Base, [I]):-
  I < Base, !.
pos_rev(I1, Base, [H|T]):-
  H is I1 mod Base,
  I2 is I1 // Base,
  pos_rev(I2, Base, T).



%! pos_frac(+Digits:list(between(0,9)), -FractionalPart:rational) is det.
% Positional fractional.

pos_frac(Ds, Frac):-
  aggregate_all(sum(Rat), (nth1(I, Ds, D), Rat is D rdiv (10 ^ I)), Frac).



%! pos_sum(+Digits:list(between(0,9)), -Integer:nonneg) is det.
% Positional summation.

pos_sum(Ds, I):- pos_sum(Ds, 10, I).


%! pos_sum(
%!   +Digits:list(between(0,9)),
%!   +Base:positive_integer,
%!   -Integer:nonneg
%! ) is det.

pos_sum(Ds, Base, I):- pos_sum(Ds, Base, 0, I).
pos_sum([D|Ds], Base, I1, I):- !,
  I2 is I1 * Base + D,
  pos_sum(Ds, Base, I2, I).
pos_sum([], _, I, I).



%! sum_pos(+Number:nonneg, -Digits:list(between(0,9))) is det.
% Wrapper around sum_pos/3 with decimal base.

sum_pos(I, Ds):-
  sum_pos(I, 10, Ds).


%! sum_pos(+Number:nonneg, +Base:nonneg, -Digits:list(between(0,9))) is det.

sum_pos(I, Base, Ds):-
  sum_pos0(I, Base, Ds0),
  reverse(Ds0, Ds).

sum_pos0(0, _, []):- !.
sum_pos0(I1, Base, [H|T]):-
  H is I1 mod Base,
  I2 is I1 // Base,
  sum_pos0(I2, Base, T).
