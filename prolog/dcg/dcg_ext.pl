:- module(
  dcg_ext,
  [
    '?'//1,	% :Dcg_0
    '?'//2,	% :Dcg_1, -Arguments1:list
    '?'//3,	% :Dcg_2, -Arguments1:list, -Arguments2:list
    '*'//1,	% :Dcg_0
    '*'//2,	% :Dcg_1, -Arguments1:list
    '*'//3,	% :Dcg_2, -Arguments1:list, -Arguments2:list
    '+'//1,	% :Dcg_0
    '+'//2,	% :Dcg_1, -Arguments1:list
    '+'//3,	% :Dcg_2, -Arguments1:list, -Arguments2:list
    '#'//2,	% ?Occurrences:nonneg, :Dcg_0
    '#'//3,	% ?Occurrences:nonneg, :Dcg_1, -Arguments1:list
    '#'//4,	% ?Occurrences:nonneg, :Dcg_2, -Arguments1:list, -Arguments2:list
    '*n'//2,	% ?High:nonneg, :Dcg_0
    '*n'//3,	% ?High:nonneg, :Dcg_1, -Arguments1:list
    '*n'//4,	% ?High:nonneg, :Dcg_2, -Arguments1:list, -Arguments2:list
    'm*'//2,	% ?Low:nonneg, :Dcg_0
    'm*'//3,	% ?Low:nonneg, :Dcg_1, -Arguments1:list
    'm*'//4,	% ?Low:nonneg, :Dcg_2, -Arguments1:list, -Arguments2:list
    'm*n'//3,	% ?Low:nonneg, ?High:nonneg, :Dcg_0
    'm*n'//4,	% ?Low:nonneg, ?High:nonneg, :Dcg_1, -Arguments1:list
    'm*n'//5,	% ?Low:nonneg, ?High:nonneg, :Dcg_2, -Arguments1:list, -Arguments2:list
    bit//1,		% ?Integer:between(0,1)
    def//3,		% :Dcg_1, -Argument, +Default
    digit_code//1,	% ?Code:code
    frac_pos/2,		% +Fractional:between(0.0,1.0), -Digits:list(between(0,9))
    generate_as_digits//2, % +Number:nonneg, +NumberOfDigits:nonneg
    generate_as_digits//3, % +Number:nonneg, +Base:nonneg, +NumberOfDigits:nonneg
    opt//2,		% :Dcg_0
    opt//2,		% :Dcg_1, ?Argument
    pos/2,		% +Number:nonneg, -Digits:list(between(0,9))
    pos/3,		% +Number:nonneg, +Base:nonneg, -Digits:list(between(0,9))
    pos_frac/2,		% +Digits:list(between(0,9)), -FractionalPart:rational
    pos_sum/2,		% +Digits:list(between(0,9)), -Number:nonneg
    pos_sum/3,		% +Digits:list(nonneg), +Base:positive_integer, -Number:nonneg
    sum_pos/2,		% +Number:nonneg, -Digits:list(between(0,9))
    sum_pos/3		% +Number:nonneg, +Base:nonneg, -Digits:list(nonneg)
  ]
).
:- reexport(library(dcg/basics), except([digit//1,digits//1])).
:- reexport(library(url/rfc1738), [
     alpha//1,		% ?Code:code
     alphadigit//1,	% ?Code:code
     digit//1,		% ?Weight:between(0,9)
     digit//2,		% ?Weight:between(0,9), ?Code:code
     escape//1 as percent_escape, % ?Code:code
     hex//1,		% ?Weigth:between(0,15)
     hex//2,		% ?Weigth:between(0,15), ?Code:code
     hialpha//1,	% ?Code:code
     lowalpha//1	% ?Code:code
   ]).

/** <module> DCG Extensions

My favorite collection of DCG rules.

@author Wouter Beek
@version 2015/11-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(lists)).
:- use_module(library(math/math_ext)).

:- meta_predicate(?(//,?,?)).
:- meta_predicate(?(3,-,?,?)).
:- meta_predicate(?(4,-,-,?,?)).
:- meta_predicate(*(//,?,?)).
:- meta_predicate(*(3,-,?,?)).
:- meta_predicate(*(4,-,-,?,?)).
:- meta_predicate(+(//,?,?)).
:- meta_predicate(+(3,-,?,?)).
:- meta_predicate(+(4,-,-,?,?)).
:- meta_predicate(#(+,//,?,?)).
:- meta_predicate(#(+,3,-,?,?)).
:- meta_predicate(#(+,4,-,-,?,?)).
:- meta_predicate('*n'(?,//,?,?)).
:- meta_predicate('*n'(?,3,-,?,?)).
:- meta_predicate('*n'(?,4,-,-,?,?)).
:- meta_predicate('m*'(?,//,?,?)).
:- meta_predicate('m*'(?,3,-,?,?)).
:- meta_predicate('m*'(?,4,-,-,?,?)).
:- meta_predicate('m*n'(?,?,//,?,?)).
:- meta_predicate('m*n'(?,?,3,-,?,?)).
:- meta_predicate('m*n'(?,?,4,-,-,?,?)).
:- meta_predicate('m*n__'(?,?,+,//,?,?)).
:- meta_predicate('m*n__'(?,?,+,3,-,?,?)).
:- meta_predicate('m*n__'(?,?,+,4,-,-,?,?)).
:- meta_predicate(def(3,-,+,?,?)).
:- meta_predicate(opt(//,?,?)).
:- meta_predicate(opt(3,?,?,?)).





%! ?(:Dcg_0)// is det.
%! ?(:Dcg_1, -Arguments1:list)// is det.
%! ?(:Dcg_2, -Arguments1:list, -Arguments2:list)// is det.

?(Dcg_0) --> 'm*n'(0, 1, Dcg_0).
?(Dcg_1, L1) --> 'm*n'(0, 1, Dcg_1, L1).
?(Dcg_2, L1, L2) --> 'm*n'(0, 1, Dcg_2, L1, L2).



%! *(:Dcg_0)// is det.
%! *(:Dcg_1, -Arguments1:list)// is det.
%! *(:Dcg_2, -Arguments1:list, -Arguments2:list)// is det.

*(Dcg_0) --> 'm*n'(0, _, Dcg_0).
*(Dcg_1, L1) --> 'm*n'(0, _, Dcg_1, L1).
*(Dcg_2, L1, L2) --> 'm*n'(0, _, Dcg_2, L1, L2).



%! #(?Occurrences:nonneg, :Dcg_0)// is det.
%! #(?Occurrences:nonneg, :Dcg_1, -Arguments1:list)// is det.
%! #(?Occurrences:nonneg, :Dcg_2, -Arguments1:list, -Arguments2:list)// is det.

#(N, Dcg_0) --> 'm*n'(N, N, Dcg_0).
#(N, Dcg_1, L1) --> 'm*n'(N, N, Dcg_1, L1).
#(N, Dcg_2, L1, L2) --> 'm*n'(N, N, Dcg_2, L1, L2).



%! '*n'(?High:nonneg, :Dcg_0)// is det.
%! '*n'(?High:nonneg, :Dcg_1, -Arguments1:list)// is det.
%! '*n'(?High:nonneg, :Dcg_2, -Arguments1:list, -Arguments2:list)// is det.

+(Dcg_0) --> 'm*n'(1, _, Dcg_0).
+(Dcg_1, L1) --> 'm*n'(1, _, Dcg_1, L1).
+(Dcg_2, L1, L2) --> 'm*n'(1, _, Dcg_2, L1, L2).



%! '*n'(?High:nonneg, :Dcg_0)// is det.
%! '*n'(?High:nonneg, :Dcg_1, -Arguments1:list)// is det.
%! '*n'(?High:nonneg, :Dcg_2, -Arguments1:list, -Arguments2:list)// is det.

'*n'(High, Dcg_0) --> 'm*n'(_, High, Dcg_0).
'*n'(High, Dcg_1, L1) --> 'm*n'(_, High, Dcg_1, L1).
'*n'(High, Dcg_2, L1, L2) --> 'm*n'(_, High, Dcg_2, L1, L2).



%! 'm*'(?Low:nonneg, :Dcg_0)// is det.
%! 'm*'(?Low:nonneg, :Dcg_1, -Arguments1:list)// is det.
%! 'm*'(?Low:nonneg, :Dcg_2, -Arguments1:list, -Arguments2:list)// is det.

'm*'(Low, Dcg_0) --> 'm*n'(Low, _, Dcg_0).
'm*'(Low, Dcg_1, L1) --> 'm*n'(Low, _, Dcg_1, L1).
'm*'(Low, Dcg_2, L1, L2) --> 'm*n'(Low, _, Dcg_2, L1, L2).



%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_0)// is det.
%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_1, -Arguments1:list)// is det.
%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_2, -Arguments1:list, -Arguments2:list)// is det.

'm*n'(Low, High, Dcg_0) --> 'm*n__'(Low, High, 0, Dcg_0).
'm*n__'(Low, High, Count1, Dcg_0) -->
  {(var(High) -> true ; Count1 < High)},
  Dcg_0, !,
  {Count2 is Count1 + 1},
  'm*n__'(Low, High, Count2, Dcg_0).
'm*n__'(Low, _, Count, _) --> {(var(Low) -> true ; Low =< Count)}.

'm*n'(Low, High, Dcg_1, L1) --> 'm*n__'(Low, High, 0, Dcg_1, L1).
'm*n__'(Low, High, Count1, Dcg_1, [H1|T1]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_1, H1), !,
  {Count2 is Count1 + 1},
  'm*n__'(Low, High, Count2, Dcg_1, T1).
'm*n__'(Low, _, Count, _, []) --> {(var(Low) -> true ; Low =< Count)}.

'm*n'(Low, High, Dcg_2, L1, L2) --> 'm*n__'(Low, High, 0, Dcg_2, L1, L2).
'm*n__'(Low, High, Count1, Dcg_2, [H1|T1], [H2|T2]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_2, H1, H2), !,
  {Count2 is Count1 + 1},
  'm*n__'(Low, High, Count2, Dcg_2, T1, T2).
'm*n__'(Low, _, Count, _, [], []) --> {(var(Low) -> true ; Low =< Count)}.



%! bit(?Integer:between(0,1))// .
% Wrapper around bit//2.
%! bit(?Integer:between(0,1), ?Code:code)// .
% Binary digit.
%
% ```abnf
% BIT = "0" / "1"
% ```

bit(I) --> bit(I, _).
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



%! opt(:Dcg_0)// .

opt(Dcg_0) --> Dcg_0, !.
opt(_) --> "".


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
