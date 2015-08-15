:- module(
  dcg_arrow,
  [
    arrow//2, % ?Head:oneof([both,left,right])
              % ?Length:nonneg
    horizontal_line//0,
    horizontal_line//1, % ?Length:nonneg
    transition//2 % :From
                  % :To
  ]
).

/** <module> Writing ASCII arrow

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_call)).

:- meta_predicate(transition(//,//,?,?)).





%! arrow(?Head:oneof([both,left,right]), ?Length:nonneg)// .
% A simple ASCII arrow with a left head, a right head,
% or both left and right heads.
%
% ### Ambiguity
%
% The notation for ASCII arrows is ambiguous, for example:
%
% ```prolog
% ?- phrase((arrow(Head1, Length1), arrow(Head2, Length2)), `<--->`).
% Head1 = left,
% Length1 = 4,
% Head2 = right,
% Length2 = 1 ;
% Head1 = left,
% Length1 = 3,
% Head2 = right,
% Length2 = 2 ;
% Head1 = left,
% Length1 = 2,
% Head2 = right,
% Length2 = 3 ;
% Head1 = left,
% Length1 = 1,
% Head2 = right,
% Length2 = 4 ;
% ```

arrow(Head, Length) -->
  % With instantiated arrow head the rest of the code is easier.
  {arrow_head(Head)},
  arrow_left_head(Head),
  arrow_horizontal_line(Head, Length),
  arrow_right_head(Head).



%! horizontal_line// .

horizontal_line -->
  {tty_size(_, ScreenWidth)},
  dcg_once(horizontal_line(ScreenWidth)).

%! horizontal_line(?Length:nonneg)// .
% @throws domain_error if length is not an integer.
% @throws type_error if length is a negative integer.

horizontal_line(Length) -->
  '#'(Length, hyphen, []).



%! transition(:From, :To)// is det.

transition(From, To) -->
  dcg_call_cp(From),
  dcg_between(space, arrow(right, 2)),
  dcg_call_cp(To).





% HELPERS %

%! arrow_head(+Head:oneof([both,left,right])) is semidet.
%! arrow_head(-Head:oneof([both,left,right])) is multi.

arrow_head(both).
arrow_head(left).
arrow_head(right).


%! arrow_head_dir(
%!   ?Head:oneof([both,left,right]),
%!   ?Direction:oneof([left,right])
%! ) .
% Relates the arrow heads to the directions in which they point.

arrow_head(both,  left ).
arrow_head(both,  right).
arrow_head(left,  left ).
arrow_head(right, right).


% ! arrow_horizontal_line(+Head:oneof([both,left,right]), ?Length:nonneg)// .

arrow_horizontal_line(Head, L1) -->
  {nonvar(L1)}, !,
  % Length correction.
  {
    arrow_length_correction_left(Head, L1, L2),
    arrow_length_correction_right(Head, L2, L3)
  },
  horizontal_line(L3).
arrow_horizontal_line(Head, L1) -->
  horizontal_line(L3),
  {
    arrow_length_correction_right(Head, L2, L3),
    arrow_length_correction_left(Head, L1, L2)
  }.


%! arrow_left_head(+Head:oneof([both,left,right]))// is nondet.

arrow_left_head(right) --> !, "".
arrow_left_head(_) --> "<".


arrow_length_correction_left(Head, L1, L2):-
  arrow_head(Head, left), !,
  succ(L2, L1).
arrow_length_correction_left(_, L, L).


arrow_length_correction_right(Head, L1, L2):-
  arrow_head(Head, right), !,
  succ(L2, L1).
arrow_length_correction_right(_, L, L).


%! arrow_right_head(+Head:oneof([both,left,right]))// is nondet.

arrow_right_head(left) --> !, "".
arrow_right_head(_) --> ">".
