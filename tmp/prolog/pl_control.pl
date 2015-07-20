:- module(
  pl_control,
  [
    if_else/2, % :If
               % :Else
    if_then/2, % :If
               % :Then
    if_then_else/3, % :If
                    % :Then
                    % :Else
    switch/2, % +Value
              % +Maps:list(pair)
    switch/3, % +Value
              % +Maps:list(pair)
              % +Default
    unless/2, % :Unless
              % :Do
    xor/2 % :X
          % :Y
  ]
).

/** <module> Prolog control

Control structures for Prolog.

@author Wouter Beek
@version 2012/07-2012/08, 2013/01, 2013/03-2013/04, 2013/09-2013/10, 2013/12
*/

:- meta_predicate(if_else(0,0)).
:- meta_predicate(if_then(0,0)).
:- meta_predicate(if_then_else(0,0,0)).
:- meta_predicate(switch(+,:,+)).
:- meta_predicate(unless(0,0)).
:- meta_predicate(xor(0,0)).



%! if_else(:If, :Else) is det.
% Procedural control structure.

if_else(If, Else):-
  if_then_else(If, true, Else).


%! if_then(:If, :Then) is det.
% Procedural control structure.

if_then(If, Then):-
  if_then_else(If, Then, true).


%! if_then_else(:If, :Then, :Else) is det.
% Procedural control structure.

if_then_else(If, Then, Else):-
  (
    call(If)
  ->
    call(Then)
  ;
    call(Else)
  ).


%! switch(+Value, +Maps:list(pair)) is det.

switch(Value, Maps):-
  switch(Value, Maps, fail).

%! switch(+Value, +Maps:list(pair), +Default) is det.

switch(Value, Maps, _Default):-
  member(Value-Goal, Maps), !,
  % Make sure the variables in the goal are bound outside the switch call.
  call(Goal).
switch(_Value, _Maps, Default):-
  % Make sure the variables in the goal are bound outside the switch call.
  call(Default).


unless(Unless, Do):-
  (
    call(Unless)
  ->
    true
  ;
    call(Do)
  ).


xor(X, _):-
  call(X), !.
xor(_, Y):-
  call(Y), !.

