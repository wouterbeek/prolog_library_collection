:- module(
  meta_ext,
  [
% CACHING
    reset_memo/0,
    memo/1, % :Goal

% CONTROL STRUCTURES
    if_else/2, % :If
               % :Else
    if_then/2, % :If
               % :Then
    if_then_else/3, % :If
                    % :Then
                    % :Else

% DETERMINISM
    call_semidet/1, % :Goal
    nonvar_det/1, % :Goal

% FINDALL RELATED PREDICATES
    setoff/3, % +Format:compound
              % :Goal
              % -Set:ord_set

% MAPLIST RELATED PREDICATES
    app_list/3, % +Preds:list
                % +Args:list
                % -Results:list
    maplist_pairs/3, % :Goal
                     % +List1:list
                     % -List2:list
    mapset/3, % :Goal
              % +List:list
              % -Set:ord_set
    mapsum/3, % :Goal
              % +List:list
              % -Number:number

% MODULES
    modules/1, % -Modules:list(atom)

% MULTI-THREADING
    run_on_sublists/2, % +List:list
                       % :Goal

% MULTIPLE CALLS
    call_nth/2, % :Goal
                % +C:integer
    complete/3, % :Goal
                % +Input
                % -History:list
    count/2, % :Goal
             % -Number:number
    multi/2, % :Goal
             % +Count:integer
    multi/4, % :Goal
             % +Count:integer
             % +Input:term
             % -Output:term
    multi/5, % :Goal
             % +Count:integer
             % +Input:term
             % -Output:term
             % -History:list(term)

% OTHERS
    run_in_working_directory/2, % :Call
                                % +WorkingDirectory:atom

% SORTING
    predsort_with_duplicates/3, % :Goal:atom
                                % +List:list
                                % -SortedList:list

% USER INTERACTION
    user_interaction/4 % +Action:atom
                       % :Goal
                       % +Headers:list(atom)
                       % +Tuples:list(list)
  ]
).

/** <module> Meta extensions

Extensions to the SWI-Prolog meta predicates.

@author Wouter Beek
@version 2012/07-2012/08, 2013/01, 2013/03
*/

:- use_module(pgc(list_ext)).
:- use_module(pgc(thread_ext)).

:- meta_predicate(call_semidet(0)).
:- meta_predicate(nonvar_det(0)).
:- meta_predicate(call_nth(0,-)).
:- meta_predicate(complete(2,+,-)).
:- meta_predicate(count(0,-)).
:- meta_predicate(if_else(0,0)).
:- meta_predicate(if_then(0,0)).
:- meta_predicate(if_then_else(0,0,0)).
:- meta_predicate(maplist_pairs(3,+,-)).
:- meta_predicate(mapset(2,+,-)).
:- meta_predicate(mapsum(2,+,-)).
:- meta_predicate(memo(0)).
:- meta_predicate(multi(0,+)).
:- meta_predicate(multi(2,+,+,-)).
:- meta_predicate(multi(2,+,+,-,-)).
:- meta_predicate(predmerge_with_duplicates(2,+,+,-)).
:- meta_predicate(predmerge_with_duplicates(2,+,+,+,+,+,-)).
:- meta_predicate(predsort_with_duplicates(3,+,-)).
:- meta_predicate(predsort_with_duplicates(3,+,-,-,-)).
:- meta_predicate(run_in_working_directory(0,+)).
:- meta_predicate(run_on_sublists(+,1)).
:- meta_predicate(setoff(+,0,-)).
:- meta_predicate(setoff_alt(+,0,-)).

:- dynamic(memo_/1).
:- dynamic(tmp/1).



% CACHING %

%% memo(:Goal) is nondet.
% Memo goals that take relatively long to compute and that
% are likely to be recomputed in the future.
% This is achieved by storing the result along with the call,
% i.e. the fully instantiated goal.
% There are no restrictions on the determinism of the goal.

memo(Goal):-
  memo_(Goal),
  !.
memo(Goal):-
  call(Goal),
  assertz(memo_(Goal)).

reset_memo:-
  retractall(memo_(_)).



% CONTROL STRUCTURES %

%% if_else(:If, :Else) is det.
% Procedural control structure.

if_else(If, Else):-
  if_then_else(If, true, Else).

%% if_then(:If, :Then) is det.
% Procedural control structure.

if_then(If, Then):-
  if_then_else(If, Then, true).

%% if_then_else(:If, :Then, :Else) is det.
% Procedural control structure.

if_then_else(If, Then, Else):-
  (
    call(If)
  ->
    call(Then)
  ;
    call(Else)
  ).



% DETERMINISM %

%% call_semidet(:Goal) is det.
% Executes the given semi-deterministic goal exactly once, i.e., regardless
% of the open choice point. If the goal is not semideterministic,
% an error is thrown.
%
% @author Ulrich Neumerkel
% @error error(mode_error(semidet, Goal),
%        context(call_semidet/1, 'Message left empty.'))

call_semidet(Goal):-
  (
    call_nth(Goal, 2)
  ->
    throw(
      error(
        mode_error(semidet, Goal),
        context(call_semidet/1, 'Message left empty.')
      )
    )
  ;
    once(Goal)
  ).

nonvar_det(Mod:Goal):-
  Goal =.. [P | Args],
  maplist(nonvar, Args),
  !,
  apply(Mod:P, Args),
  !.
nonvar_det(Goal):-
  call(Goal).



% FINDALL RELATED PREDICATES %

%% setoff(+Format, :Goal, -Set:ord_set) is det.
% The sorted version of forall/2.
%
% @param Format A compound term.
% @param Goal A predicate name.
% @param Set An ordered set.
% @see forall/2

setoff(Format, Goal, Set):-
  findall(Format, Goal, List),
  sort(List, Set).

% @tbd Run this with help_web/1!
setoff_alt(Format, Goal, _Set):-
  call(Goal),
  (tmp(Format) -> true ; assertz(tmp(Format))),
  fail.
setoff_alt(_Format, _Goal, Set):-
  findall(Format, tmp(Format), Set0),
  retractall(tmp(_)),
  sort(Set0, Set).



% MAPLIST RELATED PREDICATES %

%% app_list(+Preds:list, +Args:list, -Results:list) is det.
% Applies multiple predicates to a static list of arguments.
% Returns the results of applying the given predicates to the given argument
% list. The number of results is the number of predicates. The arguments are
% the same for every predicate call.

app_list([], _Args, []).
app_list([Module:Pred | Preds], Args, [Result | Results]):-
  append(Args, [Result], Args0),
  Call =.. [Pred | Args0],
  call(Module:Call),
  app_list(Preds, Args, Results).

%% maplist_pairs(:Goal, +List1:list, -List2:list) is det.
% Applies the given goal to all pairs of elements occuring in =List1=.

maplist_pairs(Goal, List1, List2):-
  findall(
    Result,
    (
      member(Element1, Element2, List1),
      call(Goal, Element1, Element2, Result)
    ),
    List2
  ).

%% mapset(:Goal, +List:list(term), -Set:ord_set(term)) is det.
% The sorted version of maplist/3.
%
% @param Goal A goal.
% @param List A list of terms.
% @param Set An ordered set of terms.

mapset(Goal, List, Set):-
  maplist(Goal, List, NewList),
  sort(NewList, Set).

mapsum(Goal, List, Sum):-
  maplist(Goal, List, Numbers),
  sum_list(Numbers, Sum).



% MODULES %

%% modules(-Modules:list(atom)) is det.
% Returns a list of the names of all the loaded modules.
%
% @param Modules A list of atomic module names.

modules(Modules):-
  findall(
    Module,
    current_module(Module),
    Modules
  ).



% MULTI-THREADING %

run_on_sublists(List, Module:Goal):-
  split_list_by_number_of_sublists(List, 12, Sublists),
  forall(
    member(TaskList, Sublists),
    thread_start(Module, Goal, TaskList, _ThreadId)
  ).



% MULTIPLE CALLS %

%% call_nth(:Goal, +C:integer) is semidet.
% Multiple calls of the same nondeterministic goal.
% Calls the given goal the given number of times.
% This does not exclude the case in which the goal could have been executed
% more than =C= times.
%
% @author Ulrich Neumerkel

call_nth(Goal, C):-
  State = count(0),
  Goal,
  arg(1, State, C1),
  C2 is C1+1,
  nb_setarg(1, State, C2),
  C = C2.

%% complete(:Goal, +Input, -Results:list) is det.
% Runs the given goal on the given input until it wears out.
% The goal is enforced to be deteministic or semi-deterministic (the extra
% choicepoint is automatically dropped).
%
% @tbd Check whether this can be unified with multi/[4,5].

complete(Goal, Input, [Input | History]):-
  Goal =.. [P | Args],
  append(Args, [Input, Intermediate], Args1),
  Goal1 =.. [P | Args1],
  call_semidet(Goal1),
  !,
  complete(Goal, Intermediate, History).
complete(_Goal, Input, [Input]).

%% count(:Goal, -Count:integer) is det.
% Returns the number of calls that can be made of the given goal.
%
% @param Goal A goal.
% @param Count An integer.

count(Goal, Count):-
  (
    Goal = _Module:Goal_,
    !
  ;
    Goal_ = Goal
  ),
  Goal_ =.. [_Predicate | Arguments],
  list_compound(Arguments, CompoundArgument),
  setoff(
    CompoundArgument,
    Goal,
    CompoundArguments
  ),
  length(CompoundArguments, Count).

%% list_compound(?List:list(term), ?Compound:compound_term) is nondet.
% Converts between lists and compound terms of the form =x_1/.../x_n=.
%
% @param List A list of terms.
% @param Compound A compound term of the form =x_1/.../x_n=.

list_compound([X], X):-
  !.
list_compound([H | T], H/CompoundT):-
  list_compound(T, CompoundT).

multi(_Goal, 0):-
  !.
multi(Goal, Count):-
  call(Goal),
  NewCount is Count - 1,
  multi(Goal, NewCount).

%% multi(:Goal, +Count:integer, +Input:term, -Output:term) is det.
% Applies a predicate multiple times on the given input and its
% subsequent outputs, i.e. repeated function application.
%
% @param Goal
% @param Count The integer counter, indicating the number of times the
%        predicate is applied repeaterly.
% @param Input A term.
% @param Output A term.

multi(Goal, Count, Input, Output):-
  multi(Goal, Count, Input, Output, _History).

multi(_Goal, 0, Output, Output, [Output]):-
  !.
multi(Goal, Count, Input, Output, [Intermediate | History]):-
  call(Goal, Input, Intermediate),
  NewCount is Count - 1,
  multi(Goal, NewCount, Intermediate, Output, History).



% OTHERS %

run_in_working_directory(Call, WorkingDirectory):-
  working_directory(OldWorkingDirectory, WorkingDirectory),
  call(Call),
  working_directory(WorkingDirectory, OldWorkingDirectory).



% SORTING %

%% predmerge_with_duplicates(+Predicate, +List1, +List2, -Solution)
% Merges the given lists based on the given sort predicate.
% @precondition It is assumed that both lists are themselves sorted.
% @param Predicate The sort predicate. It should be tertiary, of the form
% <{ <, =, > }, Element1, Element2>.
%
% @param List1 An ordered list.
% @param List2 An ordered list.
% @param Solution An ordered list.

predmerge_with_duplicates(_Predicate, [], MergeResult, MergeResult):-
  !.
predmerge_with_duplicates(_Predicate, MergeResult, [], MergeResult):-
  !.
predmerge_with_duplicates(Predicate, [H1 | T1], [H2 | T2], Result):-
  call(Predicate, Delta, H1, H2),
  predmerge_with_duplicates(Delta, Predicate, H1, H2, T1, T2, Result).

%% predmerge_with_duplicates(
%%   +Delta,
%%   +Predicate,
%%   +ElementHalf1,
%%   +ElementHalf2,
%%   +SortedListHalf1,
%%   +SortedListHalf2,
%%   -SortedList
%% ) is det.

% H1 > H2, so place H2 in front of the result, and run again with H1.
predmerge_with_duplicates(>, Predicate, H1, H2, T1, T2, [H2 | Result]):-
  predmerge_with_duplicates(Predicate, [H1 | T1], T2, Result).
% H1 = H2, so place both H1 and H2 in the result (the order does not matter).
predmerge_with_duplicates(=, Predicate, H1, H2, T1, T2, [H1, H2 | Result]):-
  predmerge_with_duplicates(Predicate, T1, T2, Result).
% H1 < H2, so place H1 in front of the result, and run again with H2.

predmerge_with_duplicates(<, Predicate, H1, H2, T1, T2, [H1 | Result]):-
  predmerge_with_duplicates(Predicate, T1, [H2 | T2], Result).

%% predsort_with_duplicates(
%%    +Predicate:atom,
%%    +UnsortedList:list,
%%    -SortedList:list
%% ) is det.
% Variation of the standard predicate predsort/3 that does keeps any
% duplicates (instead of removing them).
%
% @param Predicate An atomic predicate name of a tertiary predicate.
% @param UnsortedList ...
% @param SortedList ...
% @see Slight alteration of predsort/3.

predsort_with_duplicates(Predicate, UnsortedList, SortedList):-
  length(UnsortedList, Length),
  predsort_with_duplicates(
    Predicate,
    Length,
    UnsortedList,
    _,
    SortedList
  ).

%% predsort_with_duplicates(
%%   +Predicate:atom,
%%   +Length:integer,
%%   -SortedListHalf:list(term),
%%   -UnsortedListHalf:list(term),
%%   -SortedList:ord_set(term)
%% ) is det.
% The division between =SortedListHalf1= and =UnsortedListHalf2= is defined
% by =Length=, which is the approximate length of both lists.
% The =SortedListHalf= is sorted in this predicate. The
% =UnsortedListHalf= will be sorted in the next iteration.
%
% @param Predicate The atomic name of a binary semideterministic predicate.
% @param Length An integer.
% @param SortedListHalf A list of terms that are already sorted.
% @param UnsortedListHalf A list of terms that are not yet sorted.
% @param SortedList An ordered set of terms.

% There are 2 more unsorted terms.
predsort_with_duplicates(
  Predicate,
  2,
  [H1, H2 | TailUnsortedList],
  TailUnsortedList,
  SortedList
):-
  !,
  % We perform one last call to finalize the sorting.
  call(Predicate, Delta, H1, H2),
  sort_with_duplicates(Delta, H1, H2, SortedList).
% There is 1 more unsorted term.
predsort_with_duplicates(
  _Predicate,
  1,
  [H | UnsortedList],
  UnsortedList,
  [H]
):-
  !.
% There are no more unsorted terms.
predsort_with_duplicates(_Predicate, 0, UnsortedList, UnsortedList, []):-
  !.
% The recursive case.
predsort_with_duplicates(Predicate, Length, L1, L3, SortedList):-
  % Rounded division of the given length.
  HalfLength1 is Length // 2,
  plus(HalfLength1, HalfLength2, Length),
  predsort_with_duplicates(Predicate, HalfLength1, L1, L2, Result1),
  predsort_with_duplicates(Predicate, HalfLength2, L2, L3, Result2),

  % The two results are themselves ordered, but when put together they may
  % be not sorted anymore. This is what the merge does.
  predmerge_with_duplicates(Predicate, Result1, Result2, SortedList).

%% sort_with_duplicates(+Delta, +Element1, +Element2, -SortedList:list)
% Returns the sorted list of the two given elements according to Delta.

sort_with_duplicates(<, H1, H2, [H1, H2]).
sort_with_duplicates(=, H1, H2, [H1, H2]).
sort_with_duplicates(>, H1, H2, [H2, H1]).



% USER INTERACTION %

%% user_interaction(
%%   +Action:atom,
%%   :Goal,
%%   +Headers:list(atom),
%%   +Tuples:list(term)
%% ) is det.
% The generic predicate for executing arbitray Prolog goals for arbitrary
% sequences of Prolog terms under user-interaction.
%
% One of the use cases is cleaning a database, where a list of =Tuples=
% has been identified for removal by =Goal=, but a user is required to
% assent to each removal action.
%
% @param Action An atomic description of the action that is performed by
%               the goal.
% @param Goal An arbitrary Prolog goal that takes the number of elements
%             in each tuple as the number of arguments.
% @param Headers A list of atoms describing the entries in each tuple.
%                The number of headers and the number of elements in each
%                tuple are assumed to be the same.
% @param Tuples A list of tuples. These are the element lists for which goal
%               is executed after user-confirmation.

user_interaction(Action, Goal, Headers, Tuples):-
  % Argument checking.
  atom(Action),
  % The number of headers is the number of elements in a tuple.
  length(Headers, TuplesLength),
  % All tuples have the same number of elements.
  forall(
    member(Tuple, Tuples),
    length(Tuple, TuplesLength)
  ),
  
  length(Tuples, NumberOfTuples),
  user_interaction(Action, Goal, 1, NumberOfTuples, Headers, Tuples).

user_interaction(_Action, _Goal, _Index, _Length, _Headers, []):-
  format(user, '\n-----\nDONE!\n-----\n', []),
  !.
user_interaction(Action, Goal, Index, Length, Headers, Tuples):-
  % Display a question.
  nth1(Index, Tuples, Tuple),
  findall(
    HeaderedElement,
    (
      nth0(J, Headers, Header),
      nth0(J, Tuple, Element),
      format(atom(HeaderedElement), '~w: ~w', [Header, Element])
    ),
    HeaderedElements
  ),
  atomic_list_concat(HeaderedElements, '\n\t', TupleAtom),
  format(
    user,
    '[~w/~w] ~w\n\t~w\n(y/n/q)\n?: ',
    [Index, Length, Action, TupleAtom]
  ),

  % Receive answer.
  get_single_char(UserCode),
  char_code(UserAtom, UserCode),

  % Act on answer.
  (
    UserAtom == q
  ->
    true
  ;
    UserAtom == y
  ->
    apply(Goal, Tuple),
    NewIndex is Index + 1,
    user_interaction(Action, Goal, NewIndex, Length, Headers, Tuples)
  ;
    UserAtom == n
  ->
    NewIndex is Index + 1,
    user_interaction(Action, Goal, NewIndex, Length, Headers, Tuples)
  ;
    user_interaction(Action, Goal, Index, Length, Headers, Tuples)
  ).
