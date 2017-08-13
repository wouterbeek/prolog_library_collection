:- module(
  default,
  [
    defgoal/2,    % :DefGoal_1,  ?Val
    defsetting/2, % +DefSetting, ?Val
  ]
).

/** <module> Default

@author Wouter Beek
@version 2015/07, 2015/11, 2016/05, 2016/08
*/

:- use_module(library(settings)).

:- meta_predicate
    defgoal(1, ?).





%! defgoal(:DefGoal_1, ?Val) is det.
%
% Runs the given goal, whenever the given value is uninstantiated.
% The given goal is assumed to be unary and deterministic, always
% returning an instantiation for `Value`.
%
% # Example
%
% The following code allows a specific start node to be given for
% traveral, but also allows the start node to be uninstantiated,
% instantiating it to a randomly chosen start node.
%
% ```prolog
% graph_traversal(StartNode) :-
%   default_goal(random_start_node, StartNode),
%   ...
% ```

defgoal(_, Val) :-
  ground(Val), !.
defgoal(DefGoal_1, Val) :-
  once(call(DefGoal_1, Val)).



%! defsetting(+SettingName, ?Val) is det.

defsetting(_, Val) :-
  ground(Val), !.
defsetting(Name, Val) :-
  setting(Name, Val).
