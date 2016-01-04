:- module(
  pl_typecheck,
  [
    add_typecheck/2 % +Clause:compound
                    % -TypecheckedClause:compound
  ]
).

/** <module> Prolog typecheck

Adds typechecking to Prolog predicates that are documented in plDoc.

## Type system semantics

If the type is missing, we use `any`.
`?` is treated as `-`.
For `:` we assume the type is `any`.

| *Type declaration* | *Start of call*       | *End of call* |
| ++X:Type           | ground AND comp(Type) |               |
|  +X:Type           | comp(Type)            |               |
|  -X:Type           | comp(Type)            | Type          |
| --X:Type           | var                   | Type          |
|  @X:Type           | Type                  | ==            |

---

@author Michael Hendrix
@author Wouter Beek
@version 2014/08, 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_modes)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(pldoc/doc_wiki)).

:- doc_collect(true).



%! add_typecheck(+Clause:compound, -TypecheckedClause:compound) is det.
% Builds a goal which asserts all types associated with the given head.

add_typecheck(Clause, TypecheckedClause):-
  findall(
    Mode,
    mode(Clause, Mode),
    Modes
  ),
  maplist(mode_term_to_arguments, Modes, ArgDs),
  build_typechecks(Clause, ArgDs, StartTypecheck, EndTypecheck),
  build_clause(Clause, StartTypecheck, EndTypecheck, TypecheckedClause).


%! build_clause(
%!   +Clause:compound,
%!   +StartTypecheck:compound,
%!   +EndTypecheck,
%!   -TypecheckedClause:compound
%! ) is det.

build_clause(
  (Head:- Body),
  StartTypecheck,
  EndTypecheck,
  (Head:- StartTypecheck, Body, EndTypecheck)
):- !.
build_clause(
  (Head --> Body),
  StartTypecheck,
  EndTypecheck,
  (Head --> {StartTypecheck}, Body, {EndTypecheck})
).


%! build_typechecks(
%!   +Clause:compound,
%!   +ArgumentDescriptions:list(compound),
%!   -StartTypecheck:compound,
%!   -EndTypecheck:compound
%! ) is det.

build_typechecks(Clause, ArgDs, StartTypecheck, EndTypecheck):-
  clause_arguments(Clause, Args),
  findall(
    StartType,
    (
      nth0(I, Args, Arg),
      nth0(I, ArgDs, ArgD),
      start_type(Args, ArgD, StartType)
    ),
    StartTypes
  ),
  findall(
    EndType,
    (
      nth0(I, Args, Arg),
      nth0(I, ArgDs, ArgD),
      end_type(Arg, ArgD, EndType)
    ),
    EndTypes
  ),
  maplist(
    xfy_list(','),
    [StartTypecheck,EndTypecheck],
    [StartTypes,EndTypes]
  ).


%! end_type(
%!   +Variable:var,
%!   +ArgumentDescription:compound,
%!   -TypeAssertion:compound
%! ) is det.

end_type(Var, arg('-',_,Type), must_be(Type,Var)).
end_type(Var, arg('--',_,Type), must_be(Type,Var)).
end_type(Var, arg('--',_,Type), must_be(Type,Var)).


%! mode_term_to_arguments(
%!   +ModeTerm:compound,
%!   -Arguments:list(compound)
%! ) is det.

mode_term_to_arguments(is(Pred,_), Args):-
  % Support for DCGs.
  (
    Pred = //(Pred0)
  ->
    true
  ;
    Pred0 = Pred
  ),
  Pred0 =.. [_|Args0],
  maplist(normalize_argument, Args0, Args).


%! normalize_argument(
%!   +ArgumentSpec:compound,
%!   -ArgumentDescription:compound
%! ) is det.

normalize_argument(ArgumentSpec0, arg(Instantiation,Variable,Type)):-
  % For easy processing, make sure that mere variables
  % are represented by a specification % as wel: `?(Variable:any)`.
  (
    var(ArgumentSpec0)
  ->
    ArgumentSpec = ?(ArgumentSpec0:any)
  ;
    ArgumentSpec=ArgumentSpec0
  ),

  % Make sure that every argument has an instantiation.
  % Also supports the non-standards instantiation `++`,
  % distinguishing `ground` from `nonvar`.
  % Default instantiation: `?`.
  (
    ArgumentSpec =.. [Instantiation,ArgumentPair],
    member(Instantiation, [++,+,-,--,?,:,@,!])
  ->
    true
  ;
    Instantiation = '?',
    ArgumentPair = ArgumentSpec
  ),

  % Make sure that every argument has a variable and a type.
  % Default type: `any`.
  (
    nonvar(ArgumentPair),
    ArgumentPair = _:Type
  ->
    true
  ;
    Variable = ArgumentPair,
    Type = any
  ).


%! start_type(
%!   +Variable:var,
%!   +ArgumentDescription:compound,
%!   -TypeAssertion:compound
%! ) is det.

start_type(Var, arg('++',_,_), must_be(ground,Var)).
start_type(Var, arg('++',_,Type), comp(Type,Var)).
start_type(Var, arg('+',_,Type), comp(Type,Var)).
start_type(Var, arg('-',_,Type), comp(Type,Var)).
start_type(Var, arg('--',_,_), var(Var)).
start_type(Var, arg('@',_,Type), must_be(Type,Var)).



% Helpers

%! clause_arguments(+Clause:compound, -Arguments:list) is det.

clause_arguments((Head:- _), Args):- !,
  Head =.. [_|Args].
clause_arguments((Head --> _), Args):-
  Head =.. [_|Args].


%! clause_functor(+Clause:compound, -Functor:compound) is det.

clause_functor((Head:- _), Module:'/'(Name,Arity)):- !,
  prolog_load_context(module, Module),
  functor(Head, Name, Arity).
clause_functor((Head --> _), Module:'//'(Name,Arity)):- !,
  prolog_load_context(module, Module),
  functor(Head, Name, Arity).


%! clause_to_mode_terms(+Clause:compound, -ModeTerms:list(compound)) is det.
% Returns the structured comment of the given clause.

clause_to_mode_terms(Clause, ModeTerms):-
  % Fetch the codes list that documents the given clause.
  clause_functor(Clause, Functor),
  doc_comment(Functor, _, _, String),
  string_codes(String, Codes),

  % Determine whether mode annotations in comments start with
  % `%%`, `%!`, or `/**`.
  phrase(pldoc_process:structured_comment(Prefixes, _), Codes, _),

  % Extract the mode lines, using the identified prefixes.
  indented_lines(Codes, Prefixes, Lines),

  pldoc_modes:mode_lines(Lines, Comment, [], _),
  setup_call_cleanup(
    open_codes_stream(Comment, Read),
    read_mode_terms(Read, ModeTerms),
    close(Read)
  ).


%! read_mode_terms(+Read:stream, -ModeTerms:list(component)) is det.
% Reads Prolog terms describing modes from the given stream.

read_mode_terms(Read, [ModeTerm|ModeTerms]):-
  read_term(Read, ModeTerm, [module(pldoc_modes),variable_names(Variables)]),
  ModeTerm \== end_of_file,
  maplist(call, Variables), !,
  read_mode_terms(Read, ModeTerms).
read_mode_terms(_, []).


%! xfy_list(?Op:atom, ?Term, ?List) is det.
% True if elements of List joined together with xfy operator Op gives
% Term. Usable in all directions.
%
% ### Example
%
% ```prolog
% ?- xfy_list(',', (a,b,c), L).
% L = [a, b, c].
%
% ?- xfy_list(Op, 4^3^2, [4,3,2]).
% Op = (^).
% ```

xfy_list(Op, Term, [Left|List]):-
  Term =.. [Op,Left,Right],
  xfy_list(Op, Right, List), !.
xfy_list(_, Term, [Term]).

