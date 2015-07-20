:- module(
  dcg_proof,
  [
    proof//2 % +Proof:compound
             % +Options:list(nvpair)
  ]
).

/** <module> DCG: Proof

Grammar for printing proofs.

@author Wouter Beek
@version 2013/07-2013/09, 2013/11-2014/01, 2014/11
*/

:- use_module(library(dcg/basics)).
:- use_module(library(option)).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_bracket)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).

:- predicate_options(proof//2, 2, [
     pass_to(proofs//3, 3)
   ]).
:- predicate_options(proofs//3, 3, [
     pass_to(proposition//2, 2)
   ]).

:- meta_predicate(proof(:,+,?,?)).





%! proof(+Proof:compound, +Options:list(nvpair))// .

proof(Proof, Options) -->
  {option(indent(I), Options)},
  proofs(I, [Proof], Options).



%! proofs(
%!   +Indent:nonneg,
%!   +Predicates:list(compound),
%!   +Options:list(nvpair)
%! )// .

proofs(_, [], _) --> "".
proofs(I1, [H|T], Options) -->
  {H =.. [Rule,Premises,Conclusion]},

  % Indentation.
  indent(I1),

  % The name of the rule that was used for deduction.
  bracketed(square, rule(Rule, Options)),

  % Separator between rule name and conclusion.
  " ",

  % The conclusion.
  proposition(Conclusion, Options),
  line_feed,

  % Premises.
  {I2 is I1 + 1},
  proofs(I2, Premises, Options),

  % Print premises / subproofs.
  proofs(I1, T, Options).



%! proposition(+Proposition:compound, +Options:list(nvpair))// .

proposition(Proposition, _) -->
  {with_output_to(atom(Atom), writeq(Proposition))},
  atom(Atom).


%! rule(+Rule:atom, +Options:list(nvpair))// .

rule(Rule, _) -->
  atom(Rule).
